# =============================================================================
# ecaplots.R
# Standardized Love plots and Kaplan-Meier curves for the ECA Pilot
# -----------------------------------------------------------------------------
# This is a SOURCEABLE SCRIPT, not an installed package. Every data partner
# drops this one file next to their analysis and runs:
#
#     source("ecaplots.R")
#
# It only calls functions from packages that are on CRAN (cobalt, survival,
# ggplot2, ggtext, ragg, systemfonts, scales, rlang, cli, cards, showtext,
# sysfonts). Nothing here needs to be
# installed as a custom/non-CRAN package, so it does not trip site rules that
# forbid unreviewed packages. All external calls are namespaced (pkg::fun) so
# sourcing this file does NOT attach anything to your search path.
#
# Documentation blocks are written in roxygen2 style for readability and so the
# file could later be folded into a package if desired; the tags are inert when
# the file is merely sourced.
#
# See README.md for usage and input conventions, and examples/ for one worked
# scenario per supported input type.
# =============================================================================

# =============================================================================
# 0. Dependencies and version pinning
# =============================================================================

#' Package versions this script was written against
#'
#' Cross-partner reproducibility hinges on everyone rendering with the same
#' package versions. These are the shared pinned versions (R 4.4.3) every
#' partner should match. [eca_check_deps()] compares the locally installed
#' versions against these and warns on any mismatch. The recommended way to
#' guarantee a match is to install the pins with `setup/install_pinned.R`,
#' but the script will still run on close versions.
#'
#' @keywords internal
ECA_PINNED_VERSIONS <- c(
  cobalt = "4.5.5",
  survival = "3.8-3",
  ggplot2 = "3.5.1",
  ggtext = "0.1.2",
  ragg = "1.3.3",
  systemfonts = "1.1.0",
  scales = "1.3.0",
  rlang = "1.1.6",
  cli = "3.6.5",
  cards = "0.7.1",
  showtext = "0.9-7",
  sysfonts = "0.8.9"
)

# Packages that must be present for the *figure* code to run at all. These
# render every figure identically across partners, so the reproducibility gain
# is worth requiring them across the board. MatchThem, WeightIt, MatchIt, mice,
# optmatch, and haven, in contrast, are only needed by particular input paths
# (which one depends on how the partner ran the analysis) and are checked
# lazily where used -- see `.eca_survival_long()` and `.eca_run_loveplot()`.
ECA_REQUIRED_PKGS <- c(
  "cobalt",
  "survival",
  "ggplot2",
  "ggtext",
  "ragg",
  "systemfonts",
  "scales",
  "rlang",
  "cli",
  "cards",
  "showtext",
  "sysfonts"
)

#' Days per month used for the day -> month axis conversion
#'
#' 365.25 / 12 = 30.4375. Kept as a single constant so every partner converts
#' time identically.
#' @keywords internal
ECA_DAYS_PER_MONTH <- 30.4375

#' Check that required packages are installed and (optionally) version-matched
#'
#' @param strict If `TRUE`, a version mismatch is an error; otherwise a warning.
#'   A *missing* required package is always an error.
#' @return (invisibly) a data frame of package, required version, installed
#'   version, and whether they match.
#' @export
eca_check_deps <- function(strict = FALSE) {
  missing_pkgs <- ECA_REQUIRED_PKGS[
    !vapply(ECA_REQUIRED_PKGS, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_pkgs)) {
    # Base stop() here on purpose: cli itself may be the missing package.
    stop(
      ngettext(
        length(missing_pkgs),
        "Required package not installed: ",
        "Required packages not installed: "
      ),
      paste(missing_pkgs, collapse = ", "),
      ". See setup/install_pinned.R.",
      call. = FALSE
    )
  }

  installed <- vapply(
    names(ECA_PINNED_VERSIONS),
    function(p) {
      as.character(utils::packageVersion(p))
    },
    character(1)
  )

  out <- data.frame(
    package = names(ECA_PINNED_VERSIONS),
    required = unname(ECA_PINNED_VERSIONS),
    installed = unname(installed),
    stringsAsFactors = FALSE
  )
  # numeric_version normalizes "3.8-3" (CRAN DESCRIPTION style) and "3.8.3"
  # (what packageVersion() prints) to the same value.
  out$match <- vapply(
    seq_len(nrow(out)),
    function(i) {
      ok <- tryCatch(
        numeric_version(out$required[i]) == numeric_version(out$installed[i]),
        error = function(e) NA
      )
      isTRUE(ok)
    },
    logical(1)
  )

  if (any(!out$match)) {
    bad <- which(!out$match)
    bullets <- stats::setNames(
      sprintf(
        "%s: need %s, have %s",
        out$package[bad],
        out$required[bad],
        out$installed[bad]
      ),
      rep("*", length(bad))
    )
    msg <- c(
      "!" = "{.pkg ecaplots} was written against pinned package versions; these differ:",
      bullets,
      "i" = "Figures may differ subtly across partners; installing the pins with {.file setup/install_pinned.R} is the surest fix."
    )
    if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
  }
  invisible(out)
}


# =============================================================================
# 1. Fonts
# =============================================================================

# Package-scoped state for the resolved plotting font. Populated by
# `eca_setup_font()`; read by plot functions when their `base_family` default
# fires. Kept in an environment (not a global variable) so it survives across
# calls in a session but doesn't leak into the user's globalenv.
.eca_state <- new.env(parent = emptyenv())
.eca_state$font_family <- "sans"

#' Get the currently-resolved plotting font family
#'
#' Reads the family stored by [eca_setup_font()] (`"sans"` until it is called).
#' Plot functions use this as the lazy default for `base_family` so users don't
#' have to plumb the resolved name through by hand.
#' @return A single string.
#' @export
eca_font_family <- function() .eca_state$font_family

#' Register the plotting font, falling back gracefully
#'
#' The figures use Open Sans. If it is installed on the machine it is used
#' directly; otherwise its regular and bold faces are downloaded once from
#' Google Fonts (via `sysfonts::font_add_google()`), cached under
#' `tools::R_user_dir("ecaplots", "cache")`, and registered with `systemfonts`
#' (for ragg) and `sysfonts` (for showtext). Later sessions reuse the cache, so
#' only the first call needs internet access. If neither works, the first
#' available entry in `fallback` is used, with a warning.
#'
#' The resolved family is recorded via [eca_font_family()].
#'
#' @param family Desired font family. Default `"Open Sans"`.
#' @param fallback Ordered candidate substitutes if `family` is unavailable.
#' @param download If `TRUE` (default), fetch `family` from Google Fonts when it
#'   is neither installed nor cached.
#' @param register_showtext If `TRUE` (default) and `sysfonts` is installed,
#'   also register the resolved font with `sysfonts::font_add()` so `showtext`
#'   can render it.
#' @param quiet If `TRUE`, suppress the substitution warning.
#' @return (invisibly) the resolved family name. Plot functions default to
#'   [eca_font_family()], so users normally don't need to thread this through.
#' @export
eca_setup_font <- function(
  family = "Open Sans",
  fallback = c(
    "Arial",
    "Helvetica",
    "Liberation Sans",
    "DejaVu Sans",
    "sans"
  ),
  download = TRUE,
  register_showtext = TRUE,
  quiet = FALSE
) {
  faces <- .eca_system_font_faces(family)
  if (is.null(faces)) {
    faces <- .eca_cached_font_faces(family, download = download)
    if (!is.null(faces)) {
      # Registered under the font's own family name: aliasing a face under a
      # different name triggers a systemfonts 1.1.0 bug where spaces inside
      # bold text collapse to ~zero width.
      systemfonts::register_font(
        name = family,
        plain = faces[["regular"]],
        bold = faces[["bold"]]
      )
    }
  }

  resolved <- family
  if (is.null(faces)) {
    resolved <- "sans"
    for (cand in fallback) {
      faces <- .eca_system_font_faces(cand)
      if (!is.null(faces)) {
        resolved <- cand
        break
      }
    }
    if (!quiet) {
      cli::cli_warn(c(
        "!" = "Font {.val {family}} is not installed and could not be downloaded.",
        "i" = "Using {.val {resolved}} instead; plots pick it up via {.fn eca_font_family}.",
        "i" = "Install {family} on this machine, or rerun {.fn eca_setup_font} with internet access (see {.file README.md}, Fonts)."
      ))
    }
  }

  if (
    isTRUE(register_showtext) &&
      !is.null(faces) &&
      requireNamespace("sysfonts", quietly = TRUE)
  ) {
    try(
      sysfonts::font_add(
        family = resolved,
        regular = faces[["regular"]],
        bold = faces[["bold"]]
      ),
      silent = TRUE
    )
  }

  .eca_state$font_family <- resolved
  invisible(resolved)
}

#' Regular and bold face files of an installed family
#'
#' @return `c(regular = <path>, bold = <path>)`, or `NULL` if the family isn't
#'   installed. `bold` equals `regular` when no real bold face exists.
#' @keywords internal
.eca_system_font_faces <- function(family) {
  if (!family %in% systemfonts::system_fonts()$family) {
    return(NULL)
  }
  face <- function(weight) {
    p <- tryCatch(
      systemfonts::match_fonts(family, weight = weight)$path,
      error = function(e) NA_character_
    )
    if (length(p) == 0 || is.na(p[1]) || !nzchar(p[1])) NA_character_ else p[1]
  }
  regular <- face("normal")
  if (is.na(regular)) {
    return(NULL)
  }
  bold <- face("bold")
  c(regular = regular, bold = if (is.na(bold)) regular else bold)
}

#' Regular and bold face files of a Google font, from the user cache
#'
#' Downloads into the cache on first use. `sysfonts::font_add_google()` saves
#' each face to `tempdir()` under an opaque name, so the new files are
#' identified by diffing `tempdir()` and read with `systemfonts::font_info()`.
#' @return `c(regular = <path>, bold = <path>)`, or `NULL` on failure.
#' @keywords internal
.eca_cached_font_faces <- function(family, download = TRUE) {
  cache <- file.path(tools::R_user_dir("ecaplots", "cache"), "fonts")
  slug <- gsub("[^A-Za-z0-9]", "", family)
  cached <- file.path(cache, paste0(slug, c("-Regular.ttf", "-Bold.ttf")))
  names(cached) <- c("regular", "bold")
  if (all(file.exists(cached))) {
    return(cached)
  }
  if (!isTRUE(download) || !requireNamespace("sysfonts", quietly = TRUE)) {
    return(NULL)
  }

  before <- list.files(tempdir(), full.names = TRUE)
  ok <- tryCatch(
    {
      sysfonts::font_add_google(family, regular.wt = 400, bold.wt = 700)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!ok) {
    return(NULL)
  }
  new <- setdiff(list.files(tempdir(), full.names = TRUE), before)
  new <- new[grepl("\\.(ttf|otf)$", new, ignore.case = TRUE)]
  if (!length(new)) {
    return(NULL)
  }
  info <- systemfonts::font_info(path = new)
  pick <- function(bold) {
    i <- which(!info$italic & info$bold == bold)
    if (length(i)) new[i[1]] else NA_character_
  }
  downloaded <- c(regular = pick(FALSE), bold = pick(TRUE))
  if (is.na(downloaded[["regular"]])) {
    return(NULL)
  }
  if (is.na(downloaded[["bold"]])) {
    downloaded[["bold"]] <- downloaded[["regular"]]
  }

  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  if (all(file.copy(downloaded, cached, overwrite = TRUE))) {
    cached
  } else {
    downloaded
  }
}


# =============================================================================
# 2. Canonical covariate specification + provider mapping
# =============================================================================

#' The spec covariate order, labels, and types
#'
#' The Love plot must show exactly these ten covariates, in this order, with
#' these display labels, regardless of what else was in a partner's propensity
#' score model. `type` is `"continuous"` (a single row that carries a point) or
#' `"categorical"` (a bold header row with one indented level row per category,
#' each carrying a point). `binary_level`, where present, is a regex (perl,
#' case-insensitive) naming the level the spec shows when the covariate is
#' binary: Age <=65, Sex Male, ECOG 0 / Karnofsky 70-80. `last_levels`, where
#' present, fixes the level order: rows in the default order of
#' [.eca_sort_levels()] by display label, except those matching each regex
#' (case-insensitive), which go last in the order given. This overrides the order of the mapping's `levels`. The performance
#' status label is a fallback: the plot names the scale the partner used
#' (see [.eca_performance_label()]).
#'
#' @return An ordered list; each element has `key`, `label`, `type`, and
#'   optionally `binary_level`.
#' @export
eca_covariate_spec <- function() {
  list(
    list(
      key = "age",
      label = "Age",
      type = "continuous",
      binary_level = "^\\s*(\u2264|<=|<|under\\b)|^\\s*\\d+\\s*[-\u2013]\\s*\\d+\\s*$",
      binary_desc = "\u226465"
    ),
    list(
      key = "sex",
      label = "Sex",
      type = "categorical",
      binary_level = "^\\s*m(ale)?\\s*$",
      binary_desc = "Male"
    ),
    list(key = "ethnicity", label = "Ethnicity", type = "categorical"),
    list(
      key = "race",
      label = "Race",
      type = "categorical",
      last_levels = c("^\\s*other\\b", "unknown|missing")
    ),
    list(
      key = "time_dx_to_index",
      label = "Time from initial diagnosis to index date",
      type = "continuous"
    ),
    list(
      key = "time_stageiv_to_index",
      label = "Time from stage IV diagnosis to index date",
      type = "continuous"
    ),
    list(
      key = "met_sites",
      label = "Metastatic sites of disease",
      type = "categorical"
    ),
    list(key = "liver_met", label = "Liver metastases", type = "categorical"),
    list(
      key = "ecog_karnofsky",
      label = "Baseline ECOG / Karnofsky performance status",
      type = "categorical",
      binary_level = "^\\D*0(\\D|$)|70\\s*[-\u2013]\\s*80",
      binary_desc = "ECOG 0 / Karnofsky 70-80"
    ),
    list(
      key = "prior_cancer_treatment",
      label = "Prior cancer treatment",
      type = "categorical",
      last_levels = c("^\\s*none\\b", "not\\s+documented")
    )
  )
}

#' A blank covariate-mapping template for a partner to fill in
#'
#' Each partner maps the ten canonical concepts to the variable name(s) *they*
#' used in their own data / propensity score model. A mapping entry is either:
#'
#' * a single string — the partner's variable name; or
#' * a list with `variable` (string) and, optionally, `levels` — a named
#'   character vector `c("raw level" = "Display label", ...)` that both selects
#'   and orders the category rows and relabels them for display. For a binary
#'   variable, list only the level to show (e.g. `c(Yes = "<Modality>")`); or
#' * for a categorical concept captured as several yes/no variables, a list
#'   with `indicators` — a named character vector
#'   `c("Display label" = "variable", ...)`. Each indicator gets one row (its
#'   Yes / 1 / TRUE level), in the order given.
#'
#' Continuous concepts (age and the two times to index) may map to one
#' continuous variable, one categorical variable (binary or multi-level), or
#' one of each: `list(variable = c("age", "age_cat"), levels = ...)`. With
#' both, the continuous row is labelled `continuous_label` (default
#' `"Continuous"`). A numeric 0/1 variable is treated as binary; give it a
#' `levels` entry (e.g. `c("0" = "<=65", "1" = ">65")`) so its row has a
#' meaningful label.
#'
#' Binary Age, Sex and ECOG / Karnofsky always show the spec level (Age <=65,
#' Sex Male, ECOG 0, Karnofsky 70-80; see [eca_covariate_spec()]), whatever
#' `levels` lists; `levels` only relabels.
#'
#' The performance status header reads "Baseline ECOG performance status" or
#' "Baseline Karnofsky performance status", detected from the data. If the
#' scale can't be detected, set it with `scale = "ECOG"` or
#' `scale = "Karnofsky"` in the `ecog_karnofsky` list entry.
#'
#' Concepts a partner genuinely does not have can be left `NA` (they are dropped
#' with a warning), but note the spec asks for all ten: if a covariate was not in
#' the PS model, include it anyway via cobalt's additional-variable path so its
#' balance is still shown (see examples/scenario_matchthem.R).
#'
#' @param filled If `TRUE`, return an illustrative example mapping instead of a
#'   blank. Its variable names are placeholders (the canonical keys
#'   themselves); edit them to match your data.
#' @return A named list keyed by the canonical concept keys.
#' @export
eca_mapping_template <- function(filled = FALSE) {
  keys <- vapply(eca_covariate_spec(), `[[`, character(1), "key")
  if (!filled) {
    tmpl <- stats::setNames(vector("list", length(keys)), keys)
    tmpl[] <- list(NA_character_)
    return(tmpl)
  }
  # Illustrative mapping: placeholder variable names equal to the canonical
  # keys. Partners should copy this and edit to match their own data.
  as.list(stats::setNames(keys, keys))
}

#' Validate a partner-supplied covariate mapping
#'
#' Checks that all names are recognized canonical keys and reports which
#' concepts are unmapped (left `NA`). Stops on unknown keys; warns on gaps.
#'
#' @param mapping A named list, as produced by [eca_mapping_template()].
#' @return (invisibly) the normalized mapping (each entry a list with
#'   `variable`, `levels`, `indicators`; for indicator entries `variable` is
#'   the named vector of indicator variables).
#' @export
validate_eca_mapping <- function(mapping) {
  spec_keys <- vapply(eca_covariate_spec(), `[[`, character(1), "key")

  if (is.null(names(mapping)) || any(names(mapping) == "")) {
    cli::cli_abort(
      "{.arg mapping} must be a fully named list keyed by canonical concept."
    )
  }
  unknown <- setdiff(names(mapping), spec_keys)
  if (length(unknown)) {
    cli::cli_abort(c(
      "Unknown mapping key{?s}: {.val {unknown}}.",
      "i" = "Valid keys: {.val {spec_keys}}."
    ))
  }

  norm <- lapply(mapping, .eca_normalize_map_entry)
  spec_types <- stats::setNames(
    vapply(eca_covariate_spec(), `[[`, character(1), "type"),
    spec_keys
  )
  n_vars <- vapply(norm, function(e) length(e$variable), integer(1))
  too_many <- names(norm)[
    !vapply(norm, `[[`, logical(1), "indicators") &
      n_vars > ifelse(spec_types[names(norm)] == "continuous", 2L, 1L)
  ]
  if (length(too_many)) {
    cli::cli_abort(c(
      "Too many variables mapped to {.val {too_many}}.",
      "i" = "Continuous concepts (e.g. age) take one continuous and/or one categorical variable; categorical concepts take one variable, or {.code indicators}."
    ))
  }
  bad_ind <- names(norm)[
    vapply(norm, `[[`, logical(1), "indicators") &
      spec_types[names(norm)] != "categorical"
  ]
  if (length(bad_ind)) {
    cli::cli_abort(
      "{.code indicators} can only be used for categorical concepts, not {.val {bad_ind}}."
    )
  }

  mapped <- names(norm)[vapply(
    norm,
    function(e) !all(is.na(e$variable)),
    logical(1)
  )]
  unmapped <- setdiff(spec_keys, mapped)
  if (length(unmapped)) {
    cli::cli_warn(c(
      "!" = "These spec covariates are unmapped and will be omitted from the Love plot: {.val {unmapped}}.",
      "i" = "The spec expects all ten; map them (via cobalt's {.arg addl} path if they were not in the PS model) unless a partner truly lacks the data."
    ))
  }
  invisible(norm)
}

#' Normalize one mapping entry to list(variable=, levels=, indicators=)
#' @keywords internal
.eca_normalize_map_entry <- function(entry) {
  if (is.null(entry) || (length(entry) == 1 && is.na(entry))) {
    return(list(variable = NA_character_, levels = NULL, indicators = FALSE))
  }
  if (is.character(entry)) {
    return(list(variable = unname(entry), levels = NULL, indicators = FALSE))
  }
  if (is.list(entry) && !is.null(entry$indicators)) {
    ind <- entry$indicators
    if (
      !is.character(ind) ||
        is.null(names(ind)) ||
        any(!nzchar(names(ind))) ||
        anyNA(ind) ||
        !is.null(entry$variable) ||
        !is.null(entry$levels)
    ) {
      cli::cli_abort(c(
        "{.code indicators} must be a fully named character vector: {.code c(\"Display label\" = \"variable\", ...)}.",
        "i" = "Don't combine it with {.code variable} or {.code levels}."
      ))
    }
    return(list(variable = ind, levels = NULL, indicators = TRUE))
  }
  if (is.list(entry)) {
    return(list(
      variable = entry$variable,
      levels = entry$levels,
      indicators = FALSE,
      continuous_label = entry$continuous_label,
      scale = entry$scale
    ))
  }
  cli::cli_abort(
    "Each mapping entry must be {.code NA}, a string, {.code list(variable=, levels=)}, or {.code list(indicators=)}."
  )
}

#' All variable names in a normalized mapping
#' @keywords internal
.eca_mapped_vars <- function(norm_map) {
  unique(stats::na.omit(unname(unlist(lapply(norm_map, `[[`, "variable")))))
}


# =============================================================================
# 3. Shared theme
# =============================================================================

#' The shared ggplot theme for ECA figures
#'
#' `theme_minimal` base, then: no background grid lines of any kind, visible x
#' and y axis lines only, no minor ticks styling beyond defaults, all text in
#' `base_family` at `base_size`.
#'
#' @param base_family Font family (already registered via [eca_setup_font()]).
#' @param base_size Base font size in points. Default 12.
#' @return A ggplot2 theme object.
#' @export
eca_theme <- function(base_family = eca_font_family(), base_size = 12) {
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.ticks = ggplot2::element_line(colour = "black"),
      text = ggplot2::element_text(family = base_family, size = base_size),
      axis.text = ggplot2::element_text(
        family = base_family,
        size = base_size,
        colour = "black"
      ),
      axis.title = ggplot2::element_text(
        family = base_family,
        size = base_size
      ),
      plot.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 20, b = 10, l = 10, unit = "pt")
    )
}


# =============================================================================
# 4. Balance: converters + Love plot
# =============================================================================

#' Convert a partner's object into a canonical balance object
#'
#' Produces an `eca_balance` object by having `cobalt::love.plot()` build a base
#' balance ggplot from standardized, absolute mean differences, which
#' [eca_love_plot()] then reshapes to the ten canonical spec covariates. All
#' input paths funnel through `cobalt::love.plot()` so the standardization is
#' identical across partners regardless of how they matched/weighted.
#'
#' Supported `x`:
#' * `mimids` / `wimids` (MatchThem) — per-imputation balance shown as the
#'   mean point with a min-max range across imputations (`agg.fun = "range"`).
#' * `matchit` (MatchIt) / `weightit` (WeightIt) — single, non-imputed fits.
#' * `data.frame` — a partner-prepared data frame. Requires `treat`, `covs`
#'   (the columns to assess), `weights` (a column name or vector; omit for the
#'   unadjusted-only case), and, for multiply-imputed data stacked long, `imp`
#'   (the imputation-id column) so cobalt pools across imputations.
#'
#' @param x Input object (see above).
#' @param mapping Partner covariate mapping (see [eca_mapping_template()]).
#' @param treat For the data.frame path: name of the 0/1 treatment column
#'   (1 = focal/trial control group, matching the ATT target).
#' @param covs For the data.frame path: character vector of covariate columns to
#'   assess, or a formula (a one-sided formula gets `treat` as its response).
#'   If `NULL`, the mapped variables are used.
#' @param weights For the data.frame path: adjusted weights, as a column name or
#'   numeric vector. `NULL` gives an unadjusted-only plot.
#' @param imp For the data.frame path: name of the imputation-id column when the
#'   data are stacked multiply-imputed rows; `NULL` for single/pre-pooled data.
#' @param s_d_denom Standardization denominator passed to cobalt. Default
#'   `"treated"` (appropriate for an ATT estimand). Always passed explicitly,
#'   overriding what cobalt would infer from a matchit/weightit object.
#' @param binary How cobalt summarizes binary covariates: `"std"` (default,
#'   standardized mean differences, as the spec expects) or `"raw"` (raw
#'   differences in proportions). The x-axis label is the spec's either way.
#' @param colors Length-2 unnamed vector for the unadjusted then adjusted point
#'   series. Defaults to the spec colors (`#E87D72`, `#56BCC2`).
#' @param missing_values Values that, like `NA`, mean "not recorded". A mapped
#'   covariate that is entirely `NA`/`missing_values` in either arm is dropped
#'   from the Love plot with a warning, even if passed via `addl`.
#' @param ... Passed through to `cobalt::love.plot()` (and onward to
#'   `cobalt::bal.tab()`), e.g. `addl = ~ var1 + var2` to assess
#'   balance on spec covariates that were not in the PS model.
#' @return An `eca_balance` object: a list with `plot` (the base `cobalt`
#'   love.plot ggplot), `mapping` (the normalized covariate mapping, with
#'   dropped covariates set to `NA`), `levels_observed` (levels of each mapped
#'   categorical found in the data), `missing_in_arm` (dropped covariates
#'   and the arm(s) where they are completely missing), `binary` and
#'   `source` (`"cobalt"`; see [as_eca_precomputed_balance()]).
#'   Categorical rows are ordered by the mapping's `levels` entry if given,
#'   else by factor level order, else numeric levels in numeric order, then
#'   other levels alphabetically, then unknown/missing levels.
#' @export
as_eca_balance <- function(
  x,
  mapping,
  treat = NULL,
  covs = NULL,
  weights = NULL,
  imp = NULL,
  s_d_denom = "treated",
  binary = c("std", "raw"),
  colors = c("#E87D72", "#56BCC2"),
  missing_values = c(
    "Unknown",
    "Missing",
    "Unknown/Missing",
    "Not documented",
    "Not reported"
  ),
  ...
) {
  binary <- match.arg(binary)
  norm_map <- validate_eca_mapping(mapping)
  missing_in_arm <- .eca_missing_in_arm(x, norm_map, treat, missing_values)
  if (length(missing_in_arm)) {
    desc <- paste0(
      names(missing_in_arm),
      " (",
      attr(missing_in_arm, "treat") %||% "treatment",
      " = ",
      vapply(missing_in_arm, paste, character(1), collapse = ", "),
      ")"
    )
    cli::cli_warn(c(
      "!" = "Spec covariate{?s} completely missing in one arm {?is/are} dropped from the Love plot: {.val {desc}}.",
      "i" = "Missing means every value is {.code NA} or one of {.val {missing_values}}; a balance statistic would compare observed data to nothing.",
      "i" = "Leave such covariates out of the PS model and out of {.arg addl}, or map them to {.code NA} to silence this warning."
    ))
    for (k in names(norm_map)) {
      v <- norm_map[[k]]$variable
      v <- v[!v %in% names(missing_in_arm)]
      norm_map[[k]]$variable <- if (length(v)) v else NA_character_
    }
  }
  lp <- .eca_run_loveplot(
    x,
    treat = treat,
    covs = covs,
    weights = weights,
    imp = imp,
    norm_map = norm_map,
    s_d_denom = s_d_denom,
    binary = binary,
    colors = colors,
    ...
  )
  structure(
    list(
      plot = lp,
      mapping = norm_map,
      levels_observed = .eca_observed_levels(x, norm_map),
      missing_in_arm = missing_in_arm,
      binary = binary,
      source = "cobalt"
    ),
    class = "eca_balance"
  )
}

#' Build a canonical balance object from precomputed (e.g. SAS) SMDs
#'
#' For partners whose balance statistics were computed outside R, typically in
#' SAS. The SMDs are plotted **as supplied**: this function does not
#' recompute or check their standardization, so they may differ from the
#' cobalt-computed values of [as_eca_balance()]. Absolute values are taken,
#' and with an `imp` column each row shows the mean across imputations plus
#' the min-max range, as for the other routes. The result goes to
#' [eca_love_plot()] like any other `eca_balance` object.
#'
#' `df` has one row per variable (and level) per imputation, with columns:
#' * `variable` -- the variable name, as used in `mapping`;
#' * `level` -- the category level for categorical rows; `NA` or `""` for
#'   continuous rows, and for 0/1 variables mapped as `indicators`;
#' * `unadjusted`, `adjusted` -- the SMDs before and after matching/weighting;
#' * `imp` (optional) -- the imputation id.
#'
#' A binary variable may give both levels or only one. With both, the row for
#' the level the Love plot names (Age <=65, Sex Male, ECOG 0, otherwise the
#' level chosen as for [as_eca_balance()]) is plotted. With only one, list
#' both levels in the mapping's `levels` entry so the variable is recognised
#' as binary and the spec level can be named; otherwise the level is shown
#' as given (with a warning if it isn't the spec level). A categorical
#' version of age or a time to index (e.g. 0/1 age) must give a `level` on
#' each row. Level rows are ordered as for [as_eca_balance()]: the mapping's
#' `levels`, else factor level order of `df$level`, else sorted. Rows for
#' unmapped variables are not plotted.
#'
#' Covariates completely missing in one arm can't be detected from SMDs:
#' leave them out of `df`.
#'
#' @param df A data frame as described above.
#' @param mapping Partner covariate mapping (see [eca_mapping_template()]).
#' @param colors Length-2 unnamed vector for the unadjusted then adjusted point
#'   series. Defaults to the spec colors (`#E87D72`, `#56BCC2`).
#' @return An `eca_balance` object (see [as_eca_balance()]) with
#'   `source = "precomputed"` and `binary = NA`.
#' @export
as_eca_precomputed_balance <- function(
  df,
  mapping,
  colors = c("#E87D72", "#56BCC2")
) {
  norm_map <- validate_eca_mapping(mapping)
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame, not {.cls {class(df)}}.")
  }
  df <- as.data.frame(df)
  need <- c("variable", "level", "unadjusted", "adjusted")
  missing_cols <- setdiff(need, names(df))
  if (length(missing_cols)) {
    cli::cli_abort(c(
      "{.arg df} is missing column{?s} {.val {missing_cols}}.",
      "i" = "Required: {.val {need}}; optional: {.val imp}. R column names are case-sensitive."
    ))
  }

  variable <- trimws(as.character(df$variable))
  if (anyNA(variable) || any(!nzchar(variable))) {
    cli::cli_abort("{.code variable} must not be missing or blank.")
  }
  level <- trimws(as.character(df$level))
  level[!is.na(level) & !nzchar(level)] <- NA
  for (col in c("unadjusted", "adjusted")) {
    if (!is.numeric(df[[col]]) || anyNA(df[[col]])) {
      cli::cli_abort(c(
        "{.code {col}} must be numeric with no missing values.",
        "i" = "SAS missing numbers read from CSV as {.val .}: use {.code read.csv(na.strings = c(\"\", \".\"))} and check which rows are missing."
      ))
    }
  }
  imp <- if ("imp" %in% names(df)) df$imp else rep(1L, nrow(df))
  if (anyNA(imp)) {
    cli::cli_abort("{.code imp} must not be missing.")
  }

  mixed <- unique(variable[
    variable %in% variable[is.na(level)] & variable %in% variable[!is.na(level)]
  ])
  if (length(mixed)) {
    cli::cli_abort(c(
      "Variable{?s} {.val {mixed}} {?has/have} rows both with and without a {.code level}.",
      "i" = "Continuous rows have no level; each categorical row names its level."
    ))
  }
  key <- ifelse(is.na(level), variable, paste0(variable, "_", level))
  dup <- duplicated(data.frame(variable, level, imp))
  if (any(dup)) {
    cli::cli_abort(
      "Duplicate rows for {.val {unique(key[dup])}}: each variable/level appears once per imputation."
    )
  }
  pairs <- unique(data.frame(variable, level, key))
  clash <- unique(pairs$key[duplicated(pairs$key)])
  if (length(clash)) {
    who <- pairs[pairs$key %in% clash, ]
    desc <- paste0(who$variable, " / ", ifelse(is.na(who$level), "(no level)", who$level))
    cli::cli_abort(c(
      "Variable/level pairs {.val {desc}} both give the row name {.val {clash}}.",
      "i" = "Rows are named {.code <variable>_<level>}; rename a variable or level so they differ."
    ))
  }
  n_imp <- length(unique(imp))
  counts <- table(key)
  incomplete <- names(counts)[counts != n_imp]
  if (length(incomplete)) {
    cli::cli_abort(c(
      "Row{?s} {.val {incomplete}} {?is/are} not present in every imputation.",
      "i" = "{.arg df} has {n_imp} imputation{?s}; each variable/level needs a row in each."
    ))
  }

  # The layout needs each categorical variable's levels, ordered as for
  # as_eca_balance(): factor level order, else sorted.
  vars <- unique(variable)
  levels_observed <- list()
  for (v in vars[vars %in% variable[!is.na(level)]]) {
    present <- unique(level[variable == v & !is.na(level)])
    levels_observed[[v]] <- if (is.factor(df$level)) {
      intersect(trimws(levels(df$level)), present)
    } else {
      .eca_sort_levels(present)
    }
  }
  attr(levels_observed, "located") <- vars

  spec <- eca_covariate_spec()
  keep <- rep(TRUE, nrow(df))
  disagree <- character(0)
  lone <- character(0)
  for (entry in spec) {
    m <- norm_map[[entry$key]]
    if (is.null(m) || all(is.na(m$variable))) {
      next
    }
    if (!isTRUE(m$indicators) && !identical(entry$type, "continuous")) {
      bare <- intersect(m$variable, variable[is.na(level)])
      if (length(bare)) {
        cli::cli_abort(c(
          "Categorical covariate {.val {bare}} has rows with no {.code level}.",
          "i" = "Give each row its level (e.g. {.val 1} or {.val Male}), or map yes/no variables with {.code indicators}."
        ))
      }
    }
    level_map <- if (!isTRUE(m$indicators)) m$levels
    if (
      identical(entry$type, "continuous") &&
        !is.null(level_map) &&
        all(m$variable %in% variable) &&
        !any(m$variable %in% names(levels_observed))
    ) {
      cli::cli_abort(c(
        "{.val {entry$key}} has a {.code levels} entry, but {.val {intersect(m$variable, variable)}} {?has/have} no rows with a {.code level}.",
        "i" = "For a categorical (e.g. 0/1) version, give each row its level (e.g. {.val 0} and {.val 1})."
      ))
    }
    for (v in intersect(m$variable, names(levels_observed))) {
      known <- levels_observed[[v]]
      # One level reported: the mapping's two levels make it a binary.
      if (
        length(known) == 1L &&
          length(level_map) == 2L &&
          known %in% names(level_map)
      ) {
        levels_observed[[v]] <- names(level_map)
        next
      }
      if (length(known) == 1L && !is.null(entry$binary_level)) {
        disp <- if (known %in% names(level_map)) level_map[[known]] else known
        if (
          !grepl(entry$binary_level, known, perl = TRUE, ignore.case = TRUE) &&
            !grepl(entry$binary_level, disp, perl = TRUE, ignore.case = TRUE)
        ) {
          lone <- c(lone, paste0(v, ": ", disp, " (spec: ", entry$binary_desc, ")"))
        }
      }
      # A binary has two levels, and a `levels` entry naming more means some
      # levels are missing from `df`, not that the variable is binary.
      if (length(known) != 2L || length(level_map) > 2L) {
        next
      }
      # The level cobalt would report: the last factor level, which for
      # character data is the last in sort() order.
      reported <- if (is.factor(df$level)) known[[2]] else sort(known)[[2]]
      shown <- .eca_binary_level(known, level_map, entry$binary_level, reported)
      if (is.na(shown)) {
        shown <- .eca_binary_level(known, level_map, reported = reported)
      }
      rows <- variable == v
      a <- rows & level %in% shown
      b <- rows & !level %in% shown
      ord <- function(i) order(imp[i])
      ia <- which(a)[ord(a)]
      ib <- which(b)[ord(b)]
      gap <- max(
        abs(abs(df$unadjusted[ia]) - abs(df$unadjusted[ib])),
        abs(abs(df$adjusted[ia]) - abs(df$adjusted[ib]))
      )
      if (gap > 1e-6) {
        disagree <- c(disagree, paste0(v, ": ", shown))
      }
      keep[b] <- FALSE
    }
  }
  if (length(lone)) {
    cli::cli_warn(c(
      "!" = "Only one level was supplied for binary covariate{?s} {.val {lone}}, and it isn't the level the spec names; it is shown as supplied.",
      "i" = "Add both levels to the mapping's {.code levels} entry (e.g. {.code levels = c(Female = \"Female\", Male = \"Male\")}) so the spec level is named; a binary's two levels have the same |SMD|."
    ))
  }
  if (length(disagree)) {
    cli::cli_inform(c(
      "i" = "The two levels of binary covariate{?s} {.val {disagree}} have different |SMD|s; the named level's value is plotted."
    ))
  }

  long <- data.frame(
    var = rep(key[keep], 2L),
    Sample = factor(
      rep(c("Unadjusted", "Adjusted"), each = sum(keep)),
      levels = c("Unadjusted", "Adjusted")
    ),
    value = abs(c(df$unadjusted[keep], df$adjusted[keep]))
  )
  grp <- interaction(long$var, long$Sample, drop = TRUE, sep = "\u001f")
  first <- !duplicated(grp)
  dat <- long[first, c("var", "Sample")]
  dat$min.stat <- as.vector(tapply(long$value, grp, min)[as.character(grp[first])])
  dat$max.stat <- as.vector(tapply(long$value, grp, max)[as.character(grp[first])])
  dat$mean.stat <- as.vector(tapply(long$value, grp, mean)[as.character(grp[first])])
  dat$stat <- dat$mean.stat
  dat$var <- factor(dat$var, levels = unique(key[keep]))
  rownames(dat) <- NULL
  if (n_imp == 1L) {
    dat <- dat[c("var", "Sample", "stat")]
  }

  # Same geoms and settings as cobalt::love.plot(), so the reshaped plot
  # matches the other routes.
  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(x = .data$stat, y = .data$var, group = .data$Sample)
  ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 1, colour = "gray5")
  if (n_imp > 1L) {
    dodge <- ggplot2::position_dodge(0.5)
    p <- p +
      ggplot2::geom_linerange(
        ggplot2::aes(
          xmin = .data$min.stat,
          xmax = .data$max.stat,
          colour = .data$Sample
        ),
        linewidth = 0.8,
        orientation = "y",
        position = dodge,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = .data$mean.stat, colour = .data$Sample),
        shape = "circle",
        fill = "white",
        size = 3,
        stroke = 0,
        position = dodge,
        na.rm = TRUE
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(colour = .data$Sample),
        shape = "circle",
        fill = "white",
        size = 3,
        stroke = 0,
        na.rm = TRUE
      )
  }
  p <- p +
    ggplot2::scale_colour_manual(
      values = stats::setNames(colors[1:2], levels(dat$Sample))
    )

  structure(
    list(
      plot = p,
      mapping = norm_map,
      levels_observed = levels_observed,
      missing_in_arm = list(),
      binary = NA_character_,
      source = "precomputed"
    ),
    class = "eca_balance"
  )
}

#' Mapped covariates with no usable data in at least one arm
#'
#' A covariate is "completely missing" in an arm when every value there is
#' `NA` or one of `missing_values` (compared case-insensitively after
#' trimming). For MatchThem
#' objects the pre-imputation data are checked. Covariates whose data can't be
#' located (e.g. matchit `addl` variables absent from `x$X`) are not checked.
#' @return A list keyed by variable name, each the arm value(s) with no
#'   data; attribute `treat` holds the treatment column name.
#' @keywords internal
.eca_missing_in_arm <- function(x, norm_map, treat, missing_values) {
  cls <- class(x)
  if (any(c("mimids", "wimids") %in% cls)) {
    dat <- x$object$data
    tr_name <- all.vars(x$models[[1]]$formula)[1]
    tr <- if (!is.null(dat) && tr_name %in% names(dat)) dat[[tr_name]]
  } else if (any(c("matchit", "weightit") %in% cls)) {
    dat <- if ("matchit" %in% cls) x$X else x$covs
    tr <- x$treat
    tr_name <- all.vars(x$formula)[1]
  } else if (is.data.frame(x) && !is.null(treat) && treat %in% names(x)) {
    dat <- x
    tr <- x[[treat]]
    tr_name <- treat
  } else {
    return(list())
  }
  if (is.null(dat) || is.null(tr) || length(tr) != nrow(dat)) {
    return(list())
  }

  tokens <- tolower(trimws(missing_values))
  arms <- sort(unique(tr[!is.na(tr)]))
  out <- list()
  for (var in intersect(.eca_mapped_vars(norm_map), names(dat))) {
    col <- dat[[var]]
    absent <- is.na(col) | tolower(trimws(as.character(col))) %in% tokens
    empty <- arms[vapply(
      arms,
      function(a) all(absent[!is.na(tr) & tr == a]),
      logical(1)
    )]
    if (length(empty)) out[[var]] <- as.character(empty)
  }
  attr(out, "treat") <- tr_name
  out
}

#' Observed levels of each mapped categorical covariate
#'
#' Used by [.eca_love_layout()] to claim only genuine `variable_level` rows from
#' cobalt's output, so an unmapped PS covariate whose name starts with a mapped
#' variable's name (e.g. `race_eth` vs `race`) can't masquerade as a level.
#' Factor columns keep their level order; others are sorted. For continuous
#' concepts (e.g. age), only categorical-looking variables are recorded:
#' factor, character, logical, or numeric with at most two distinct values
#' (as cobalt treats them as binary).
#' @return A list keyed by variable name; attribute `located` lists the mapped
#'   variables found in `x`'s data.
#' @keywords internal
.eca_observed_levels <- function(x, norm_map) {
  cls <- class(x)
  dat <- if (any(c("mimids", "wimids") %in% cls)) {
    tryCatch(MatchThem::complete(x, action = 1), error = function(e) NULL)
  } else if ("matchit" %in% cls) {
    x$X
  } else if ("weightit" %in% cls) {
    x$covs
  } else if (is.data.frame(x)) {
    x
  }

  out <- list()
  if (is.null(dat)) {
    return(out)
  }
  types <- stats::setNames(
    vapply(eca_covariate_spec(), `[[`, character(1), "type"),
    vapply(eca_covariate_spec(), `[[`, character(1), "key")
  )
  located <- character(0)
  for (key in intersect(names(types), names(norm_map))) {
    for (var in intersect(.eca_mapped_vars(norm_map[key]), names(dat))) {
      located <- c(located, var)
      col <- dat[[var]]
      present <- unique(as.character(col[!is.na(col)]))
      if (
        types[[key]] == "continuous" && is.numeric(col) && length(present) > 2L
      ) {
        next
      }
      out[[var]] <- if (is.factor(col)) {
        intersect(levels(col), present)
      } else if (is.numeric(col)) {
        as.character(sort(unique(col[!is.na(col)])))
      } else {
        .eca_sort_levels(present)
      }
    }
  }
  attr(out, "located") <- located
  out
}

#' Dispatch the correct cobalt::love.plot() call for the input type
#'
#' We deliberately let `cobalt::love.plot()` build the base ggplot: it already
#' standardizes the balance statistics, drops the distance row, colors the
#' unadjusted/adjusted series, and (for multiply imputed inputs) aggregates
#' across imputations. [eca_love_plot()] then reshapes that ggplot to the spec.
#' This avoids depending on the exact column names inside a `bal.tab` object.
#' @keywords internal
.eca_run_loveplot <- function(
  x,
  treat,
  covs,
  weights,
  imp,
  norm_map,
  s_d_denom,
  binary,
  colors,
  ...
) {
  base_args <- list(
    stats = "mean.diffs",
    binary = binary,
    continuous = "std",
    abs = TRUE,
    un = TRUE,
    s.d.denom = s_d_denom,
    drop.distance = TRUE,
    line = FALSE,
    thresholds = NULL, # we draw our own dashed line at the threshold
    colors = colors,
    shapes = c("circle", "circle")
  )

  cls <- class(x)
  if (any(c("mimids", "wimids") %in% cls)) {
    # For mimids/wimids, cobalt's default agg.fun collapses across imputations
    # to a single point per Sample; force "range" so we get the dot + range
    # rendering that matches the data.frame + imp branch.
    return(do.call(
      .eca_cobalt_love_plot,
      c(list(x, agg.fun = "range"), base_args, list(...))
    ))
  }
  if (any(c("matchit", "weightit") %in% cls)) {
    return(do.call(.eca_cobalt_love_plot, c(list(x), base_args, list(...))))
  }
  if (is.data.frame(x)) {
    if (is.null(treat)) {
      cli::cli_abort(c(
        "A {.cls data.frame} input needs a treatment column.",
        "i" = "Pass {.arg treat} (the 0/1 group column; 1 = trial control)."
      ))
    }
    if (is.null(covs)) {
      covs <- .eca_mapped_vars(norm_map)
    }
    is_formula <- inherits(covs, "formula")
    if (is_formula && length(covs) == 2L) {
      covs <- stats::reformulate(
        attr(stats::terms(covs), "term.labels"),
        response = treat
      )
    }
    covs_obj <- if (is_formula) covs else x[, covs, drop = FALSE]

    args <- c(list(covs_obj), base_args)
    if (is_formula) {
      args$data <- x
    } else {
      args$treat <- x[[treat]]
    }
    if (!is.null(weights)) {
      args$weights <- if (is.character(weights) && length(weights) == 1L) {
        x[[weights]]
      } else {
        weights
      }
    }
    if (!is.null(imp)) {
      args$imp <- x[[imp]]
      # Force range aggregation to match the mimids/wimids branch.
      args$agg.fun <- "range"
    }
    return(do.call(.eca_cobalt_love_plot, c(args, list(...))))
  }
  cli::cli_abort(c(
    "Can't build a balance plot from an object of class {.cls {cls}}.",
    "i" = paste(
      "Supply a {.cls mimids}/{.cls wimids}/{.cls matchit}/",
      "{.cls weightit} object, or a {.cls data.frame} with",
      "{.arg treat}/{.arg weights}."
    )
  ))
}

#' `cobalt::love.plot()`, minus its "large mean differences" warning
#'
#' cobalt warns when any row's |mean difference| exceeds 5, suggesting
#' unstandardized continuous variables. Our calls always standardize, and the
#' row is often one we drop anyway (e.g. a transformed or interaction PS term), so the
#' hint misleads. [eca_love_plot()] warns instead if a plotted row exceeds 5.
#' With `binary = "raw"`, cobalt's "standardized and raw mean differences ...
#' in the same plot" warning is muffled too: the spec fixes the axis label.
#' @keywords internal
.eca_cobalt_love_plot <- function(...) {
  withCallingHandlers(
    cobalt::love.plot(...),
    warning = function(w) {
      if (
        # cobalt wraps messages at the console width; collapse line breaks.
        grepl(
          "large mean differences detected|raw mean differences are present in the same",
          gsub("\\s+", " ", conditionMessage(w)),
          ignore.case = TRUE
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' Read the column name behind a plot aesthetic, with a fallback
#'
#' `cobalt::love.plot()` stores its mapping as `aes(x = .data$stat, y = .data$var)`
#' -- compound `.data[[col]]` expressions rather than bare symbols. `rlang::as_name`
#' errors on those; `rlang::as_label` returns the bare column name for both
#' `.data$col` and `.data[["col"]]` forms, which is what we want. If the resolved
#' name isn't a column of the plot's data, we fall back.
#' @keywords internal
.eca_aes_col <- function(p, aes, fallback = NA_character_) {
  m <- p$mapping[[aes]]
  if (is.null(m)) {
    return(fallback)
  }
  nm <- tryCatch(rlang::as_label(m), error = function(e) NA_character_)
  if (is.na(nm) || !nm %in% names(p$data)) fallback else nm
}

#' Compute the spec y-axis layout: which rows to keep, their order, and labels
#'
#' Given the covariate identifiers `cobalt` used on the y axis, assign them to
#' the ten canonical concepts and build the top-to-bottom display: a bold
#' concept header then level rows for categoricals; a single plain row for
#' continuous covariates. Concept headers get a synthetic key that is NOT
#' present in the data, so they render as empty ticks carrying only a bold label.
#' Level rows are ordered by the `levels` mapping entry if given, else by the
#' data (factor level order, or sorted).
#'
#' @param vars Character vector of the covariate identifiers on the love.plot
#'   y-axis (variable names for continuous, `variable_level` for categorical).
#' @param norm_map Normalized covariate mapping.
#' @param observed_levels Output of [.eca_observed_levels()].
#' @return list(`keep` = data rows to retain, `order` = y keys top->bottom,
#'   `labels` = named vector of markdown labels).
#' @keywords internal
.eca_love_layout <- function(vars, norm_map, observed_levels = list()) {
  spec <- eca_covariate_spec()
  # `level_pad` is applied AFTER a categorical level label. With `hjust = 1`
  # on the y-axis, trailing whitespace pushes the level's text left of the
  # axis, visually offsetting it from bold headers and continuous rows.
  level_pad <- "&nbsp;&nbsp;&nbsp;&nbsp;"
  # Escape `<` / `>` in label text: gridtext parses a leading `>` as a
  # `<blockquote>` (which it doesn't support and errors on) -- see level
  # names like `>2` for "Metastatic sites of disease".
  esc_html <- function(s) {
    s <- gsub(">", "&gt;", s, fixed = TRUE)
    gsub("<", "&lt;", s, fixed = TRUE)
  }

  keep <- character(0) # data `var` values to retain (level/continuous rows)
  order <- character(0) # y keys, top -> bottom
  labels <- character(0) # named by y key -> markdown label
  claimed <- character(0)
  no_rows <- character(0) # mapped concepts cobalt produced no rows for
  guessed <- character(0) # categoricals whose levels had to be inferred
  dropped_levels <- character(0) # `levels` entries with no cobalt row
  unmatched <- character(0) # binaries with no level matching the spec rule
  unlabelled <- character(0) # binary level shown but absent from `levels`

  located <- attr(observed_levels, "located")

  # Rows for one categorical variable: named character, row key -> display
  # text (unescaped). Updates `guessed` / `dropped_levels`.
  all_vars <- .eca_mapped_vars(norm_map)
  cat_rows <- function(
    var,
    key,
    label,
    level_map,
    prefer = NULL,
    prefer_desc = NULL,
    last_levels = NULL
  ) {
    prefix <- paste0(var, "_")
    known <- observed_levels[[var]]
    avail <- setdiff(vars, claimed)
    if (!is.null(known)) {
      # Claim only rows for real levels (in the data's level order).
      raw_levels <- known[paste0(prefix, known) %in% avail]
    } else {
      raw_levels <- .eca_sort_levels(sub(
        paste0("^", .eca_escape(prefix)),
        "",
        .eca_prefix_rows(var, avail, all_vars)
      ))
    }
    exact <- if (var %in% avail) var else character(0)
    if (!length(raw_levels) && !length(exact)) {
      return(character(0))
    }
    if (is.null(known) && is.null(level_map)) {
      guessed <<- c(guessed, key)
    }

    # Binary: cobalt draws one row, either `var_<last level>` or (for 0/1,
    # logical) the bare `var`. |SMD| is identical for both levels, so label
    # that row with the level chosen by .eca_binary_level().
    if (length(known) == 2L && length(raw_levels) + length(exact) == 1L) {
      row <- if (length(exact)) var else paste0(prefix, raw_levels)
      reported <- if (!length(exact)) raw_levels
      shown <- .eca_binary_level(known, level_map, prefer, reported)
      if (!is.null(prefer) && is.na(shown)) {
        unmatched <<- c(unmatched, paste0(label, " (", prefer_desc, ")"))
        shown <- .eca_binary_level(known, level_map, reported = reported)
      }
      if (!is.null(level_map) && !shown %in% names(level_map)) {
        unlabelled <<- c(unlabelled, paste0(label, ": ", shown))
      }
      disp <- if (shown %in% names(level_map)) {
        unname(level_map[[shown]])
      } else {
        shown
      }
      return(stats::setNames(disp, row))
    }

    if (!is.null(level_map)) {
      # Restrict/order to provider-specified levels present in the data.
      missing_lv <- setdiff(names(level_map), raw_levels)
      if (length(missing_lv)) {
        dropped_levels <<- c(dropped_levels, paste0(label, ": ", missing_lv))
      }
      raw_levels <- names(level_map)[names(level_map) %in% raw_levels]
    }
    disp <- if (!is.null(level_map)) {
      unname(level_map[raw_levels])
    } else {
      raw_levels
    }
    out <- stats::setNames(as.character(disp), paste0(prefix, raw_levels))
    if (length(exact)) {
      out[exact] <- label # rare: binary with no suffix and data not located
    }
    if (length(last_levels)) {
      tier <- rep(0L, length(out))
      for (i in rev(seq_along(last_levels))) {
        tier[grepl(last_levels[[i]], out, ignore.case = TRUE, perl = TRUE)] <- i
      }
      out <- out[order(tier, match(out, .eca_sort_levels(out)))]
    }
    out
  }

  # Bold header + indented rows (named: row key -> display text).
  add_block <- function(key, label, rows) {
    hdr <- paste0(".__hdr__.", key)
    claimed <<- c(claimed, names(rows))
    keep <<- c(keep, names(rows))
    order <<- c(order, hdr, names(rows))
    labels[hdr] <<- paste0("**", label, "**")
    labels[names(rows)] <<- paste0(esc_html(rows), level_pad)
  }

  for (entry in spec) {
    key <- entry$key
    var <- norm_map[[key]]$variable
    if (is.null(var) || all(is.na(var))) {
      next
    }
    level_map <- norm_map[[key]]$levels # optional named vector
    if (
      identical(key, "ecog_karnofsky") && !isTRUE(norm_map[[key]]$indicators)
    ) {
      entry$label <- .eca_performance_label(
        var,
        observed_levels[[var]],
        level_map,
        .eca_prefix_rows(var, vars, all_vars),
        norm_map[[key]]$scale
      )
    }

    if (identical(entry$type, "continuous")) {
      # Each mapped variable is continuous or categorical (binary or more
      # levels), judged from the data; if the data weren't located, a
      # variable with a `levels` entry and no bare cobalt row is categorical.
      is_cat <- vapply(
        var,
        function(v) {
          if (v %in% located) {
            return(!is.null(observed_levels[[v]]))
          }
          !is.null(level_map) && !(length(var) > 1L && v %in% vars)
        },
        logical(1)
      )
      cont <- var[!is_cat]
      catv <- var[is_cat]
      if (length(cont) > 1L || length(catv) > 1L) {
        cli::cli_abort(c(
          "{.val {key}} maps to {length(cont)} continuous and {length(catv)} categorical variable{?s}.",
          "i" = "Map at most one continuous and one categorical variable to a concept."
        ))
      }

      if (!length(catv)) {
        if (!(cont %in% vars) || cont %in% claimed) {
          no_rows <- c(no_rows, key)
          next
        }
        claimed <- c(claimed, cont)
        keep <- c(keep, cont)
        order <- c(order, cont)
        # A lone continuous covariate is a single bold row, no header.
        labels[cont] <- paste0("**", esc_html(entry$label), "**")
        next
      }

      rows <- character(0)
      if (length(cont)) {
        if (cont %in% setdiff(vars, claimed)) {
          rows[cont] <- norm_map[[key]]$continuous_label %||% "Continuous"
          claimed <- c(claimed, cont)
        } else {
          no_rows <- c(no_rows, paste0(key, ": ", cont))
        }
      }
      rows <- c(
        rows,
        cat_rows(
          catv,
          key,
          entry$label,
          level_map,
          entry$binary_level,
          entry$binary_desc
        )
      )
      if (!length(rows)) {
        no_rows <- c(no_rows, key)
        next
      }
      add_block(key, entry$label, rows)
      next
    }

    if (isTRUE(norm_map[[key]]$indicators)) {
      rows <- character(0)
      for (i in seq_along(var)) {
        row <- .eca_indicator_row(
          var[[i]],
          setdiff(vars, claimed),
          observed_levels[[var[[i]]]],
          all_vars
        )
        if (is.na(row)) {
          no_rows <- c(no_rows, paste0(key, ": ", var[[i]]))
          next
        }
        claimed <- c(claimed, row)
        rows[row] <- names(var)[[i]]
      }
      if (length(rows)) {
        add_block(key, entry$label, rows)
      }
      next
    }

    rows <- cat_rows(
      var,
      key,
      entry$label,
      level_map,
      entry$binary_level,
      entry$binary_desc,
      entry$last_levels
    )
    if (!length(rows)) {
      no_rows <- c(no_rows, key)
      next
    }
    add_block(key, entry$label, rows)
  }

  if (!length(keep)) {
    cli::cli_abort(c(
      "None of the mapped covariates matched rows in the cobalt output.",
      "i" = "Check that the mapping's variable names match your data's column names; cobalt names categorical rows {.code <variable>_<level>}.",
      "i" = "cobalt reported: {.val {vars}}"
    ))
  }
  if (length(no_rows)) {
    cli::cli_warn(c(
      "!" = "Mapped spec covariate{?s} {?has/have} no rows in the cobalt balance output and {?is/are} omitted from the Love plot: {.val {no_rows}}.",
      "i" = "If they were not in the PS model, pass them as additional variables, e.g. {.code addl = ~ var1 + var2}."
    ))
  }
  if (length(guessed)) {
    cli::cli_warn(c(
      "!" = "Could not find the data for categorical covariate{?s} {.val {guessed}}; {?its/their} level rows were inferred from cobalt row names.",
      "i" = "Give an explicit {.code levels} entry in the mapping so no other variable's rows can be mistaken for levels."
    ))
  }
  if (length(dropped_levels)) {
    cli::cli_warn(c(
      "!" = "Mapped level{?s} {?has/have} no row in the cobalt balance output and {?is/are} omitted: {.val {dropped_levels}}.",
      "i" = "cobalt shows a single row for two-level (binary) variables, so one level of each binary is expected to be absent."
    ))
  }
  if (length(unmatched)) {
    cli::cli_warn(c(
      "!" = "No level of binary covariate{?s} {.val {unmatched}} matches the spec level to show; the row uses a fallback level instead.",
      "i" = "Recode or rename the levels so the spec level is recognisable (e.g. {.val \u226465}, {.val Male}, {.val 0}, {.val 70-80})."
    ))
  }
  if (length(unlabelled)) {
    cli::cli_warn(c(
      "!" = "The binary level shown for {.val {unlabelled}} has no label in the mapping's {.code levels}, so its raw value is displayed.",
      "i" = "The spec level is always shown; add it to {.code levels}, e.g. {.code levels = c(\"0\" = \"\u226465\", \"1\" = \">65\")}."
    ))
  }
  list(keep = keep, order = order, labels = labels)
}

#' Love-plot header for performance status: ECOG or Karnofsky
#'
#' Uses the mapping's `scale` if given; otherwise reads the scale from the
#' level text ("ECOG" / "Karnofsky" / "KPS", else any value >= 10 means
#' Karnofsky, values 0-5 mean ECOG), then from the variable name. Falls back
#' to the combined label, with a warning, if the scale can't be told.
#' @param rows cobalt rows `var_<level>`, used when the data weren't located.
#' @keywords internal
.eca_performance_label <- function(var, known, level_map, rows, scale = NULL) {
  labels <- c(
    ecog = "Baseline ECOG performance status",
    karnofsky = "Baseline Karnofsky performance status"
  )
  if (!is.null(scale)) {
    hit <- names(labels)[tolower(scale) == names(labels)]
    if (length(scale) != 1L || !length(hit)) {
      cli::cli_abort(
        "{.code scale} for {.val ecog_karnofsky} must be {.val ECOG} or {.val Karnofsky}, not {.val {scale}}."
      )
    }
    return(labels[[hit]])
  }

  word_scale <- function(txt) {
    is_kps <- any(grepl("karnofsky|kps", txt, ignore.case = TRUE))
    is_ecog <- any(grepl("ecog", txt, ignore.case = TRUE))
    if (is_kps != is_ecog) {
      if (is_kps) "karnofsky" else "ecog"
    } else {
      NA_character_
    }
  }
  txt <- c(known, names(level_map), unname(level_map))
  if (!length(txt)) {
    txt <- substring(rows, nchar(var) + 2L)
  }
  found <- word_scale(txt)
  if (is.na(found)) {
    nums <- as.numeric(unlist(regmatches(txt, gregexpr("\\d+", txt))))
    if (length(nums)) {
      found <- if (any(nums >= 10)) {
        "karnofsky"
      } else if (all(nums <= 5)) {
        "ecog"
      } else {
        NA
      }
    }
  }
  if (is.na(found)) {
    found <- word_scale(var)
  }
  if (is.na(found)) {
    cli::cli_warn(c(
      "!" = "Could not tell whether {.val {var}} is ECOG or Karnofsky performance status; using a combined label.",
      "i" = "Add {.code scale = \"ECOG\"} or {.code scale = \"Karnofsky\"} to the {.val ecog_karnofsky} mapping entry."
    ))
    return("Baseline ECOG / Karnofsky performance status")
  }
  labels[[found]]
}

#' cobalt rows `var_<level>` for a variable whose levels are unknown
#'
#' Excludes rows belonging to another mapped variable whose name extends
#' `var` (e.g. `age_binary` when guessing rows for `age`).
#' @keywords internal
.eca_prefix_rows <- function(var, avail, all_vars) {
  prefix <- paste0(var, "_")
  rows <- avail[startsWith(avail, prefix)]
  for (o in setdiff(all_vars[startsWith(all_vars, prefix)], var)) {
    rows <- rows[rows != o & !startsWith(rows, paste0(o, "_"))]
  }
  rows
}

#' cobalt row for one yes/no indicator variable
#'
#' The bare `var` row (0/1, logical), else its only `var_level` row, else the
#' row for its "positive" level (e.g. `var_Yes` when levels are Yes/No/Unknown).
#' @param avail Unclaimed cobalt row names.
#' @param known Observed levels of `var`, or `NULL` if its data weren't found.
#' @param all_vars All mapped variable names (see [.eca_prefix_rows()]).
#' @return The row name, or `NA` if none can be identified.
#' @keywords internal
.eca_indicator_row <- function(var, avail, known = NULL, all_vars = var) {
  if (var %in% avail) {
    return(var)
  }
  prefix <- paste0(var, "_")
  cand <- if (!is.null(known)) {
    intersect(paste0(prefix, known), avail)
  } else {
    .eca_prefix_rows(var, avail, all_vars)
  }
  if (length(cand) == 1L) {
    return(cand)
  }
  pos <- cand[
    tolower(substring(cand, nchar(prefix) + 1L)) %in% .eca_positive_levels
  ]
  if (length(pos) == 1L) pos else NA_character_
}

.eca_positive_levels <- c("yes", "1", "true", "present")

#' Which level to name on a binary covariate's single Love-plot row
#'
#' With `prefer` (the concept's spec `binary_level` regex), the one level whose
#' raw value or `levels` label matches it, or `NA` if not exactly one does.
#' Otherwise, in order: the level if the mapping's `levels` lists exactly one
#' of the two; else a "positive" level (Yes / 1 / TRUE / Present); else the
#' level cobalt reports (`reported`), or the last level if that's unknown.
#' @param known The two observed levels, in display order.
#' @param reported The level of cobalt's `var_<level>` row, if any.
#' @keywords internal
.eca_binary_level <- function(
  known,
  level_map = NULL,
  prefer = NULL,
  reported = NULL
) {
  if (!is.null(prefer)) {
    disp <- ifelse(known %in% names(level_map), level_map[known], known)
    hit <- grepl(prefer, known, perl = TRUE, ignore.case = TRUE) |
      grepl(prefer, disp, perl = TRUE, ignore.case = TRUE)
    return(if (sum(hit) == 1L) known[hit] else NA_character_)
  }
  mapped <- intersect(names(level_map), known)
  if (length(mapped) == 1L) {
    return(mapped)
  }
  pos <- known[tolower(known) %in% .eca_positive_levels]
  if (length(pos) == 1L) {
    return(pos)
  }
  if (length(reported) == 1L && reported %in% known) {
    return(reported)
  }
  known[[2]]
}

#' Default order for category levels with no `levels` entry or factor order
#'
#' Numeric-looking levels first, by their leading number: `<`/`\u2264` before
#' the bare number, `>`/`\u2265` or a trailing `+` after it (so
#' `1, 2, >2` and `\u226449, 50-64, ..., 85+`). Then other levels
#' alphabetically, then unknown/missing-type levels.
#' @keywords internal
.eca_sort_levels <- function(x) {
  x <- unique(as.character(x))
  low <- tolower(trimws(x))
  num_re <- "^(<=|>=|<|>|\u2264|\u2265)?\\s*(\\d+(\\.\\d+)?)"
  is_num <- grepl(num_re, low, perl = TRUE)
  num <- suppressWarnings(as.numeric(sub(paste0(num_re, ".*$"), "\\2", low, perl = TRUE)))
  num[!is_num] <- NA
  cmp <- sub(paste0(num_re, ".*$"), "\\1", low, perl = TRUE)
  side <- ifelse(
    !is_num,
    0L,
    ifelse(
      cmp %in% c("<", "<=", "\u2264"),
      -1L,
      ifelse(cmp %in% c(">", ">=", "\u2265") | grepl("\\+\\s*$", low), 1L, 0L)
    )
  )
  group <- ifelse(
    grepl("unknown|missing|not documented|not reported", low),
    3L,
    ifelse(is_num, 1L, 2L)
  )
  x[order(group, num, side, low, method = "radix")]
}

#' Regex-escape a string
#' @keywords internal
.eca_escape <- function(x) {
  # PCRE handles `{` and `}` inside a character class; TRE (R's default) does
  # not and errors with "Invalid contents of {}". Use perl = TRUE.
  gsub("([.^$*+?()\\[\\]{}|\\\\])", "\\\\\\1", x, perl = TRUE)
}

#' Legend theme shared by the Love plot and KM curve
#'
#' Off: `legend.position = "none"`, so no aesthetic can sneak a guide back in.
#' On: an untitled legend inside the panel, its `justification` corner placed
#' at `position` so the whole box stays within the plotted area.
#' @keywords internal
.eca_legend_theme <- function(
  show,
  position,
  justification,
  base_family,
  base_size
) {
  if (!isTRUE(show)) {
    return(ggplot2::theme(legend.position = "none"))
  }
  ggplot2::theme(
    legend.position = "inside",
    legend.position.inside = position,
    legend.justification.inside = justification,
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family = base_family, size = base_size)
  )
}

#' Build the standardized ECA Love (covariate balance) plot
#'
#' Takes the base `cobalt::love.plot()` ggplot produced by [as_eca_balance()]
#' and reshapes it to the spec: keep only the ten spec covariates; order the
#' y-axis per the spec with bold concept headers and indented category levels;
#' relabel; apply the dashed threshold line, the fixed 0-1 x-axis, and the
#' spec point colors; strip the title, legend, and background grid, leaving
#' axis lines only. Points and range bars beyond `x_max` are drawn at `x_max`
#' (as `cobalt::love.plot(limits = )` does), with a warning.
#'
#' @param x An `eca_balance` object from [as_eca_balance()], or a raw partner
#'   object; if raw, `mapping` (and any [as_eca_balance()] args) must be given.
#' @param mapping Covariate mapping; required only when `x` is raw.
#' @param threshold Balance threshold for the vertical dashed reference line.
#'   Default 0.10.
#' @param colors Length-2 unnamed vector for the unadjusted then adjusted point
#'   series. Defaults to the spec colors (`#E87D72`, `#56BCC2`).
#' @param base_family,base_size Font family/size. Defaults to the family
#'   resolved by [eca_setup_font()] (see [eca_font_family()]), 12.
#' @param x_max Upper x-axis limit. Default 1.
#' @param x_breaks X-axis breaks. Default 0, 0.10, 0.30, 0.50, 0.70, 0.90.
#' @param show_legend Show an inside legend for the unadjusted / adjusted
#'   series. Default `FALSE` (the spec has no legend).
#' @param legend_position Inside-panel position of the legend's bottom-right
#'   corner, in panel coordinates (0-1). Default bottom-right, `c(0.98, 0.02)`.
#' @param ... Forwarded to [as_eca_balance()] when `x` is raw.
#' @return A ggplot object (save with [eca_save_plot()], `type = "love"`).
#' @export
eca_love_plot <- function(
  x,
  mapping = NULL,
  threshold = 0.10,
  colors = c("#E87D72", "#56BCC2"),
  base_family = eca_font_family(),
  base_size = 12,
  x_max = 1,
  x_breaks = c(0, 0.10, 0.30, 0.50, 0.70, 0.90),
  show_legend = FALSE,
  legend_position = c(0.98, 0.02),
  ...
) {
  if (!inherits(x, "eca_balance")) {
    if (is.null(mapping)) {
      cli::cli_abort(c(
        "{.arg x} is not an {.cls eca_balance} object.",
        "i" = "Supply {.arg mapping} so it can be converted via {.fn as_eca_balance}."
      ))
    }
    x <- as_eca_balance(x, mapping = mapping, colors = colors, ...)
  }

  base_plot <- x$plot
  norm_map <- x$mapping

  y_var <- .eca_aes_col(base_plot, "y", fallback = "var")
  x_var <- .eca_aes_col(base_plot, "x", fallback = NA_character_)
  if (is.na(x_var)) {
    num_cols <- names(base_plot$data)[vapply(
      base_plot$data,
      is.numeric,
      logical(1)
    )]
    x_var <- num_cols[1]
  }

  lay <- .eca_love_layout(
    unique(as.character(base_plot$data[[y_var]])),
    norm_map,
    x$levels_observed %||% list()
  )

  # Keep only the mapped spec covariate rows, in plot-level AND any layer-level
  # data (love.plot usually uses plot-level data, but be defensive).
  keep_rows <- function(df) {
    if (is.data.frame(df) && y_var %in% names(df)) {
      df[as.character(df[[y_var]]) %in% lay$keep, , drop = FALSE]
    } else {
      df
    }
  }
  base_plot$data <- keep_rows(base_plot$data)
  base_plot$layers <- lapply(base_plot$layers, function(ly) {
    ly$data <- keep_rows(ly$data)
    ly
  })

  stat_cols <- intersect(c(x_var, "max.stat"), names(base_plot$data))
  big <- apply(abs(as.matrix(base_plot$data[stat_cols])) > 5, 1, any)
  big_rows <- unique(as.character(base_plot$data[[y_var]][big %in% TRUE]))
  if (length(big_rows)) {
    origin <- if (identical(x$source, "precomputed")) {
      "These are the supplied (precomputed) values"
    } else if (identical(x$binary, "raw")) {
      "These are cobalt's mean differences (standardized for continuous rows, raw for binary rows)"
    } else {
      "These are standardized SMDs from cobalt"
    }
    cli::cli_warn(c(
      "!" = "Plotted mean difference{?s} above 5 for {.val {big_rows}}.",
      "i" = paste0(
        origin,
        ", not a calculation error by this script; values this large usually mean a rare level or near-separation between arms. Worth checking the data."
      )
    ))
  }

  over <- apply(as.matrix(base_plot$data[stat_cols]) > x_max, 1, any)
  over_rows <- unique(as.character(base_plot$data[[y_var]][over %in% TRUE]))
  if (length(over_rows)) {
    cli::cli_warn(c(
      "!" = paste0(
        "Absolute standardized mean difference{?s} above ",
        x_max,
        " for {.val {over_rows}} {?is/are} drawn at the axis limit."
      ),
      "i" = "Points and range bars are clamped to the fixed x-axis so every partner's plot shares one scale."
    ))
  }

  # Dashed threshold line, drawn BENEATH the points.
  base_plot$layers <- c(
    list(ggplot2::geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      colour = "grey40"
    )),
    base_plot$layers
  )

  # Overriding the scales that love.plot already set emits chatty "Scale for ...
  # already present" messages; suppress them for a clean partner experience.
  # cobalt's Sample factor is always (unadjusted, adjusted), whatever
  # `sample.names` renames them to.
  samp <- base_plot$data$Sample
  sample_colors <- if (is.factor(samp) && nlevels(samp) == 2L) {
    stats::setNames(colors[1:2], levels(samp))
  } else {
    c(Unadjusted = colors[[1]], Adjusted = colors[[2]])
  }

  y_labels <- ggtext::element_markdown(
    family = base_family,
    size = base_size,
    hjust = 1,
    # Small breathing room between right-aligned labels and the axis.
    margin = ggplot2::margin(r = 6)
  )

  suppressMessages(
    base_plot +
      ggplot2::scale_color_manual(values = sample_colors) +
      ggplot2::scale_y_discrete(
        limits = rev(lay$order),
        labels = lay$labels,
        drop = FALSE
      ) +
      ggplot2::scale_x_continuous(
        name = "Absolute standardized mean difference",
        breaks = x_breaks,
        labels = function(b) ifelse(b == 0, "0", sprintf("%.2f", b)),
        limits = c(0, x_max),
        # squish, not the default censor: out-of-range points sit at x_max.
        oob = scales::squish,
        expand = ggplot2::expansion(mult = c(0, 0.02))
      ) +
      ggplot2::labs(title = NULL, subtitle = NULL, y = NULL) +
      eca_theme(base_family = base_family, base_size = base_size) +
      .eca_legend_theme(
        show_legend,
        legend_position,
        c(1, 0),
        base_family,
        base_size
      ) +
      ggplot2::theme(
        axis.text.y = y_labels,
        # ggplot2 >= 4.0's complete themes set a plain `axis.text.y.left`,
        # which would override `axis.text.y` and print the markdown literally.
        axis.text.y.left = y_labels,
        # No y tick marks -- the row labels themselves position each covariate.
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "black")
      )
  )
}


# =============================================================================
# 5. Survival: converters + Rubin's-rules pooling + KM curve
# =============================================================================

#' Convert a partner's object into a canonical per-subject survival table
#'
#' Produces an `eca_survival` object: one row per subject per imputation, with
#' columns `imp`, `arm` (factor, control level first), `time`, `event` (0/1),
#' `weight`, `cluster_id`. This is the input to [eca_pool_survival()] /
#' [eca_km_curve()].
#'
#' Supported `x`:
#' * `mimids`/`wimids` (MatchThem) — uses `MatchThem::complete(x, "long")`;
#'   pulls `weights` and (if present) `subclass`.
#' * `data.frame` — stacked long rows; provide the column names below. For
#'   single-imputation / pre-pooled data omit `imp_col` (treated as one dataset).
#' * `list` of data frames — one per imputation; row-bound with `imp` = index.
#'
#' @param x Input object.
#' @param time_col,event_col Column names for follow-up time and 0/1 event.
#' @param arm_col Column identifying the arm.
#' @param control_value Value of `arm_col` denoting the RESOLVE control arm.
#' @param weight_col Column of analysis weights. `NULL` -> all weights 1
#'   (e.g. the trial control arm, or an unweighted sensitivity curve).
#' @param imp_col Imputation-id column (data.frame path). `NULL` = single data.
#' @param cluster_col Subclass/cluster id for robust SEs (matching).
#'   `NULL` for IPTW-type weights.
#' @param arm_labels Named vector `c(control = ..., eca = ...)` for display.
#' @param time_unit Required: `"days"` or `"months"`, the unit `time_col` is
#'   stored in. Recorded as an attribute and used by [eca_km_curve()] to
#'   convert the x-axis. Warns if the longest time looks like the other unit.
#' @return An `eca_survival` object.
#' @export
as_eca_survival <- function(
  x,
  time_col,
  event_col,
  arm_col,
  control_value,
  weight_col = NULL,
  imp_col = NULL,
  cluster_col = NULL,
  arm_labels = c(
    control = "RESOLVE Trial Control Arm",
    eca = "External Comparator Arm"
  ),
  time_unit = NULL
) {
  time_unit <- .eca_time_unit(time_unit)

  long <- .eca_survival_long(x, imp_col = imp_col) # -> data.frame + 'imp' col

  need <- c(time_col, event_col, arm_col)
  missing_cols <- need[!need %in% names(long)]
  if (length(missing_cols)) {
    cli::cli_abort("Column{?s} not found in the data: {.val {missing_cols}}.")
  }

  check_cols <- c(need, weight_col)
  n_na <- vapply(check_cols, function(v) sum(is.na(long[[v]])), integer(1))
  if (any(n_na > 0)) {
    bad <- n_na[n_na > 0]
    cli::cli_abort(c(
      "Missing values in survival input column{?s} {.val {names(bad)}}.",
      "i" = "NA counts: {paste0(names(bad), ' = ', bad, collapse = ', ')}.",
      "i" = "Drop or resolve these rows before calling {.fn as_eca_survival}."
    ))
  }
  time_num <- as.numeric(long[[time_col]])
  .eca_check_time_scale(time_num, time_unit)
  if (any(time_num < 0)) {
    neg_arms <- table(as.character(long[[arm_col]][time_num < 0]))
    cli::cli_warn(c(
      "{.val {sum(time_num < 0)}} row{?s} of {.arg time_col} ({.val {time_col}}) {?is/are} negative (by arm: {paste0(names(neg_arms), ' = ', neg_arms, collapse = ', ')}).",
      "i" = "They are kept in the KM estimate, as {.fn survival::survfit} does; the KM plot starts at time 0.",
      "i" = "Check the follow-up derivation, or drop those rows first if they shouldn't count."
    ))
  }
  event_int <- as.integer(long[[event_col]])
  if (!all(event_int %in% c(0L, 1L))) {
    cli::cli_abort(c(
      "{.arg event_col} ({.val {event_col}}) must be coded 0/1.",
      "i" = "Observed values: {.val {sort(unique(as.character(long[[event_col]])))}}."
    ))
  }
  if (!is.null(weight_col) && any(as.numeric(long[[weight_col]]) < 0)) {
    cli::cli_abort(
      "{.arg weight_col} ({.val {weight_col}}) has {.val {sum(as.numeric(long[[weight_col]]) < 0)}} negative weight{?s}."
    )
  }

  is_control <- long[[arm_col]] == control_value
  if (all(is.na(is_control))) {
    cli::cli_abort(c(
      "Comparing {.arg arm_col} ({.val {arm_col}}) with {.arg control_value} produced all-NA.",
      "i" = "This usually means a type mismatch (e.g. {.arg arm_col} is a factor but {.arg control_value} is numeric)."
    ))
  }
  if (!any(is_control, na.rm = TRUE)) {
    cli::cli_abort(c(
      "No rows in {.arg arm_col} ({.val {arm_col}}) equal {.arg control_value} ({.val {control_value}}).",
      "i" = "Observed values: {.val {unique(as.character(long[[arm_col]]))}}. Check the coding of the trial control arm."
    ))
  }
  if (all(is_control, na.rm = TRUE)) {
    cli::cli_abort(c(
      "Every row in {.arg arm_col} equals {.arg control_value} ({.val {control_value}}); the external comparator arm is empty.",
      "i" = "Confirm the arm coding, or that ECA subjects survived the upstream filters."
    ))
  }
  out <- data.frame(
    imp = long$imp,
    arm = factor(
      ifelse(is_control, arm_labels[["control"]], arm_labels[["eca"]]),
      levels = c(arm_labels[["control"]], arm_labels[["eca"]])
    ),
    time = time_num,
    event = event_int,
    weight = if (is.null(weight_col)) 1 else as.numeric(long[[weight_col]]),
    cluster_id = if (is.null(cluster_col)) NA else long[[cluster_col]],
    stringsAsFactors = FALSE
  )
  attr(out, "time_unit") <- time_unit
  attr(out, "arm_labels") <- arm_labels
  class(out) <- c("eca_survival", class(out))
  out
}

#' Validate a required `time_unit` argument
#' @keywords internal
.eca_time_unit <- function(time_unit) {
  if (is.null(time_unit)) {
    cli::cli_abort(c(
      "{.arg time_unit} is required: {.val days} or {.val months}.",
      "i" = "Give the unit follow-up time is stored in; the KM x-axis converts days to months at 30.4375 days per month."
    ))
  }
  match.arg(time_unit, c("days", "months"))
}

#' Warn when follow-up times look like the other unit
#' @keywords internal
.eca_check_time_scale <- function(time, time_unit) {
  mx <- max(time, na.rm = TRUE)
  if (identical(time_unit, "days") && mx < 100) {
    cli::cli_warn(c(
      "!" = "The longest follow-up time is {mx} with {.code time_unit = \"days\"} (about {cards::round5(mx / ECA_DAYS_PER_MONTH, 1)} month{?s}).",
      "i" = "If times are stored in months, set {.code time_unit = \"months\"}."
    ))
  }
  if (identical(time_unit, "months") && mx > 400) {
    cli::cli_warn(c(
      "!" = "The longest follow-up time is {mx} with {.code time_unit = \"months\"} (about {cards::round5(mx / 12)} year{?s}).",
      "i" = "If times are stored in days, set {.code time_unit = \"days\"}."
    ))
  }
}

#' Coerce various inputs to a long data.frame carrying an `imp` column
#' @keywords internal
.eca_survival_long <- function(x, imp_col) {
  cls <- class(x)
  if (any(c("mimids", "wimids") %in% cls)) {
    if (!requireNamespace("MatchThem", quietly = TRUE)) {
      cli::cli_abort(
        "Package {.pkg MatchThem} is needed for {.cls mimids}/{.cls wimids} input."
      )
    }
    long <- MatchThem::complete(x, action = "long")
    long$imp <- as.integer(long$.imp)
    return(long)
  }
  if (is.list(x) && !is.data.frame(x)) {
    # list of per-imputation data frames
    long <- do.call(
      rbind,
      Map(
        function(df, i) {
          df$imp <- i
          df
        },
        x,
        seq_along(x)
      )
    )
    return(long)
  }
  if (is.data.frame(x)) {
    long <- as.data.frame(x)
    long$imp <- if (is.null(imp_col)) {
      1L
    } else {
      as.integer(as.factor(long[[imp_col]]))
    }
    return(long)
  }
  cli::cli_abort(c(
    "Can't build survival data from an object of class {.cls {cls}}.",
    "i" = "Supply a {.cls mimids}/{.cls wimids} object, a {.cls data.frame}, or a list of per-imputation data frames."
  ))
}

#' Build an eca_pooled_survival object directly from a survival table
#'
#' For partners (e.g. SAS analyses) who computed pooled survival externally.
#' The data frame must have columns `arm`, `time`, `surv`, `lower`, `upper`,
#' with survival on the 0-1 scale. `arm` must have exactly two values. An arm
#' without a `time == 0` row gets `(0, 1, 1, 1)` prepended.
#'
#' @param df Data frame with the columns above.
#' @param control_arm Required: the value of `df$arm` for the RESOLVE trial
#'   control arm (drawn black). The other arm is the ECA.
#' @param time_unit Required: `"days"` or `"months"`.
#' @return An `eca_pooled_survival` object, arms ordered control first.
#' @export
as_eca_pooled_survival <- function(df, control_arm = NULL, time_unit = NULL) {
  time_unit <- .eca_time_unit(time_unit)
  req <- c("arm", "time", "surv", "lower", "upper")
  if (!all(req %in% names(df))) {
    cli::cli_abort("{.arg df} must contain columns: {.val {req}}.")
  }
  df <- as.data.frame(df)[, req]
  na_cols <- c("arm", "time", "surv")[vapply(
    df[c("arm", "time", "surv")],
    anyNA,
    logical(1)
  )]
  if (length(na_cols)) {
    cli::cli_abort(c(
      "{.arg df} has missing values in {.val {na_cols}}.",
      "i" = "Only {.arg lower}/{.arg upper} may be NA (no valid CI there; the ribbon stops)."
    ))
  }
  arms <- unique(as.character(df$arm))
  if (length(arms) != 2) {
    cli::cli_abort(c(
      "{.arg df$arm} must have exactly 2 arms, not {length(arms)}.",
      "i" = "Observed: {.val {arms}}."
    ))
  }
  if (is.null(control_arm) || !as.character(control_arm) %in% arms) {
    cli::cli_abort(c(
      "{.arg control_arm} must name the RESOLVE trial control arm in {.arg df$arm}.",
      "i" = "Observed arms: {.val {arms}}."
    ))
  }
  control_arm <- as.character(control_arm)
  df$arm <- factor(
    as.character(df$arm),
    levels = c(control_arm, setdiff(arms, control_arm))
  )
  .eca_check_time_scale(df$time, time_unit)
  probs <- unlist(df[c("surv", "lower", "upper")])
  if (any(probs < 0 | probs > 1, na.rm = TRUE)) {
    mx <- max(probs, na.rm = TRUE)
    cli::cli_abort(c(
      "{.arg surv}, {.arg lower} and {.arg upper} must be probabilities in [0, 1].",
      "i" = if (mx > 1 && mx <= 100) {
        "The maximum is {mx}; if these are percents, divide by 100."
      }
    ))
  }
  if (any(df$time < 0)) {
    cli::cli_warn(c(
      "{.arg df$time} has {sum(df$time < 0)} negative value{?s}.",
      "i" = "They are kept; the KM plot starts at time 0."
    ))
  }

  no_zero <- levels(df$arm)[
    !vapply(
      levels(df$arm),
      function(a) any(df$time[df$arm == a] == 0),
      logical(1)
    )
  ]
  if (length(no_zero)) {
    cli::cli_inform(
      "Arm{?s} {.val {no_zero}} {?has/have} no time-0 row; adding (time = 0, surv = 1) so the curve starts at 100%."
    )
    start <- data.frame(
      arm = factor(no_zero, levels = levels(df$arm)),
      time = 0,
      surv = 1,
      lower = 1,
      upper = 1
    )
    df <- rbind(start, df)
  }
  df <- df[order(df$arm, df$time), ]
  rownames(df) <- NULL
  attr(df, "time_unit") <- time_unit
  class(df) <- c("eca_pooled_survival", "data.frame")
  df
}

#' Pool weighted Kaplan-Meier estimates across imputations with Rubin's rules
#'
#' For each arm and each imputation, fits a weighted KM estimator (robust SEs,
#' `conf.type = "log-log"`), summarizes on a common time grid, transforms the
#' survival probability to the complementary log-log scale `Q = log(-log S)`,
#' and combines across imputations with Rubin's rules on that scale before
#' back-transforming. Use the same pooling for the median-OS / landmark
#' estimates so the figure and tables agree.
#'
#' Single-dataset arms (the trial control arm, or `m = 1`) reduce to the plain
#' weighted KM with its own log-log confidence limits.
#'
#' @param surv An `eca_survival` object from [as_eca_survival()].
#' @param time_grid Optional numeric vector of times (in the object's units) at
#'   which to evaluate. Default: within each arm, 0 plus the observed event
#'   times.
#' @param conf_level Confidence level. Default 0.95.
#' @return An `eca_pooled_survival` object (`arm`, `time`, `surv`, `lower`,
#'   `upper`), carrying the same `time_unit` attribute as `surv`. `lower` and
#'   `upper` are `NA` where no valid CI exists (see [.eca_rubin_cloglog()]).
#' @export
eca_pool_survival <- function(surv, time_grid = NULL, conf_level = 0.95) {
  if (!inherits(surv, "eca_survival")) {
    cli::cli_abort(
      "{.arg surv} must be an {.cls eca_survival} object from {.fn as_eca_survival}."
    )
  }
  if (!requireNamespace("survival", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg survival} is required.")
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  arms <- levels(surv$arm)

  pieces <- lapply(arms, function(this_arm) {
    a <- surv[surv$arm == this_arm, , drop = FALSE]
    imps <- sort(unique(a$imp))

    grid <- time_grid
    if (is.null(grid)) {
      ev <- sort(unique(a$time[a$event == 1]))
      grid <- sort(unique(c(0, ev)))
    }

    per_imp <- lapply(imps, function(i) {
      d <- a[a$imp == i, , drop = FALSE]
      # If a cluster id is in play, survfit errors on NA ids. Drop rows with a
      # missing cluster id ONLY on the survival side; this doesn't affect the
      # balance path (`as_eca_balance` sees the untouched data), which needs
      # weight-0/unmatched subjects for the pre-matching SMD denominator.
      use_cluster <- !all(is.na(d$cluster_id))
      if (use_cluster) {
        d <- d[!is.na(d$cluster_id), , drop = FALSE]
      }
      fit_args <- list(
        formula = survival::Surv(time, event) ~ 1,
        data = d,
        weights = d$weight,
        robust = TRUE,
        conf.type = "log-log"
      )
      if (use_cluster) {
        fit_args$id <- d$cluster_id
      }
      fit <- do.call(survival::survfit, fit_args)
      s <- summary(fit, times = grid, extend = TRUE)
      data.frame(time = s$time, surv = s$surv, std.err = s$std.err, imp = i)
    })
    imp_df <- do.call(rbind, per_imp)

    # Pool per time point on the complementary log-log scale.
    pooled <- lapply(split(imp_df, imp_df$time), function(g) {
      .eca_rubin_cloglog(g$surv, g$std.err, z = z)
    })
    res <- do.call(rbind, pooled)
    res$time <- as.numeric(names(pooled))
    res$arm <- this_arm
    res[order(res$time), c("arm", "time", "surv", "lower", "upper")]
  })

  out <- do.call(rbind, pieces)
  out$arm <- factor(out$arm, levels = arms)
  attr(out, "time_unit") <- attr(surv, "time_unit")
  class(out) <- c("eca_pooled_survival", "data.frame")
  out
}

#' Rubin's-rules pooling of survival at one time point on the cloglog scale
#'
#' Every imputation counts (`m = length(surv)`),
#' including ones with S of 0 or 1. If any imputation has S = 0 (or 1), the
#' pooled S is 0 (or 1) and the CI is `NA`, so the ribbon stops where no valid
#' interval exists (e.g. the last patient at risk dies). One exception: when
#' S = 1 in every imputation (e.g. time 0), the CI is (1, 1) so the band
#' starts at 100%. With one imputation, the between-imputation variance is 0.
#' @keywords internal
.eca_rubin_cloglog <- function(surv, std.err, z) {
  if (all(surv >= 1, na.rm = TRUE)) {
    return(data.frame(surv = 1, lower = 1, upper = 1))
  }
  m <- length(surv)
  Q <- log(-log(surv)) # complementary log-log
  Vq <- (std.err / (surv * log(surv)))^2 # delta-method variance of Q

  Q_bar <- mean(Q, na.rm = TRUE)
  V_within <- mean(Vq, na.rm = TRUE)
  B <- if (sum(!is.na(Q)) > 1) stats::var(Q, na.rm = TRUE) else 0
  V_total <- V_within + (1 + 1 / m) * B

  ci_ok <- is.finite(Q_bar) && is.finite(V_total)
  lo_Q <- Q_bar - z * sqrt(V_total)
  hi_Q <- Q_bar + z * sqrt(V_total)

  # Back-transform. Note the double negation flips the CI ends.
  data.frame(
    surv = exp(-exp(Q_bar)),
    lower = if (ci_ok) exp(-exp(hi_Q)) else NA_real_,
    upper = if (ci_ok) exp(-exp(lo_Q)) else NA_real_
  )
}

#' Step-expand pooled CIs so the ribbon matches `geom_step()`
#'
#' Each grid point's CI is held until the next grid point, as the survival
#' estimate is, unless the next point has no CI: the band then ends at the
#' last valid point (see [.eca_rubin_cloglog()]).
#'
#' @keywords internal
.eca_step_ribbon <- function(pooled) {
  do.call(
    rbind,
    lapply(split(pooled, pooled$arm, drop = TRUE), function(d) {
      d <- d[order(d$month), ]
      n <- nrow(d)
      ends <- d[-1, ]
      # No CI at the interval's end means everyone left dies there, so the
      # band stops rather than spanning the last patient's interval.
      ends$lower <- ifelse(is.na(ends$lower), NA_real_, d$lower[-n])
      ends$upper <- ifelse(is.na(ends$upper), NA_real_, d$upper[-n])
      out <- rbind(d, ends)
      # Stable order: each interval's end sorts before the next interval's start.
      out[order(out$month, rep(c(1, 0), c(n, n - 1))), ]
    })
  )
}

#' Build the standardized ECA Kaplan-Meier curve
#'
#' @param x An `eca_survival` object (will be pooled here), an already-pooled
#'   `eca_pooled_survival` object, or a raw partner object plus the
#'   [as_eca_survival()] arguments in `...`.
#' @param colors Named vector for the two arms, keyed by the arm display labels.
#'   Defaults to the spec: control = black, ECA = `"#4A68D9"`. If your arm
#'   labels differ, name this vector to match them.
#' @param linetypes Named vector of line types for the two arms, keyed like
#'   `colors`. Defaults to the spec: control solid, ECA dashed.
#' @param show_ci Draw the shaded confidence band. Default `TRUE`.
#' @param show_legend Show an inside legend distinguishing the arms. Default
#'   `FALSE` (the spec has no legend).
#' @param legend_position Inside-panel position of the legend's top-right
#'   corner, in panel coordinates (0-1). Default top-right, `c(0.98, 0.98)`.
#' @param x_limits,x_by X-axis (months) range and break spacing. Defaults
#'   `c(0, 32)` and 4, per the spec.
#' @param base_family,base_size Font family/size. Defaults to the family
#'   resolved by [eca_setup_font()] (see [eca_font_family()]), 12.
#' @param control_label Which arm level is the black control curve; defaults to
#'   the first factor level of `arm`.
#' @param ... Forwarded to [as_eca_survival()] when `x` is raw.
#' @return A ggplot object (save with [eca_save_plot()], `type = "km"`).
#' @export
eca_km_curve <- function(
  x,
  colors = NULL,
  linetypes = NULL,
  show_ci = TRUE,
  show_legend = FALSE,
  legend_position = c(0.98, 0.98),
  x_limits = c(0, 32),
  x_by = 4,
  base_family = eca_font_family(),
  base_size = 12,
  control_label = NULL,
  ...
) {
  # Resolve input to an eca_pooled_survival.
  if (inherits(x, "eca_pooled_survival")) {
    pooled <- x
  } else if (inherits(x, "eca_survival")) {
    pooled <- eca_pool_survival(x)
  } else {
    pooled <- eca_pool_survival(as_eca_survival(x, ...))
  }

  time_unit <- attr(pooled, "time_unit") %||% "days"
  pooled$month <- if (identical(time_unit, "months")) {
    pooled$time
  } else {
    pooled$time / ECA_DAYS_PER_MONTH
  }

  arms <- levels(pooled$arm)
  if (is.null(control_label)) {
    control_label <- arms[1]
  }
  if (!control_label %in% arms) {
    cli::cli_abort(c(
      "{.arg control_label} ({.val {control_label}}) is not one of the arm levels.",
      "i" = "Available arm levels: {.val {arms}}."
    ))
  }
  # Ensure the control arm plots first for a stable legend order.
  arms <- c(control_label, setdiff(arms, control_label))
  pooled$arm <- factor(pooled$arm, levels = arms)

  if (is.null(colors)) {
    colors <- stats::setNames(c("black", "#4A68D9"), arms)
  }
  if (is.null(linetypes)) {
    linetypes <- stats::setNames(c("solid", "dashed"), arms)
  }

  p <- ggplot2::ggplot(
    pooled,
    ggplot2::aes(
      x = .data$month,
      y = .data$surv,
      color = .data$arm,
      fill = .data$arm
    )
  )
  if (isTRUE(show_ci)) {
    p <- p +
      ggplot2::geom_ribbon(
        data = .eca_step_ribbon(pooled),
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        alpha = 0.2,
        colour = NA
      )
  }
  p <- p +
    ggplot2::geom_step(
      ggplot2::aes(linetype = .data$arm),
      linewidth = 1,
      linejoin = "mitre"
    ) +
    ggplot2::scale_color_manual(values = colors, breaks = arms) +
    ggplot2::scale_fill_manual(values = colors, breaks = arms) +
    ggplot2::scale_linetype_manual(values = linetypes, breaks = arms) +
    ggplot2::scale_x_continuous(
      name = "Time (months)",
      breaks = seq(x_limits[1], x_limits[2], by = x_by)
    ) +
    ggplot2::scale_y_continuous(
      name = "Probability of survival (%)",
      breaks = seq(0, 1, by = 0.25),
      labels = scales::percent_format(accuracy = 1)
    ) +
    # Clip with coord_cartesian (NOT scale limits): scale limits would drop rows
    # past the axis end and truncate the survival step before it.
    ggplot2::coord_cartesian(xlim = x_limits, ylim = c(0, 1), expand = FALSE) +
    ggplot2::guides(y = ggplot2::guide_axis(cap = "upper")) +
    eca_theme(base_family = base_family, base_size = base_size) +
    .eca_legend_theme(
      show_legend,
      legend_position,
      c(1, 1),
      base_family,
      base_size
    )
}


# =============================================================================
# 6. Saving (ragg for cross-machine consistency)
# =============================================================================

#' Save an ECA figure to PNG via ragg at the spec dimensions
#'
#' Uses `ragg::agg_png` through `ggplot2::ggsave` so text/anti-aliasing is
#' consistent across partners' machines. Default dimensions follow the spec:
#' Love plot 10 x 6.38 in; KM curve 10 x 6.67 in.
#'
#' **Resolution and font size.** `dpi` sets the output resolution *and* is used
#' to size text correctly. `ragg` sizes fonts correctly at any DPI on its own.
#' When `use_showtext = TRUE`, this function additionally sets
#' `showtext::showtext_opts(dpi = dpi)` to the SAME `dpi` before saving — this
#' is essential: if showtext's DPI does not match the device DPI, point sizes
#' render wrong (e.g. 12 pt looks tiny at 600 dpi). Because both come from the
#' one `dpi` argument here, they can never drift apart. showtext auto-rendering
#' is switched on for the save, then both it and the DPI option are restored
#' to their prior state. Warns if a Love/KM plot's font isn't Open Sans (e.g.
#' [eca_setup_font()] wasn't run, or fell back, before the plot was built).
#'
#' @param plot A ggplot object.
#' @param filename Output path (`.png`).
#' @param type `"love"`, `"km"`, or `"custom"` (then give `width`/`height`).
#' @param width,height Inches; defaulted from `type` when `NULL`.
#' @param dpi Resolution in dots per inch. Default 350 (spec minimum). Drives
#'   both the saved resolution and showtext's font sizing.
#' @param use_showtext Render text with `showtext` (with DPI synchronized to
#'   `dpi`). Default `TRUE`. `showtext` and
#'   `sysfonts` are required across the board (see [eca_check_deps()]); the
#'   defensive fallback below only triggers if a partner sourced this file
#'   without running the dependency check first. Set `FALSE` to force the pure
#'   `ragg` path (also sizes fonts correctly).
#' @param background Background colour. Default `"white"`.
#' @return (invisibly) the filename.
#' @export
eca_save_plot <- function(
  plot,
  filename,
  type = c("love", "km", "custom"),
  width = NULL,
  height = NULL,
  dpi = 350,
  use_showtext = TRUE,
  background = "white"
) {
  type <- match.arg(type)
  dims <- switch(type, love = c(10, 6.38), km = c(10, 6.67), custom = c(NA, NA))
  if (is.null(width)) {
    width <- dims[1]
  }
  if (is.null(height)) {
    height <- dims[2]
  }
  if (is.na(width) || is.na(height)) {
    cli::cli_abort(
      "For {.code type = \"custom\"}, supply both {.arg width} and {.arg height}."
    )
  }

  plot_family <- plot$theme$text$family
  if (
    type != "custom" &&
      !is.null(plot_family) &&
      !identical(plot_family, "Open Sans")
  ) {
    cli::cli_warn(c(
      "!" = "This plot uses font {.val {plot_family}}, not the spec's {.val Open Sans}.",
      "i" = "Run {.fn eca_setup_font} before building the plot (it must succeed), then rebuild and save."
    ))
  }

  # Optionally route text through showtext, with its DPI locked to the save DPI.
  if (isTRUE(use_showtext)) {
    have_showtext <- requireNamespace("showtext", quietly = TRUE) &&
      requireNamespace("sysfonts", quietly = TRUE)
    if (!have_showtext) {
      cli::cli_warn(c(
        "!" = "{.arg use_showtext = TRUE} but {.pkg showtext}/{.pkg sysfonts} are not installed.",
        "i" = "Falling back to {.pkg ragg}'s native text rendering (also sizes fonts correctly)."
      ))
    } else {
      # Sync showtext's DPI to the save DPI so point sizes are correct, then
      # restore the prior showtext state after the save.
      old_dpi <- showtext::showtext_opts()$dpi
      # showtext has no getter; showtext_auto(TRUE) installs this hook.
      was_auto <- any(vapply(
        getHook("grid.newpage"),
        inherits,
        logical(1),
        "showtext_hook"
      ))
      showtext::showtext_opts(dpi = dpi)
      showtext::showtext_auto(TRUE)
      on.exit(
        {
          showtext::showtext_auto(was_auto)
          if (!is.null(old_dpi)) showtext::showtext_opts(dpi = old_dpi)
        },
        add = TRUE
      )
    }
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = ragg::agg_png,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = background
  )
  invisible(filename)
}


# =============================================================================
# 7. small utilities
# =============================================================================

#' Null-coalescing helper
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a)) b else a
