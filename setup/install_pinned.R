# =============================================================================
# setup/install_pinned.R
#
# Goal: every data partner renders with the SAME package versions, so the two
# figures are byte-comparable (modulo fonts). Run ONE of the two approaches
# below. All packages are on CRAN -- nothing here requires a custom/unreviewed
# package, so it should pass site restrictions.
#
# These are the shared pinned versions (R 4.4.3) every partner should match.
# =============================================================================

# Required across the board -- these render every figure and are always used.
# Ordered low-level first: remotes::install_version() only checks that a
# dependency is present, so installing rlang/cli/etc. before ggplot2 & co keeps
# a newer CRAN copy from being pulled in underneath them.
pinned <- c(
  rlang       = "1.1.6",
  cli         = "3.6.5",
  cards       = "0.7.1",
  scales      = "1.3.0",
  systemfonts = "1.1.0",
  sysfonts    = "0.8.9",   # font registration backing showtext
  survival    = "3.8-3",
  ggplot2     = "3.5.1",
  ragg        = "1.3.3",
  showtext    = "0.9-7",   # font rendering with DPI-synced sizing (eca_save_plot)
  ggtext      = "0.1.2",
  cobalt      = "4.5.5"
)

# install_version() builds from source: Windows needs Rtools; Linux/macOS need
# the freetype, harfbuzz, fribidi, libpng (and libtiff/libjpeg) development
# libraries for systemfonts/ragg.

# -----------------------------------------------------------------------------
# APPROACH A (preferred if you already use renv): add these pins to your project
# renv and snapshot. Sourcing this file inside an active renv project skips
# Approach B; run the lines below by hand instead.
# -----------------------------------------------------------------------------
# renv::init()                      # only if this project has no renv yet
# for (p in names(pinned)) renv::record(paste0(p, "@", unname(pinned[p])))
# renv::install(paste0(names(pinned), "@", unname(pinned)))
# renv::snapshot()

# -----------------------------------------------------------------------------
# APPROACH B (no renv): install exact versions into your library with remotes.
# -----------------------------------------------------------------------------
use_approach_b <- !nzchar(Sys.getenv("RENV_PROJECT"))
if (!use_approach_b) {
  message("Active renv project detected: skipping Approach B. Use Approach A above.")
}
if (use_approach_b && !requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
same_version <- function(a, b) {
  # packageVersion() prints with "." separators; CRAN DESCRIPTION files
  # often use "-" (e.g., survival's "3.8-3" prints as "3.8.3"). numeric_version
  # normalizes both so 3.8-3 and 3.8.3 compare equal.
  if (is.na(a) || is.na(b)) return(FALSE)
  ok <- tryCatch(numeric_version(a) == numeric_version(b), error = function(e) NA)
  isTRUE(ok)
}
install_or_ask <- function(pkg, want) {
  # Three cases:
  #   (a) not installed        -> install pinned version (no prompt).
  #   (b) installed, matches   -> skip.
  #   (c) installed, differs   -> interactively ask before touching it. In
  #                               non-interactive sessions, keep the installed
  #                               version so we never silently downgrade CI.
  current <- tryCatch(as.character(utils::packageVersion(pkg)),
                      error = function(e) NA_character_)
  if (is.na(current)) {
    message(sprintf("%s: not installed - installing pinned version %s ...", pkg, want))
    remotes::install_version(pkg, version = want, upgrade = "never")
    return(invisible())
  }
  if (same_version(current, want)) {
    message(sprintf("%s %s already installed - skipping.", pkg, want))
    return(invisible())
  }
  prompt <- sprintf(
    "%s: installed %s differs from pinned %s.\n  Install pinned version for cross-partner consistency?",
    pkg, current, want
  )
  ans <- if (interactive()) {
    utils::askYesNo(prompt, default = FALSE)
  } else {
    message(prompt, " Non-interactive session; keeping installed version.")
    FALSE
  }
  if (isTRUE(ans)) {
    message(sprintf("Installing %s %s (had: %s) ...", pkg, want, current))
    remotes::install_version(pkg, version = want, upgrade = "never")
  } else {
    message(sprintf("Keeping %s %s (differs from pinned %s).", pkg, current, want))
  }
  invisible()
}
if (use_approach_b) {
  for (p in names(pinned)) install_or_ask(p, unname(pinned[[p]]))
}

# Scenario-specific runtime packages: install ONLY the ones matching how YOU ran
# the analysis. These affect input conversion, not the final figures. Pick the
# row for your situation and un-comment the corresponding install_version call.
#
#   Scenario                                            Packages you need
#   --------------------------------------------------  --------------------------
#   MatchThem mimids / wimids (Scenario 1)              MatchThem, mice
#     ... with optimal or full matching                 + optmatch
#   WeightIt wimids via MatchThem (Scenario 2)          MatchThem, WeightIt, mice
#   Hand-computed PS + IPTW weights (Scenario 3)        (none of these)
#   SAS export / pre-pooled survival (Scenario 4a/b)    haven (only if reading .sas7bdat)
#
# MatchIt is installed with MatchThem; pin it too so its version matches.
#
# Versions below are the shared pins; adjust if your analysis used a
# different version.
scenario_pkgs <- c(MatchThem = "1.2.1", MatchIt = "4.7.0", WeightIt = "1.3.2",
                   mice = "3.17.0", optmatch = "0.10.8", haven = "2.5.4")
# for (p in <the packages you need>)
#   remotes::install_version(p, version = unname(scenario_pkgs[[p]]),
#                            upgrade = "never")

message("Done. Now: source('ecaplots.R'); eca_check_deps(strict = TRUE)")
