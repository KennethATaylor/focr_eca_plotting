# ecaplots — standardized ECA-pilot figures

One sourceable R script that produces the two standardized figures every data partner in the ECA pilot must submit:

1.  a **Love plot** (covariate balance), and
2.  a **Kaplan–Meier curve** (RESOLVE trial control arm vs. the external comparator arm).

It is **not** an installed package — you `source("ecaplots.R")` and call the functions. Every dependency is a CRAN package (`cobalt`, `survival`, `ggplot2`, `ggtext`, `ragg`, `systemfonts`, `scales`, `rlang`, `cli`, `cards`, plus `showtext` / `sysfonts` for DPI-synced font sizing and the Open Sans download), so there is nothing custom to get through a package review.

## Why a common script?

Partners reached their weighted/matched analyses by different routes (`MatchIt`/`MatchThem`, `WeightIt`, or hand-rolled PS + IPTW-ATT weights) and hold the results in different shapes (a `MatchThem` object, a stacked multiply-imputed data frame, or a survival table exported from SAS). This script converts any of those into two small **canonical structures** and renders identical, spec-compliant figures from them:

- balance is computed with **cobalt** (same standardization for everyone), except for SAS partners who supply their own SMDs (see the SAS note below);
- the ECA survival curve is always **pooled across the 5 imputations with Rubin's rules** on the complementary-log-log scale;
- image files are always written with the **`ragg`** package (to improve image quality and consistency across different OS environments) at the spec dimensions.

## Quick start

Run from the `eca_standard_plots/` directory (or adjust the `source()` paths).

**One-time setup, before anything else:** run `setup/install_pinned.R` to install the pinned package versions. It also lists the optional packages each input path needs (`MatchThem`/`mice`, `WeightIt`, `optmatch`, `haven` for SAS files); install only those you use.

**One-time mapping:** copy `examples/covariate_mapping_template.R` to e.g. `mapping_<partner>.R` and edit the right-hand sides to your variable names and levels. It defines an object called `my_mapping`.

``` r
# 0. Load the functions, check package versions, register the font
source("ecaplots.R")
eca_check_deps()          # errors on missing packages, warns on version drift
eca_setup_font()          # Open Sans: installed, cached, or downloaded once
                          # (see Fonts below); else a fallback + a warning

# 1. Covariate mapping (your edited copy of the template)
source("mapping_<partner>.R")
mapping <- my_mapping
validate_eca_mapping(mapping)

# 2. Love plot
bal    <- as_eca_balance(your_object, mapping = mapping)
p_love <- eca_love_plot(bal)
eca_save_plot(p_love, "love.png", type = "love")

# 3. KM curve
surv <- as_eca_survival(
  your_object,
  time_col = "...", 
  event_col = "...",
  arm_col = "...", 
  control_value = 1,    # value for the RESOLVE trial control arm
  weight_col = "...",
  cluster_col = NULL,   # matching: subclass / match id; IPTW: NULL
  time_unit = "days"    # or "months"; required
  )   
p_km <- eca_km_curve(surv)
eca_save_plot(p_km, "km.png", type = "km")
```

The `examples/` scripts show the exact calls for each kind of input (see [Which example matches you?](#which-example-matches-you)).

## Input conventions

These are not always checked by the code. Getting one wrong can give a plausible-looking but wrong figure.

- **Treatment coding**
  - The treatment / arm variable is 0/1 with **1 = RESOLVE trial control** (the focal group) and 0 = external comparator.
  - The estimand is ATT, and SMDs are standardized by the trial-control SD (`s_d_denom = "treated"`). If you code `1 = ECA`, the balance statistics use the wrong standardization and no error is raised.
  - For `MatchThem` objects, the treatment in your PS formula must follow the same coding.
- **Binary covariates** are summarized as standardized mean differences by default (`binary = "std"` in `as_eca_balance()`). `binary = "raw"` uses raw differences in proportions instead; the x-axis label stays "Absolute standardized mean difference" either way.
- **Weights**
  - ATT weights: every trial-control subject has weight 1.
  - For IPTW-ATT, comparator weights are `ps / (1 - ps)`, with `ps` = `P(trial control | covariates)`.
- **Matching: keep unmatched subjects**
  - For data-frame input, include *all* subjects, giving unmatched patients a weight of **0** (not `NA`).
  - The Love plot's unadjusted SMDs are computed on the full pre-matching sample; dropping unmatched subjects makes the pre-adjustment SMDs wrong with no error.
  - For the KM curve, pass the subclass / match id as `cluster_col`; leave it `NULL` for IPTW-ATT.
- **Multiple imputation**
  - Give the per-imputation rows, stacked long with an imputation id (`imp =` / `imp_col =`), or a `MatchThem` object.
  - Failing to provide an imputation id if not providing a `MatchThem` object will result in the between-imputation variance being ignored and the confidence interval comes out too narrow.
- **Spec covariates not in your PS model** still need to be mapped (assuming they are not completely unobserved in your data) so their balance is shown.
  - With data-frame input, nothing more is needed: balance is assessed on every mapped variable. `MatchThem` objects (`mimids` / `wimids`) only carry the PS-model covariates, so also pass the others to `as_eca_balance()` as `addl = ~ var1 + var2`.
- **Follow-up time** must be in days or months (`time_unit`). The x-axis is in months (30.4375 days/month), shown from 0 to 32.

### Prior cancer treatment (and other yes/no covariates)

Map this concept to match what your PS model used. The template (`examples/covariate_mapping_template.R`) has a worked entry for each case:

| Your data | Mapping entry | Love-plot rows |
|------------------------|------------------------|------------------------|
| One multi-level variable | `list(variable = "prior_tx", levels = c(...))` | one per listed level |
| One yes/no variable (only one modality captured) | `list(variable = "prior_<modality>", levels = c(Yes = "<Modality>"))` | "\<Modality\>" |
| Separate yes/no variable per modality | `list(indicators = c(Surgery = "prior_surg", Radiation = "prior_rad", Chemotherapy = "prior_chemo"))` | one per indicator |

A binary variable always gets one row, because its \|SMD\| is the same for either level. By default that row is its `Yes` / `1` / `TRUE` / Present level. List a single level in `levels` to choose it or relabel it.

For binary Age, Sex and performance status, the code fixes the row: Age **≤65**, Sex **Male**, ECOG **0**, Karnofsky **70-80**. `levels` can relabel these rows but cannot pick the other level. If the fixed level can't be recognized from the level text (e.g. a 0/1 age with no labels), a fallback is used with a warning.

Race rows are always in a fixed order: the default order described below, by display label (alphabetical for text labels, e.g., Asian, Black, White), then any "Other..." level, then any "Unknown"/"Missing" level. `levels` can select and relabel race levels but not reorder them.

Prior cancer treatment rows (one multi-level variable) are also in a fixed order: the default order described below, by display label, then "None", then "Not documented".

For other categorical covariates, rows follow your `levels` entry, else the factor level order, else a default order: numeric levels in numeric order (so `1`, `2`, `>2`; `≤49`, `50-64`, …, `85+`), then other levels alphabetically, then "Unknown"/"Missing"-type levels.

The performance status header names the scale you used: **Baseline ECOG performance status** or **Baseline Karnofsky performance status**. It is detected from the level text (values ≥10, or "Karnofsky"/"KPS", mean Karnofsky; values 0-5, or "ECOG", mean ECOG), then from the variable name. If it can't be detected you get a combined label and a warning; set it explicitly with `ecog_karnofsky = list(variable = "...", scale = "Karnofsky")`.

### Age (and times to index): continuous and/or categorical

Map age to whatever your PS model used: one continuous variable, one categorical variable (binary or more levels), or one of each.

| Your data | Mapping entry | Love-plot rows |
|------------------------|------------------------|------------------------|
| Continuous | `"age"` | **Age** |
| Binary (≤65 / \>65) | `"age_binary"` | **Age**, "≤65" |
| Multi-level (≤49 … 85+) | `list(variable = "age_cat", levels = c(...))` | **Age**, one per level |
| Continuous + categorical | `list(variable = c("age", "age_cat"), continuous_label = "Years")` | **Age**, "Years", one per level |

A numeric `0`/`1` indicator of binary age needs `levels = c("0" = "≤65", "1" = ">65")`, so the ≤65 row can be identified and labelled. The time-to-index concepts accept the same forms.

### Covariates recorded in only one arm

A spec-specified covariate may be recorded in only one arm. The functions work under the assumption these variables were left out of your PS model. You should also not include it in the `addl` argument. If it is entirely `NA`, or entirely an unknown/missing code (by default `"Unknown"`, `"Missing"`, `"Unknown/Missing"`, `"Not documented"`, `"Not reported"`; case-insensitive), in either arm, `as_eca_balance()` drops the variable from the Love plot and throws a warning. Pass `missing_values =` if your data use another code for "not recorded". To silence the warning, map the covariate to `NA`. However, silencing a warning is not necessary to generate the output; only errors will prevent results from being generated.

## Which example matches you? {#which-example-matches-you}

| Your situation | Read |
|------------------------------------|------------------------------------|
| `MatchThem` (`mimids`/`wimids`) | `examples/scenario_matchthem.R` |
| `WeightIt` via `MatchThem::weightthem()` (`wimids`) | `examples/scenario_weightit.R` |
| You computed PS + IPTW-ATT weights yourself | `examples/scenario_manual_weights.R` |
| SAS analysis | `examples/scenario_prepooled_and_sas.R` |

**SAS partners:** export one row per subject per imputation (with the weight, the imputation id and, for matching, the match id) and let this script compute balance and pool survival. That is path (a) in the SAS example, which also covers reading `.sas7bdat` formats and SAS missing values.

- For the Love plot you can instead use the SMDs you already computed in SAS, per imputation, with `as_eca_precomputed_balance()` (path (c)). They are plotted as SAS calculated them, so you don't need to change your SAS approach, but they may be standardized differently from partners whose balance is computed here. Covariates completely missing in one arm can't be detected from SMDs, so leave them out of the table.

- Importing a survival curve already pooled in SAS (path (b)) is a last resort, because SAS's weighted KM intervals don't match the robust (for IPTW-ATT) or cluster-robust (for matching), Rubin's-rules-pooled intervals computed here unless you reproduce that method exactly in SAS beforehand.

## Fonts

The figures use Open Sans. `eca_setup_font()` uses it if it is installed. Otherwise it downloads it once from Google Fonts and caches it, so later sessions work offline. If your machine can't reach the internet, install Open Sans (regular and bold) through your system's font manager or IT, then restart R and rerun `eca_setup_font()`. If neither works, a fallback font is used with a warning; tell the coordinating team if you have to submit figures without using Open Sans.

## Output

`eca_save_plot()` writes PNGs with the `ragg` package at the spec size. Don't override these settings:

| Figure    | `type`   | Size (in) | Resolution |
|-----------|----------|-----------|------------|
| Love plot | `"love"` | 10 × 6.38 | 350 dpi    |
| KM curve  | `"km"`   | 10 × 6.67 | 350 dpi    |

Email the two PNGs, one Love plot and one KM curve, to FoCR.

## Functions

| Function | Purpose |
|------------------------------------|------------------------------------|
| `eca_check_deps()` | verify installed vs. pinned package versions |
| `eca_setup_font()` | register Open Sans (or a fallback) for `ragg` and `showtext` |
| `eca_font_family()` | the font family resolved by `eca_setup_font()` (plot default) |
| `eca_covariate_spec()` | the fixed 10-covariate spec order/labels |
| `eca_mapping_template()` / `validate_eca_mapping()` | build & check your name mapping |
| `as_eca_balance()` | any input → canonical balance table (via cobalt) |
| `as_eca_precomputed_balance()` | SMDs computed elsewhere (e.g. SAS) → canonical balance table |
| `eca_love_plot()` | the Love plot |
| `as_eca_survival()` / `as_eca_pooled_survival()` | any input → canonical survival |
| `eca_pool_survival()` | Rubin's-rules pooling across imputations |
| `eca_km_curve()` | the KM curve |
| `eca_save_plot()` | write PNG via `ragg` at spec dimensions |

Documentation for each function can be found in its related header in the `ecaplots.R` file.
