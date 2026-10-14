# Changelog

## 2026-10-14

### New

- **Love plot from SAS-computed SMDs: `as_eca_precomputed_balance(df, mapping)`.** SAS partners can build the Love plot from the standardized mean differences SAS already produced, without changing how SAS calculates them. `df` has one row per variable (and level) per imputation, with columns `variable`, `level`, `unadjusted`, `adjusted` and optional `imp`. Absolute values are taken, and with imputations each row shows the mean and the min–max range, as in the other routes. The result goes to `eca_love_plot()` like any other balance object, so row order, labels and styling are the same. See path (c) in `examples/scenario_prepooled_and_sas.R`.
  - SMDs are plotted as supplied, so they may be standardized differently from SMDs computed here with cobalt.
  - Covariates completely missing in one arm can't be detected from SMDs; leave them out of `df`.
  - For a binary variable, give both levels or only one. If only one, list both levels in the mapping's `levels` entry so the spec level (Age ≤65, Sex Male, ECOG 0) can be named. Otherwise the supplied level is shown, with a warning if it isn't the spec level.
  - A 0/1 or categorical version of age or a time to index needs a `level` on each row.
  - Errors on missing columns, missing SMDs, duplicate rows, rows absent from some imputations, categorical rows without a level, and different variable/level pairs that produce the same row name.
- **`binary` argument on `as_eca_balance()`.** `binary = "std"` (default) summarizes binary covariates as standardized mean differences; `binary = "raw"` uses raw differences in proportions. The x-axis label stays "Absolute standardized mean difference" either way.

### Changed

- **Default order of category levels.** When a categorical covariate has no `levels` entry in the mapping and isn't a factor, its rows are now ordered: numeric levels in numeric order (`1`, `2`, `>2`; `≤49`, `50-64`, …, `85+`), then other levels alphabetically, then Unknown/Missing-type levels. Previously the order was purely alphabetical, which put `>2` before `1`. A `levels` entry or factor level order still takes precedence.
- **Prior cancer treatment level order.** For one multi-level variable, rows are now shown in the default order, then "None", then "Not documented", whatever order `levels` lists them in (as Race already works).
- **Race level order.** The levels before "Other" and "Unknown"/"Missing" now follow the default order above (unchanged for text labels).
- **Warnings.** The "mean difference above 5" warning says whether the values came from cobalt, from `binary = "raw"`, or from precomputed SMDs. cobalt's warning about mixing standardized and raw differences in one plot is no longer shown when `binary = "raw"` (the axis label is fixed by the spec).

### Fixed

- **Love plot labels on ggplot2 4.x.** The y-axis labels printed their formatting codes literally (`**Age**`, `&nbsp;`) instead of bold headers and indented levels. They now render correctly on ggplot2 version >=4.x (for any providers who cannot revert to older versions) as well as the pinned 3.5.1.

### Documentation

- `README.md`: `binary` argument; the SAS-computed-SMD option for SAS partners; the level-ordering rules, including Prior cancer treatment; `as_eca_precomputed_balance()` added to the Functions table.
- `examples/scenario_prepooled_and_sas.R`: new path (c), with a skeleton table and call, and cobalt's SMD formula for reference only (matching it is not currently required). The old note that the Love plot always needs subject-level data is removed.
- `examples/covariate_mapping_template.R`: notes on the fixed Race and Prior cancer treatment order; the Prior cancer treatment example uses "Not documented" instead of "Unknown".
