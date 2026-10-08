# =============================================================================
# Scenario 4 - SAS analyses
#
# Two sub-cases, in order of preference:
#   (a) PREFERRED: you did matching/weighting within each of the 5
#       imputations in SAS and exported one row per subject per imputation.
#       Load into R and use the SAME data.frame paths as Scenario 3, so
#       balance and Rubin's-rules pooling are computed exactly as for every
#       other partner. Do NOT collapse the imputations into one dataset (e.g.
#       averaging imputed values): that ignores between-imputation variance
#       and the confidence band comes out too narrow.
#   (b) LAST RESORT: survival already pooled in SAS. Only if subject-level
#       export is impossible. See the requirements in section (b).
#
# NOT runnable as-is: a template referencing objects/files from YOUR analysis:
#   "eca_subject_level.sas7bdat", "formats.sas7bcat", `pooled_tbl`.
# Run from eca_standard_plots/ (or adjust the source() path), substitute your
# objects, and edit the column names. Reading .sas7bdat needs `haven`
# (installed by setup/install_pinned.R).
# =============================================================================

source("ecaplots.R")
eca_check_deps()
eca_setup_font()
source("mapping_<partner>.R")   # your copy of examples/covariate_mapping_template.R
mapping <- my_mapping
validate_eca_mapping(mapping)

# ---------------------------------------------------------------------------
# (a) SAS per-subject export -> read with haven, then data.frame paths
# ---------------------------------------------------------------------------
# Required columns, one row per subject per imputation:
#   imputation id   e.g. `_Imputation_` from PROC MI
#   arm             numeric 1 = RESOLVE trial control, 0 = external comparator
#   weight          ATT weight: 1 for every trial-control subject
#   match id        matching only (e.g. `_MatchID` from PROC PSMATCH)
#   the mapped spec covariates, follow-up time, event (0/1)
#
# Matching (PROC PSMATCH): export ALL subjects (OUTPUT OUT(OBS=ALL)=...), not
# just the matched ones. The Love plot's "unadjusted" SMDs are computed on the
# full pre-matching sample, so dropping unmatched subjects makes them wrong
# without any error. Unmatched subjects must have weight 0, not missing.

dat <- haven::read_sas(
  "eca_subject_level.sas7bdat",
  catalog_file = "formats.sas7bcat"   # your format catalog, if you have one
)
# SAS categorical covariates are often numeric codes with a format
# (e.g. sex = 1/2). as_factor() applies the value labels, so the Love plot
# shows "Male"/"Female" rather than 1/2 and can find the spec rows
# (Age <=65, Sex Male, ECOG 0). Without a catalog, recode by hand.
# Leave `arm`, the event and the imputation id numeric: as_factor() only
# converts labelled columns, so give those columns no value labels.
dat <- haven::as_factor(dat)
dat <- as.data.frame(dat)

# Missing SAS character values arrive as "" rather than NA. Convert them so
# they aren't plotted as a level.
chr <- vapply(dat, is.character, logical(1))
dat[chr] <- lapply(dat[chr], function(v) replace(v, trimws(v) == "", NA))

# Matching only: depending on the PSMATCH method and how you exported the
# data, unmatched subjects get a weight of 0 or missing. Check, and set any
# missing weights to 0 (missing weights stop both figures with an error).
# dat$`_MATCHWGT_`[is.na(dat$`_MATCHWGT_`)] <- 0

# If you exported CSV instead, SAS writes missing numbers as "." -- read with
# the line below. check.names = FALSE keeps `_Imputation_` and `_MATCHWGT_`
# from being renamed `X_Imputation_` / `X_MATCHWGT_`.
# dat <- read.csv("eca_subject_level.csv", na.strings = c("", "."),
#                 check.names = FALSE)

bal <- as_eca_balance(
  dat,
  mapping = mapping,
  treat   = "arm",
  weights = "att_weight",           # or "_MATCHWGT_" for PSMATCH
  imp     = "_Imputation_"
)
eca_save_plot(eca_love_plot(bal), "love_sas.png", type = "love")

surv <- as_eca_survival(
  dat,
  time_col      = "time",
  event_col     = "event",
  arm_col       = "arm",
  control_value = 1,
  weight_col    = "att_weight",     # or "_MATCHWGT_"
  imp_col       = "_Imputation_",
  cluster_col   = NULL,             # matching: "_MatchID"; IPTW: NULL
  time_unit     = "days"
)
eca_save_plot(eca_km_curve(surv), "km_sas.png", type = "km")

# ---------------------------------------------------------------------------
# (b) LAST RESORT: survival ALREADY pooled in SAS
# ---------------------------------------------------------------------------
# Use only if subject-level rows cannot leave your environment. Your pooled
# curve must match what the script computes for everyone else:
#   * per imputation and arm, a weighted KM with a ROBUST (sandwich) SE,
#     clustered on the match id for matching. PROC LIFETEST's WEIGHT
#     statement does not give a robust SE, so its intervals will be narrower;
#   * pooled across imputations with Rubin's rules on the complementary
#     log-log scale, Q = log(-log S), Var(Q) = (SE(S) / (S * log S))^2,
#     total variance = mean within + (1 + 1/m) * between, then
#     back-transformed (S = exp(-exp(Q)); the CI ends swap);
#   * the trial control arm unweighted (weight 1).
#
# `pooled_tbl` must have columns arm, time, surv, lower, upper, with survival
# on the 0-1 scale (not percent). From a PROC LIFETEST OUTSURV= layout:
# pooled_tbl <- data.frame(
#   arm   = ifelse(sas_out$arm == 1, "RESOLVE Trial Control Arm",
#                  "External Comparator Arm"),
#   time  = sas_out$time,          # your time variable's name
#   surv  = sas_out$SURVIVAL,
#   lower = sas_out$SDF_LCL,
#   upper = sas_out$SDF_UCL
# )
# R column names are case-sensitive: check names(sas_out) first. If time 0
# has a missing CI, set lower = upper = 1 there so the band starts at 100%.
#
# `control_arm` names the RESOLVE arm value in `pooled_tbl$arm` (drawn black);
# time_unit is the unit `time` is stored in.
# pooled <- as_eca_pooled_survival(pooled_tbl,
#                                  control_arm = "RESOLVE Trial Control Arm",
#                                  time_unit = "days")
# eca_save_plot(eca_km_curve(pooled), "km_sas_pooled.png", type = "km")

# NOTE: the Love plot always needs subject-level covariates to compute balance;
# there is no "pre-pooled balance" shortcut. Use path (a) for the Love plot
# even if survival came pre-pooled.
