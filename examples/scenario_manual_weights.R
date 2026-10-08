# =============================================================================
# Scenario 3 - Hand-computed propensity scores + IPTW-ATT weights
#
# You fit the PS model yourself and formed ATT weights by hand, where
# ps = P(RESOLVE trial control | covariates):
#   ATT weight = 1 for the focal (trial control) group;
#              = ps / (1 - ps) for the comparator group.
# You have ONE long data frame with all 5 imputations stacked, carrying the
# per-row weight and an imputation id. Everything runs through the data.frame
# paths; cobalt still does the standardized balance computation and pools across
# imputations via the `imp` argument. The data.frame path assesses balance on
# every mapped covariate, so no `addl` is needed for covariates outside the PS
# model.
#
# NOT runnable as-is: a template referencing objects from YOUR analysis:
#   `stacked`.
# Run from eca_standard_plots/ (or adjust the source() path), substitute your
# objects, and edit the column names.
# =============================================================================

source("ecaplots.R")
eca_check_deps()
eca_setup_font()
source("mapping_<partner>.R")   # your copy of examples/covariate_mapping_template.R
mapping <- my_mapping
validate_eca_mapping(mapping)

# `stacked` columns (example):
#   imp_id, arm (1 = trial control, 0 = comparator), att_weight,
#   age, sex, race, ethnicity, time_dx_to_index, time_stageiv_to_index,
#   ecog_karnofsky, prior_cancer_treatment, met_sites, liver_met,
#   time (days), event (0/1)

# ---------------------------------------------------------------------------
# LOVE PLOT -- cobalt pools across imputations because we pass `imp` argument
# ---------------------------------------------------------------------------
bal <- as_eca_balance(
  stacked,
  mapping = mapping,
  treat = "arm",
  weights = "att_weight",
  imp = "imp_id"
)
p_love <- eca_love_plot(bal)
eca_save_plot(p_love, "love_manual.png", type = "love")

# ---------------------------------------------------------------------------
# KM CURVE -- Rubin's-rules pooling across the 5 imputations
# ---------------------------------------------------------------------------
surv <- as_eca_survival(
  stacked,
  time_col = "time",
  event_col = "event",
  arm_col = "arm",
  control_value = 1,
  weight_col = "att_weight",
  imp_col = "imp_id",
  cluster_col = NULL, # IPTW: no matching subclass
  time_unit = "days"
)
p_km <- eca_km_curve(surv)
eca_save_plot(p_km, "km_manual.png", type = "km")
