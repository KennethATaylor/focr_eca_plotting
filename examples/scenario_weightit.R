# =============================================================================
# Scenario 2 - WeightIt via MatchThem (wimids)
#
# You weighted WITHIN each of the 5 imputations with MatchThem::weightthem(),
# which calls WeightIt, giving a `wimids` object. The calls are the same as
# Scenario 1, because cobalt and MatchThem::complete() both understand wimids
# objects; only the cluster id differs (none for IPTW).
#
# NOT runnable as-is: a template referencing objects from YOUR analysis:
#   `w_imp`.
# Run from eca_standard_plots/ (or adjust the source() path), substitute your
# objects, and edit the column names.
# =============================================================================

source("ecaplots.R")
eca_check_deps()
eca_setup_font()
source("mapping_<partner>.R") # your copy of examples/covariate_mapping_template.R
mapping <- my_mapping
validate_eca_mapping(mapping)

# --- your existing analysis produces `w_imp` (class wimids) ------------------
# library(MatchThem)
# w_imp <- weightthem(arm ~ age + sex + race + ethnicity +
#                       time_dx_to_index + time_stageiv_to_index +
#                       ecog_karnofsky + prior_cancer_treatment,
#                     datasets = imp_data, approach = "within",
#                     estimand = "ATT", method = "<your method>")
# (e.g. "glm", "ebal", "energy")

# ---------------------------------------------------------------------------
# LOVE PLOT
# ---------------------------------------------------------------------------
# A wimids object only carries the PS-model covariates, so pass spec
# covariates that were not in the model (here, met_sites and liver_met) via
# `addl`. Drop `addl` if every spec covariate was in the model.
bal <- as_eca_balance(
  w_imp,
  mapping = mapping,
  addl = ~ met_sites + liver_met
)
p_love <- eca_love_plot(bal)
eca_save_plot(p_love, "love_weightit.png", type = "love")

# ---------------------------------------------------------------------------
# KM CURVE
# ---------------------------------------------------------------------------
surv <- as_eca_survival(
  w_imp,
  time_col = "os_time",
  event_col = "os_event",
  arm_col = "arm",
  control_value = 1,
  weight_col = "weights", # MatchThem writes the weights here
  cluster_col = NULL, # IPTW: robust SEs come from the weights alone
  time_unit = "days"
)
p_km <- eca_km_curve(surv) # pools across imputations with Rubin's rules
eca_save_plot(p_km, "km_weightit.png", type = "km")
