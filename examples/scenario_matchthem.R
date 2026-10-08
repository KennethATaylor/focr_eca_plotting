# =============================================================================
# Scenario 1 - MatchThem (mimids / wimids)
#
# You matched or weighted WITHIN each of 5 imputations using MatchThem, e.g.
# matching (mimids) or IPTW-ATT via WeightIt+MatchThem (wimids).
# Both figures come straight off the object.
#
# NOT runnable as-is: a template referencing objects from YOUR analysis:
#   `matchthem_obj`.
# Run from eca_standard_plots/ (or adjust the source() path), substitute your
# objects, and edit the column names.
# =============================================================================

source("ecaplots.R")
eca_check_deps()        # warns if your package versions drift from the pins
eca_setup_font() # Open Sans (installed, cached, or downloaded once), else a fallback + warning

# --- your existing analysis produces `matchthem_obj` (class mimids/wimids) ----
# library(MatchThem)
# matchthem_obj <- matchthem(arm ~ age + sex + race + ethnicity +
#                              time_dx_to_index + time_stageiv_to_index +
#                              ecog_karnofsky + prior_cancer_treatment,
#                            datasets = imp_data, approach = "within",
#                            estimand = "ATT", method = "<your method>")
# (any MatchIt method; or weightthem() for IPTW-ATT)

# ---------------------------------------------------------------------------
# LOVE PLOT
# ---------------------------------------------------------------------------
source("mapping_<partner>.R")   # your copy of examples/covariate_mapping_template.R
mapping <- my_mapping
validate_eca_mapping(mapping)

# A MatchThem object only carries the PS-model covariates. If some spec
# covariates were NOT in your PS model (here, met_sites and liver_met), still
# show their balance by passing them to cobalt as additional variables.
# Anything in `...` flows through to cobalt::bal.tab(). Drop `addl` if every
# spec covariate was in the model.
bal <- as_eca_balance(
  matchthem_obj,
  mapping = mapping,
  addl    = ~ met_sites + liver_met
)

p_love <- eca_love_plot(bal)
eca_save_plot(p_love, "love_matchthem.png", type = "love")

# ---------------------------------------------------------------------------
# KM CURVE
# ---------------------------------------------------------------------------
# `arm` here is 1 = RESOLVE trial control, 0 = external comparator; time is in
# days and, for matching, the subclass provides the cluster for robust SEs.
surv <- as_eca_survival(
  matchthem_obj,
  time_col      = "os_time",
  event_col     = "os_event",
  arm_col       = "arm",
  control_value = 1,
  weight_col    = "weights",     # MatchThem writes matching/IPTW weights here
  cluster_col   = "subclass",    # present for matching; NULL for IPTW
  time_unit     = "days"
)

p_km <- eca_km_curve(surv)       # pools across imputations with Rubin's rules
eca_save_plot(p_km, "km_matchthem.png", type = "km")
