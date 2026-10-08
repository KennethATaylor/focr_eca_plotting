# -----------------------------------------------------------------------------
# covariate_mapping_template.R
#
# Copy this file, rename it (e.g. mapping_<partner>.R), and edit the right-hand
# side of each entry to match YOUR variable names. The left-hand keys are fixed
# canonical concepts and must not change. This mapping is what makes every
# partner's Love plot show the same ten spec covariates (when applicable), in the same order, with
# the same labels -- no matter what the underlying variables are called.
#
# Each entry is either:
#   * a string             -> your variable name; or
#   * list(variable = "<your var>",
#          levels   = c("<raw level>" = "<Display label>", ...))
#     -> use `levels` to choose which category rows appear, in what order, and
#        how they are labeled. Omit `levels` to show all levels as-is.
#        A yes/no variable shows ONE row (its SMD is the same for either
#        level); by default the Yes / 1 / TRUE / Present level. List only
#        the level you want to choose or relabel it, e.g. c("Yes" = "<Modality>").
#   * list(indicators = c("<Display label>" = "<your var>", ...))
#     -> a categorical concept recorded as several yes/no variables (see
#        prior_cancer_treatment below).
#
# Leave an entry as NA only if you genuinely do not have that covariate. The spec
# asks for all ten; if a covariate was NOT in your PS model, still map it so
# its balance is shown. With a data frame input that is all you need (balance
# is assessed on every mapped covariate). With a MatchThem (mimids / wimids)
# object, which only carries the PS-model covariates, also pass it to
# cobalt as an additional (`addl`) variable (see examples/scenario_matchthem.R).
#
# Exception: a covariate recorded in only ONE arm. Leave it out of the PS
# model and out of `addl`. as_eca_balance() detects a covariate that is
# entirely NA, or entirely an unknown/missing code (see its `missing_values`
# argument), in either arm, and drops it from the Love plot with a warning.
# Map it to NA instead if you want no warning.
#
# Variable names and levels below are ILLUSTRATIVE placeholders only; replace
# them with your own variable names and the levels in your own data.
# -----------------------------------------------------------------------------

my_mapping <- list(
  # Age -- match what your PS model used:
  #   continuous only:          "age"
  #   categorical only:         "age_binary"   (e.g. <=65 / >65; one "<=65" row)
  #                             list(variable = "age_cat",
  #                                  levels = c("<=49" = "<=49", "50-64" = "50-64",
  #                                             "65-74" = "65-74", "75-84" = "75-84",
  #                                             "85+" = "85+"))
  #   continuous + categorical: list(variable = c("age", "age_cat"),
  #                                  continuous_label = "Continuous")
  # A numeric binary age variable (if coded as 0/1) needs levels = c("0" = "<=65", "1" = ">65") so
  # the spec's <=65 row can be identified and labelled.
  # At most one continuous and one categorical variable per concept; the two
  # time-to-index concepts below accept the same forms.
  age = "age",

  sex = list(
    variable = "sex",
    levels = c(
      "Female" = "Female",
      "Male" = "Male"
    )
  ),

  ethnicity = list(
    variable = "ethnicity",
    levels = c(
      "Hispanic" = "Hispanic",
      "Not Hispanic" = "Not Hispanic",
      "Unknown" = "Unknown"
    )
  ),

  # Race rows are always shown alphabetically, with "Other..." then
  # "Unknown"/"Missing" last, whatever order `levels` lists them in.
  race = list(
    variable = "race",
    levels = c(
      "Asian" = "Asian",
      "Black" = "Black",
      "White" = "White",
      "Other" = "Other",
      "Unknown" = "Unknown"
    )
  ),

  time_dx_to_index = "time_dx_to_index",

  time_stageiv_to_index = "time_stageiv_to_index",

  met_sites = list(
    variable = "met_sites",
    levels = c(
      "1" = "1",
      "2" = "2",
      "3+" = "3+",
      "Unknown" = "Unknown"
    )
  ),

  liver_met = list(
    variable = "liver_met",
    levels = c(
      "Yes" = "Yes",
      "No" = "No",
      "Unknown" = "Unknown"
    )
  ),

  # The header reads "Baseline ECOG ..." or "Baseline Karnofsky performance
  # status", detected from the levels; add scale = "ECOG" / "Karnofsky" if
  # the levels don't make it clear (you'll get a warning).
  ecog_karnofsky = list(
    variable = "ecog_karnofsky",
    levels = c(
      "0" = "0",
      "1" = "1",
      "2+" = "2+",
      "Unknown" = "Unknown"
    )
  ),

  # Prior cancer treatment -- pick the form that matches what your PS model
  # used (keep exactly one):
  #   (a) ONE multi-level variable (e.g. None / Surgery / Radiation / ...):
  #       the list below; `levels` picks, orders and relabels the rows.
  #   (b) ONE yes/no variable, because only one modality was captured:
  #         list(variable = "prior_<modality>",
  #              levels   = c("Yes" = "<Modality>"))
  #       Listing only "Yes" shows that level's row, with your label.
  #   (c) SEPARATE yes/no variables, one per modality:
  #         list(indicators = c("Surgery"      = "prior_surgery",
  #                             "Radiation"    = "prior_radiation",
  #                             "Chemotherapy" = "prior_chemotherapy"))
  #       One row per indicator (its Yes / 1 / TRUE level), labelled and
  #       ordered as given. Values may be Yes/No, 0/1 or TRUE/FALSE.
  # Omit modalities you don't have; don't add ones that aren't captured.
  # `indicators` works for any categorical concept, not just this one.
  prior_cancer_treatment = list(
    variable = "prior_cancer_treatment",
    levels = c(
      "Surgery" = "Surgery",
      "Radiation" = "Radiation",
      "Systemic therapy" = "Systemic therapy",
      "None" = "None",
      "Unknown" = "Unknown"
    )
  )
)

# Use it from your analysis script (keep the object name `my_mapping`):
#   source("ecaplots.R")
#   source("mapping_<partner>.R")
#   mapping <- my_mapping
#   validate_eca_mapping(mapping)
