# Why this script is here
# I examine selected clinical sensitivities and historical reconciliation calculations.
# The older net-savings calculation is not the current economic model.

# 07_sensitivity_analyses.R
# AAS CTA reanalysis, stage 8: sensitivity analyses and historical reconciliation.

source("R/06_primary_models.R")

# ---- Yield and model helpers --------------------------------------------------
# I use shared functions to calculate yield comparisons and refit the specified model in sensitivity cohorts.

# AAS yield in concordant versus discordant encounters with known outcome:
# risk difference with an unpooled Wald interval, and a Pearson chi-square P
# value that switches to Fisher's exact test when any expected count is below 5.
calculate_yield_comparison <- function(data, analysis_label) {
  keep <- data$algorithm_class %in% c("Concordant", "Discordant") & !is.na(data$aas_positive)
  d <- data[keep, , drop = FALSE]

  concordant <- d$algorithm_class == "Concordant"
  discordant <- d$algorithm_class == "Discordant"

  c_pos <- sum(d$aas_positive[concordant])
  c_den <- sum(concordant)
  d_pos <- sum(d$aas_positive[discordant])
  d_den <- sum(discordant)

  c_rate <- c_pos / c_den
  d_rate <- d_pos / d_den
  difference <- c_rate - d_rate
  standard_error <- sqrt(c_rate * (1 - c_rate) / c_den + d_rate * (1 - d_rate) / d_den)

  comparison_table <- matrix(
    c(c_pos, c_den - c_pos, d_pos, d_den - d_pos),
    nrow = 2L,
    byrow = TRUE
  )

  expected <- suppressWarnings(
    stats::chisq.test(comparison_table, correct = FALSE)$expected
  )

  p_value <- if (any(expected < 5)) {
    stats::fisher.test(comparison_table)$p.value
  } else {
    stats::chisq.test(comparison_table, correct = FALSE)$p.value
  }

  data.frame(
    analysis = analysis_label,
    cohort_n = nrow(data),
    concordant_positive = c_pos,
    concordant_denominator = c_den,
    concordant_percent = 100 * c_rate,
    discordant_positive = d_pos,
    discordant_denominator = d_den,
    discordant_percent = 100 * d_rate,
    absolute_difference_percentage_points = 100 * difference,
    ci_low_percentage_points = 100 * (difference - 1.96 * standard_error),
    ci_high_percentage_points = 100 * (difference + 1.96 * standard_error),
    p_value = p_value,
    stringsAsFactors = FALSE
  )
}

prepare_sensitivity_model_data <- function(data) {
  d <- data[
    data$algorithm_class %in% c("Concordant", "Discordant") &
      data$provider_group %in% c("Physician", "Non-physician"), , drop = FALSE
  ]

  d$age_per_10_years <- d$age / 10
  d$sex_model <- factor(d$sex, levels = c("Male", "Female"))
  d$esi_model <- factor(d$esi_group, levels = c("ESI 1-2", "ESI 3-4"))
  d$provider_model <- factor(d$provider_group, levels = c("Physician", "Non-physician"))
  d$priority_model <- factor(d$order_priority, levels = c("STAT", "Non-STAT"))
  d$site_model <- factor(
    d$site,
    levels = c("Hospital D", "Hospital A", "Hospital B", "Hospital C")
  )

  variables <- c(
    "algorithm_discordant",
    "age_per_10_years",
    "sex_model",
    "esi_model",
    "provider_model",
    "priority_model",
    "site_model"
  )

  d[stats::complete.cases(d[variables]), , drop = FALSE]
}

# Refits the primary model specification in a sensitivity cohort.
fit_sensitivity_model <- function(data, analysis_label) {
  d <- prepare_sensitivity_model_data(data)

  model <- stats::glm(
    algorithm_discordant ~
      age_per_10_years + sex_model + esi_model + provider_model + priority_model + site_model,
    family = stats::binomial(),
    data = d
  )

  estimates <- tidy_logistic_model(model)
  estimates$comparison <- unname(term_labels[estimates$term])
  estimates$analysis <- analysis_label
  estimates$analytic_n <- nrow(d)
  estimates$discordant_events <- sum(d$algorithm_discordant)
  estimates$concordant_events <- sum(!d$algorithm_discordant)
  estimates
}

# ---- Component orders within 60 minutes ---------------------------------------
# I restrict to the timing flag to see how findings compare with the primary cohort.

timing_data <- analysis_data[analysis_data$within_60_minutes, , drop = FALSE]

if (nrow(timing_data) != 1173L) {
  stop("The <=60-minute adult sensitivity cohort is not 1,173.", call. = FALSE)
}

timing_yield <- calculate_yield_comparison(timing_data, "CTA component orders <=60 minutes")
timing_model_estimates <- fit_sensitivity_model(timing_data, "CTA component orders <=60 minutes")

# ---- First included encounter per patient -------------------------------------
# I retain each patient’s first included encounter to examine the effect of repeat encounters.

patient_order <- order(analysis_data$patient_empi, analysis_data$primary_order_datetime)

ordered_patient_data <- analysis_data[patient_order, , drop = FALSE]
first_encounter_data <- ordered_patient_data[
  !duplicated(ordered_patient_data$patient_empi), , drop = FALSE
]

if (nrow(first_encounter_data) != 1120L) {
  stop("First-encounter sensitivity cohort is not 1,120 patients.", call. = FALSE)
}

first_encounter_yield <- calculate_yield_comparison(
  first_encounter_data,
  "First included encounter per patient"
)

first_encounter_model_estimates <- fit_sensitivity_model(
  first_encounter_data,
  "First included encounter per patient"
)

# ---- ADD-RS requiring all three components ------------------------------------
# I require complete documentation of the score components for this sensitivity analysis.

strict_add_rs_data <- analysis_data

strict_add_rs_data$algorithm_class <- classify_algorithm(
  strict_add_rs_data$add_rs_strict_complete,
  strict_add_rs_data$d_dimer_status
)
strict_add_rs_data$algorithm_discordant <- discordant_indicator(
  strict_add_rs_data$algorithm_class
)

strict_add_rs_yield <- calculate_yield_comparison(
  strict_add_rs_data,
  "Require all three ADD-RS components"
)

# ---- Absent D-dimer treated as unclassifiable ---------------------------------
# I change how absent tests are classified in this sensitivity analysis.

observed_d_dimer_data <- analysis_data

observed_d_dimer_data$algorithm_class[
  observed_d_dimer_data$add_rs <= 1L &
    observed_d_dimer_data$d_dimer_status == "Absent/not obtained"
] <- "Unclassifiable"

observed_d_dimer_data$algorithm_discordant <- discordant_indicator(
  observed_d_dimer_data$algorithm_class
)

observed_d_dimer_yield <- calculate_yield_comparison(
  observed_d_dimer_data,
  "Absent D-dimer treated as unclassifiable"
)

# ---- Historical behavior: unknown AAS outcome forced negative -----------------
# I reproduce the historical handling of unknown outcomes for comparison with the primary analysis.

historical_outcome_data <- analysis_data
historical_outcome_data$aas_positive[is.na(historical_outcome_data$aas_positive)] <- 0L

historical_outcome_yield <- calculate_yield_comparison(
  historical_outcome_data,
  "Unknown AAS abstraction forced negative (historical behavior)"
)

yield_sensitivity_results <- rbind(
  calculate_yield_comparison(analysis_data, "Primary"),
  timing_yield,
  first_encounter_yield,
  strict_add_rs_yield,
  observed_d_dimer_yield,
  historical_outcome_yield
)

model_sensitivity_results <- rbind(timing_model_estimates, first_encounter_model_estimates)

# ---- Attending versus APP with residents excluded -----------------------------
# I compare the specified provider groups after excluding residents.

resident_excluded <- analysis_data[
  analysis_data$provider_role %in% c("Attending physician", "APP") &
    !is.na(analysis_data$aas_positive), , drop = FALSE
]

resident_excluded_table <- table(
  factor(resident_excluded$provider_role, levels = c("Attending physician", "APP")),
  factor(resident_excluded$aas_positive, levels = c(0L, 1L))
)

resident_excluded_p <- chisq_p(resident_excluded_table)

resident_excluded_results <- data.frame(
  provider_role = c("Attending physician", "APP"),
  aas_positive_n = resident_excluded_table[, "1"],
  aas_known_denominator = rowSums(resident_excluded_table),
  aas_positive_percent =
    100 * resident_excluded_table[, "1"] / rowSums(resident_excluded_table),
  p_value = resident_excluded_p,
  stringsAsFactors = FALSE
)

# ---- Low-acuity ESI 3-4 subgroup ----------------------------------------------
# I summarize classification and yield within the specified acuity subgroup.

low_acuity <- analysis_data[
  analysis_data$esi_group == "ESI 3-4" &
    analysis_data$algorithm_class %in% c("Concordant", "Discordant"), , drop = FALSE
]

low_acuity_concordance_table <- table(
  factor(low_acuity$provider_group, levels = c("Physician", "Non-physician")),
  factor(low_acuity$algorithm_class, levels = c("Concordant", "Discordant"))
)

low_acuity_concordance_p <- chisq_p(low_acuity_concordance_table)

low_acuity_known_outcome <- low_acuity[!is.na(low_acuity$aas_positive), , drop = FALSE]

low_acuity_yield_table <- table(
  factor(low_acuity_known_outcome$provider_group, levels = c("Physician", "Non-physician")),
  factor(low_acuity_known_outcome$aas_positive, levels = c(0L, 1L))
)

low_acuity_yield_p <- chisq_p(low_acuity_yield_table)

low_acuity_results <- data.frame(
  provider_group = c("Physician", "Non-physician"),
  classifiable_n = rowSums(low_acuity_concordance_table),
  concordant_n = low_acuity_concordance_table[, "Concordant"],
  discordant_n = low_acuity_concordance_table[, "Discordant"],
  aas_positive_n = low_acuity_yield_table[, "1"],
  aas_known_denominator = rowSums(low_acuity_yield_table),
  concordance_p_value = low_acuity_concordance_p,
  yield_p_value = low_acuity_yield_p,
  stringsAsFactors = FALSE
)

# ---- Discordance reason and alternative diagnoses by algorithm class ----------
# I examine recorded discordance reasons and alternative-diagnosis flags across algorithm classes.

discordant_known <- analysis_data[
  analysis_data$algorithm_class == "Discordant" & !is.na(analysis_data$aas_positive), ,
  drop = FALSE
]

discordance_reason_results <- do.call(
  rbind,
  lapply(
    c("Low ADD-RS with absent D-dimer", "Low ADD-RS with negative D-dimer"),
    function(reason) {
      keep <- discordant_known$discordance_reason == reason
      events <- sum(discordant_known$aas_positive[keep])
      denominator <- sum(keep)

      data.frame(
        discordance_reason = reason,
        aas_positive_n = events,
        aas_known_denominator = denominator,
        aas_positive_percent = 100 * events / denominator,
        stringsAsFactors = FALSE
      )
    }
  )
)

alternative_analysis <- analysis_data[
  analysis_data$aas_outcome == "Negative" &
    analysis_data$algorithm_class %in% c("Concordant", "Discordant"), , drop = FALSE
]

alternative_table <- table(
  factor(alternative_analysis$algorithm_class, levels = c("Concordant", "Discordant")),
  factor(alternative_analysis$alternative_diagnosis_definite, levels = c(0L, 1L))
)

alternative_p <- chisq_p(alternative_table)

alternative_diagnosis_by_algorithm <- data.frame(
  algorithm_class = c("Concordant", "Discordant"),
  alternative_diagnosis_n = alternative_table[, "1"],
  aas_negative_denominator = rowSums(alternative_table),
  alternative_diagnosis_percent = 100 * alternative_table[, "1"] / rowSums(alternative_table),
  p_value = alternative_p,
  stringsAsFactors = FALSE
)

# ---- Historical net-savings model (reconciliation only) -----------------------
# I keep this earlier calculation for reconciliation. The current economic analyses are in the later scripts.
# Reproduces the earlier model for comparison with prior results. The current
# manuscript uses the cost-consequence analyses in R/14, R/15 and R/16 with the
# shared economic_cost_consequence_engine.R.

economic_net_savings <- function(
  discordant_n,
  aas_prevalence,
  d_dimer_sensitivity,
  d_dimer_specificity,
  cta_cost = 595,
  d_dimer_cost = 25,
  missed_aas_cost = 81660
) {
  negative_rate <-
    d_dimer_specificity * (1 - aas_prevalence) +
    (1 - d_dimer_sensitivity) * aas_prevalence

  deferred_cta <- discordant_n * negative_rate
  expected_missed_aas <- discordant_n * aas_prevalence * (1 - d_dimer_sensitivity)

  list(
    projected_d_dimer_negative_rate = negative_rate,
    projected_deferred_cta = deferred_cta,
    expected_missed_aas = expected_missed_aas,
    avoided_cta_cost = deferred_cta * cta_cost,
    d_dimer_testing_cost = discordant_n * d_dimer_cost,
    expected_missed_aas_cost = expected_missed_aas * missed_aas_cost,
    net_savings =
      deferred_cta * cta_cost -
      discordant_n * d_dimer_cost -
      expected_missed_aas * missed_aas_cost
  )
}

economic_discordant_n <- sum(analysis_data$algorithm_class == "Discordant")
economic_discordant_prevalence <- sum(discordant_known$aas_positive) / nrow(discordant_known)
full_known_aas_prevalence <-
  sum(analysis_data$aas_positive, na.rm = TRUE) / sum(!is.na(analysis_data$aas_positive))

# Base case: Asha and Miers D-dimer performance.
base_args <- list(
  discordant_n = economic_discordant_n,
  aas_prevalence = economic_discordant_prevalence,
  d_dimer_sensitivity = 0.98,
  d_dimer_specificity = 0.419,
  cta_cost = 595,
  d_dimer_cost = 25,
  missed_aas_cost = 81660
)

economic_base <- do.call(economic_net_savings, base_args)

economic_watanabe <- do.call(
  economic_net_savings,
  utils::modifyList(base_args, list(d_dimer_sensitivity = 0.952, d_dimer_specificity = 0.604))
)

economic_inputs <- data.frame(
  input = c(
    "Eligible encounters",
    "Algorithm-discordant encounters",
    "Known AAS prevalence among discordant encounters",
    "CTA standardized price",
    "D-dimer test cost",
    "Missed-AAS cost",
    "Asha and Miers sensitivity",
    "Asha and Miers specificity",
    "Watanabe sensitivity",
    "Watanabe specificity"
  ),
  value = c(
    nrow(analysis_data),
    economic_discordant_n,
    economic_discordant_prevalence,
    595,
    25,
    81660,
    0.98,
    0.419,
    0.952,
    0.604
  ),
  source = c(
    "R/03_cohort_construction.R",
    "R/04_variable_derivation.R",
    "R/07_sensitivity_analyses.R",
    rep("Historical manuscript assumption", 3L),
    rep("Historical manuscript/meta-analysis input", 4L)
  ),
  stringsAsFactors = FALSE
)

economic_base_results <- data.frame(
  measure = c(
    "Total standardized gross imaging expenditure",
    "Discordant standardized gross imaging expenditure",
    "Projected D-dimer-negative rate",
    "Projected deferred CTAs",
    "Expected missed AAS cases",
    "Avoided CTA cost",
    "D-dimer testing cost",
    "Expected missed-AAS cost",
    "Modeled net savings"
  ),
  value = c(
    nrow(analysis_data) * 595,
    economic_discordant_n * 595,
    economic_base$projected_d_dimer_negative_rate,
    economic_base$projected_deferred_cta,
    economic_base$expected_missed_aas,
    economic_base$avoided_cta_cost,
    economic_base$d_dimer_testing_cost,
    economic_base$expected_missed_aas_cost,
    economic_base$net_savings
  ),
  generating_object = "economic_base_results",
  stringsAsFactors = FALSE
)

# One-way sensitivity: net savings with one base-case input moved to each bound.
one_way_values <- function(parameter, lower_input, upper_input) {
  net_savings <- function(inputs) {
    do.call(economic_net_savings, utils::modifyList(base_args, inputs))$net_savings
  }

  lower <- net_savings(lower_input)
  base <- net_savings(list())
  upper <- net_savings(upper_input)

  data.frame(
    parameter = parameter,
    net_savings_lower_input = lower,
    net_savings_base = base,
    net_savings_upper_input = upper,
    net_savings_min = min(lower, upper),
    net_savings_max = max(lower, upper),
    sensitivity_range = abs(upper - lower),
    stringsAsFactors = FALSE
  )
}

table5_source <- rbind(
  one_way_values("CTA cost", list(cta_cost = 476), list(cta_cost = 714)),
  one_way_values(
    "Cohort AAS prevalence",
    list(aas_prevalence = 0.037),
    list(aas_prevalence = full_known_aas_prevalence)
  ),
  one_way_values(
    "Missed-AAS cost",
    list(missed_aas_cost = 59280),
    list(missed_aas_cost = 116370)
  ),
  data.frame(
    parameter = "D-dimer source",
    net_savings_lower_input = economic_watanabe$net_savings,
    net_savings_base = economic_base$net_savings,
    net_savings_upper_input = NA_real_,
    net_savings_min = min(economic_watanabe$net_savings, economic_base$net_savings),
    net_savings_max = max(economic_watanabe$net_savings, economic_base$net_savings),
    sensitivity_range = abs(economic_base$net_savings - economic_watanabe$net_savings),
    stringsAsFactors = FALSE
  )
)

table5_source$lower_input_value <- c(476, 0.037, 59280, NA_real_)
table5_source$base_input_value <- c(595, economic_discordant_prevalence, 81660, NA_real_)
table5_source$upper_input_value <- c(714, full_known_aas_prevalence, 116370, NA_real_)
table5_source$lower_input_label <- c("$476", "3.7%", "$59,280", "Watanabe et al.")
table5_source$base_input_label <- c(
  "$595",
  sprintf("%.2f%%", 100 * economic_discordant_prevalence),
  "$81,660",
  "Asha and Miers"
)
table5_source$upper_input_label <- c(
  "$714",
  sprintf("%.2f%%", 100 * full_known_aas_prevalence),
  "$116,370",
  NA_character_
)
table5_source$input_unit <- c(
  "2022 USD per composite CTA",
  "Proportion",
  "2022 USD per missed AAS",
  "Meta-analysis source"
)

table5_source <- rbind(
  table5_source,
  data.frame(
    parameter = "Base-case modeled net savings",
    net_savings_lower_input = NA_real_,
    net_savings_base = economic_base$net_savings,
    net_savings_upper_input = NA_real_,
    net_savings_min = economic_base$net_savings,
    net_savings_max = economic_base$net_savings,
    sensitivity_range = 0,
    lower_input_value = NA_real_,
    base_input_value = economic_base$net_savings,
    upper_input_value = NA_real_,
    lower_input_label = NA_character_,
    base_input_label = paste0(
      "$",
      format(round(economic_base$net_savings), big.mark = ",", scientific = FALSE)
    ),
    upper_input_label = NA_character_,
    input_unit = "2022 USD; modeled",
    stringsAsFactors = FALSE
  )
)

table5_source$generating_object <- "table5_source"

# ---- Validate and save --------------------------------------------------------
# I check the expected discordant count before saving the sensitivity results.

if (economic_discordant_n != 873L) {
  stop("Economic discordant count does not reconcile to 873.", call. = FALSE)
}

sensitivity_results_file <- file.path(aas_derived_dir, "aas_sensitivity_results.rds")

saveRDS(
  list(
    yield_sensitivity_results = yield_sensitivity_results,
    model_sensitivity_results = model_sensitivity_results,
    resident_excluded_results = resident_excluded_results,
    low_acuity_results = low_acuity_results,
    discordance_reason_results = discordance_reason_results,
    alternative_diagnosis_by_algorithm = alternative_diagnosis_by_algorithm,
    economic_inputs = economic_inputs,
    economic_base_results = economic_base_results,
    table5_source = table5_source
  ),
  sensitivity_results_file
)

message("\nYield sensitivity analyses:")
print(yield_sensitivity_results, row.names = FALSE)

message("\nDiscordance decomposition with known AAS outcomes:")
print(discordance_reason_results, row.names = FALSE)

message("\nLow-acuity classifiable subgroup:")
print(low_acuity_results, row.names = FALSE)

message("\nAlternative diagnosis by algorithm class among AAS-negative encounters:")
print(alternative_diagnosis_by_algorithm, row.names = FALSE)

message("\nUpdated economic base case:")
print(economic_base_results, row.names = FALSE)

message("\nSensitivity result objects written outside Git:")
message(sensitivity_results_file)
