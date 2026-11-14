# Why this script is here
# I export aggregate clinical tables and the manuscript value registry.
# Running this script also reruns the upstream clinical chain.

# 08_tables_figures.R
# AAS CTA reanalysis, stage 9: aggregate deliverables, manuscript values and results handoff.
#
# Sourcing this file reruns stages 00-07 before exporting.

source("R/07_sensitivity_analyses.R")

results_dir <- "results"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

write_aggregate_csv <- function(x, filename) {
  utils::write.csv(x, file.path(results_dir, filename), row.names = FALSE, na = "")
}

# ---- Table 1-5 sources and supporting tables ----------------------------------
# I export the aggregate table sources produced by the upstream analysis.

write_aggregate_csv(table1_source, "table1_patient_provider_characteristics.csv")
write_aggregate_csv(table2_source, "table2_algorithm_concordance_yield.csv")
write_aggregate_csv(table3_source, "table3_alternative_diagnoses_provider_yield.csv")
write_aggregate_csv(table4_source, "table4_primary_regression.csv")
write_aggregate_csv(table5_source, "table5_economic_sensitivity.csv")
write_aggregate_csv(provider_reconciliation, "provider_reconciliation.csv")
write_aggregate_csv(yield_sensitivity_results, "yield_sensitivity_results.csv")
write_aggregate_csv(model_sensitivity_results, "model_sensitivity_results.csv")
write_aggregate_csv(clustered_model_estimates, "clustered_model_estimates.csv")
write_aggregate_csv(economic_inputs, "economic_model_inputs.csv")
write_aggregate_csv(economic_base_results, "economic_model_base_results.csv")

# ---- Machine-readable manuscript value registry -------------------------------
# I collect manuscript values with their denominators and source details so they can be traced back.

n_encounters <- nrow(analysis_data)
n_d_dimer_obtained <- sum(analysis_data$d_dimer_obtained)

# One registry row per element of value_id. Scalar arguments are recycled.
manuscript_value <- function(
  value_id,
  section,
  label,
  estimate = NA_real_,
  numerator = NA_real_,
  denominator = NA_real_,
  percent = NA_real_,
  ci_low = NA_real_,
  ci_high = NA_real_,
  p_value = NA_real_,
  units = "",
  generating_script,
  generating_object,
  notes = "",
  status = "Preliminary verified"
) {
  data.frame(
    value_id = value_id,
    section = section,
    label = label,
    estimate = estimate,
    numerator = numerator,
    denominator = denominator,
    percent = percent,
    ci_low = ci_low,
    ci_high = ci_high,
    p_value = p_value,
    units = units,
    generating_script = generating_script,
    generating_object = generating_object,
    notes = notes,
    status = status,
    stringsAsFactors = FALSE
  )
}

# Registry rows for counts: numerator n, its denominator and the percentage.
count_value <- function(value_id, section, label, n, denominator, ...) {
  manuscript_value(
    value_id, section, label,
    numerator = n,
    denominator = denominator,
    percent = 100 * n / denominator,
    ...
  )
}

count_levels <- function(x, levels, na.rm = FALSE) {
  vapply(levels, function(level) sum(x == level, na.rm = na.rm), integer(1), USE.NAMES = FALSE)
}

id_suffix <- function(x) tolower(gsub("[^A-Za-z]+", "_", x))

provider_roles <- c("Attending physician", "Resident physician", "APP", "Unknown")
provider_role_n <- count_levels(analysis_data$provider_role, provider_roles)
provider_group_n <- count_levels(analysis_data$provider_group, provider_groups)
sites <- c("Hospital A", "Hospital B", "Hospital C", "Hospital D")
add_rs_levels <- c("0", "1", "2", "3", "Missing")
d_dimer_statuses <- c("Absent/not obtained", "Positive", "Negative", "Indeterminate")
algorithm_classes <- c("Concordant", "Discordant", "Unclassifiable")

known_aas_n <- sum(!is.na(analysis_data$aas_positive))
aas_positive_n <- sum(analysis_data$aas_positive, na.rm = TRUE)

classifiable_known <-
  analysis_data$algorithm_class %in% c("Concordant", "Discordant") &
  !is.na(analysis_data$aas_positive)
classifiable_positive <- sum(analysis_data$aas_positive[classifiable_known])

clustered_nonintercept <- clustered_model_estimates[
  clustered_model_estimates$term != "(Intercept)", , drop = FALSE
]

model_sensitivity_nonintercept <- model_sensitivity_results[
  model_sensitivity_results$term != "(Intercept)", , drop = FALSE
]

manuscript_values <- rbind(
  manuscript_value(
    "cohort_encounters", "Cohort", "Eligible adult composite CTA encounters",
    estimate = n_encounters,
    units = "encounters",
    generating_script = "R/03_cohort_construction.R",
    generating_object = "final_cohort_encounters"
  ),
  manuscript_value(
    "cohort_unique_patients", "Cohort", "Unique patients",
    estimate = length(unique(analysis_data$patient_empi)),
    units = "patients",
    generating_script = "R/03_cohort_construction.R",
    generating_object = "n_final_unique_patients"
  ),
  manuscript_value(
    "cohort_repeat_patients", "Cohort", "Patients contributing more than one included encounter",
    estimate = sum(table(analysis_data$patient_empi) > 1L),
    units = "patients",
    generating_script = "R/03_cohort_construction.R",
    generating_object = "n_repeat_patients"
  ),
  count_value(
    paste0("provider_role_", id_suffix(provider_roles)), "Provider", provider_roles,
    estimate = provider_role_n,
    n = provider_role_n, denominator = n_encounters,
    units = "encounters",
    generating_script = "R/04_variable_derivation.R",
    generating_object = "provider_reconciliation"
  ),
  count_value(
    paste0("provider_group_", id_suffix(provider_groups)), "Provider",
    paste(provider_groups, "provider group"),
    estimate = provider_group_n,
    n = provider_group_n, denominator = n_encounters,
    units = "encounters",
    generating_script = "R/04_variable_derivation.R",
    generating_object = "provider_reconciliation"
  ),
  manuscript_value(
    "age_mean", "Table 1", "Age, mean",
    estimate = mean(analysis_data$age),
    denominator = sum(!is.na(analysis_data$age)),
    units = "years",
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table1_source",
    notes = paste0("SD=", round(stats::sd(analysis_data$age), 4))
  ),
  count_value(
    "male_sex", "Table 1", "Male sex",
    n = sum(analysis_data$sex == "Male"), denominator = n_encounters,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table1_source"
  ),
  count_value(
    "stat_order", "Table 1", "STAT order priority",
    n = sum(analysis_data$order_priority == "STAT"), denominator = n_encounters,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table1_source"
  ),
  count_value(
    paste0("esi_", 1:4), "Table 1", paste("ESI", 1:4),
    n = count_levels(analysis_data$esi_num, 1:4, na.rm = TRUE), denominator = n_encounters,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table1_source"
  ),
  count_value(
    paste0("site_", tolower(sub("Hospital ", "", sites))), "Table 1", sites,
    n = count_levels(analysis_data$site, sites), denominator = n_encounters,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table1_source"
  ),
  count_value(
    paste0("add_rs_", tolower(add_rs_levels)), "Risk stratification",
    paste("ADD-RS", add_rs_levels),
    n = count_levels(add_rs_display, add_rs_levels), denominator = n_encounters,
    generating_script = "R/04_variable_derivation.R",
    generating_object = "analytic_data$add_rs"
  ),
  # Result percentages use obtained tests as the denominator.
  count_value(
    paste0("d_dimer_", id_suffix(d_dimer_statuses)), "D-dimer", d_dimer_statuses,
    n = count_levels(analysis_data$d_dimer_status, d_dimer_statuses),
    denominator = ifelse(
      d_dimer_statuses == "Absent/not obtained",
      n_encounters,
      n_d_dimer_obtained
    ),
    generating_script = "R/04_variable_derivation.R",
    generating_object = "analytic_data$d_dimer_status"
  ),
  count_value(
    paste0("algorithm_", tolower(algorithm_classes)), "Algorithm",
    paste("Algorithm", tolower(algorithm_classes)),
    n = count_levels(analysis_data$algorithm_class, algorithm_classes),
    denominator = n_encounters,
    generating_script = "R/04_variable_derivation.R",
    generating_object = "analytic_data$algorithm_class"
  ),
  manuscript_value(
    paste0("discordance_reason_", seq_len(nrow(discordance_reason_results))), "Algorithm",
    discordance_reason_results$discordance_reason,
    numerator = discordance_reason_results$aas_positive_n,
    denominator = discordance_reason_results$aas_known_denominator,
    percent = discordance_reason_results$aas_positive_percent,
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "discordance_reason_results",
    notes = "Numerator is AAS-positive; denominator is discordant encounters with known AAS outcome"
  ),
  count_value(
    "aas_full_cohort", "Outcome", "AAS positivity, full cohort with known outcome",
    n = aas_positive_n, denominator = known_aas_n,
    generating_script = "R/04_variable_derivation.R",
    generating_object = "analytic_data$aas_positive"
  ),
  count_value(
    "aas_classifiable_cohort", "Outcome",
    "AAS positivity, classifiable cohort with known outcome",
    n = classifiable_positive, denominator = sum(classifiable_known),
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table2_source"
  ),
  manuscript_value(
    "yield_concordant", "Primary yield", "AAS yield, algorithm concordant",
    numerator = primary_yield_comparison$concordant_positive,
    denominator = primary_yield_comparison$concordant_denominator,
    percent = primary_yield_comparison$concordant_percent,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "primary_yield_comparison"
  ),
  manuscript_value(
    "yield_discordant", "Primary yield", "AAS yield, algorithm discordant",
    numerator = primary_yield_comparison$discordant_positive,
    denominator = primary_yield_comparison$discordant_denominator,
    percent = primary_yield_comparison$discordant_percent,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "primary_yield_comparison"
  ),
  manuscript_value(
    "yield_absolute_difference", "Primary yield",
    "Absolute yield difference, concordant minus discordant",
    estimate = primary_yield_comparison$absolute_difference_percentage_points,
    ci_low = primary_yield_comparison$ci_low_percentage_points,
    ci_high = primary_yield_comparison$ci_high_percentage_points,
    p_value = primary_yield_comparison$p_value,
    units = "percentage points",
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "primary_yield_comparison"
  ),
  manuscript_value(
    paste0("provider_yield_", seq_len(nrow(provider_yield_comparison))), "Provider yield",
    paste(
      provider_yield_comparison$comparison,
      provider_yield_comparison$provider_group,
      sep = ": "
    ),
    numerator = provider_yield_comparison$aas_positive_n,
    denominator = provider_yield_comparison$aas_known_denominator,
    percent = provider_yield_comparison$aas_positive_percent,
    p_value = provider_yield_comparison$p_value,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "provider_yield_comparison"
  ),
  manuscript_value(
    paste0("low_acuity_", seq_len(nrow(low_acuity_results))), "Low-acuity subgroup",
    paste("ESI 3-4", low_acuity_results$provider_group),
    numerator = low_acuity_results$aas_positive_n,
    denominator = low_acuity_results$aas_known_denominator,
    percent = 100 * low_acuity_results$aas_positive_n / low_acuity_results$aas_known_denominator,
    p_value = low_acuity_results$yield_p_value,
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "low_acuity_results",
    notes = paste0(
      "Concordant ", low_acuity_results$concordant_n, "/", low_acuity_results$classifiable_n,
      "; concordance P=", signif(low_acuity_results$concordance_p_value, 4)
    )
  ),
  manuscript_value(
    paste0("alternative_diagnosis_", seq_len(nrow(table3_panel_a))), "Alternative diagnosis",
    table3_panel_a$category,
    numerator = table3_panel_a$numerator,
    denominator = table3_panel_a$denominator,
    percent = table3_panel_a$percent,
    ci_low = table3_panel_a$ci_low_percent,
    ci_high = table3_panel_a$ci_high_percent,
    generating_script = "R/05_descriptive_analysis.R",
    generating_object = "table3_source"
  ),
  manuscript_value(
    paste0("primary_model_", seq_len(nrow(table4_source))), "Primary regression",
    table4_source$comparison,
    estimate = table4_source$odds_ratio,
    denominator = table4_source$analytic_n,
    ci_low = table4_source$ci_low,
    ci_high = table4_source$ci_high,
    p_value = table4_source$p_value,
    units = "adjusted odds ratio",
    generating_script = "R/06_primary_models.R",
    generating_object = "table4_source",
    notes = paste0(
      table4_source$discordant_events, " discordant and ",
      table4_source$concordant_events, " concordant outcomes"
    )
  ),
  manuscript_value(
    paste0("clustered_model_", seq_len(nrow(clustered_nonintercept))), "Clustered regression",
    paste(clustered_nonintercept$comparison, clustered_nonintercept$covariance, sep = ": "),
    estimate = clustered_nonintercept$odds_ratio,
    denominator = nrow(model_data),
    ci_low = clustered_nonintercept$ci_low,
    ci_high = clustered_nonintercept$ci_high,
    p_value = clustered_nonintercept$p_value,
    units = "adjusted odds ratio",
    generating_script = "R/06_primary_models.R",
    generating_object = "clustered_model_estimates"
  ),
  manuscript_value(
    paste0("model_sensitivity_", seq_len(nrow(model_sensitivity_nonintercept))),
    "Sensitivity regression",
    paste(
      model_sensitivity_nonintercept$analysis,
      model_sensitivity_nonintercept$comparison,
      sep = ": "
    ),
    estimate = model_sensitivity_nonintercept$odds_ratio,
    denominator = model_sensitivity_nonintercept$analytic_n,
    ci_low = model_sensitivity_nonintercept$ci_low,
    ci_high = model_sensitivity_nonintercept$ci_high,
    p_value = model_sensitivity_nonintercept$p_value,
    units = "adjusted odds ratio",
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "model_sensitivity_results"
  ),
  manuscript_value(
    paste0("alternative_by_algorithm_", seq_len(nrow(alternative_diagnosis_by_algorithm))),
    "Alternative diagnosis",
    paste(alternative_diagnosis_by_algorithm$algorithm_class, "AAS-negative encounters"),
    numerator = alternative_diagnosis_by_algorithm$alternative_diagnosis_n,
    denominator = alternative_diagnosis_by_algorithm$aas_negative_denominator,
    percent = alternative_diagnosis_by_algorithm$alternative_diagnosis_percent,
    p_value = alternative_diagnosis_by_algorithm$p_value,
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "alternative_diagnosis_by_algorithm"
  ),
  manuscript_value(
    paste0("yield_sensitivity_", seq_len(nrow(yield_sensitivity_results))),
    "Sensitivity analysis",
    yield_sensitivity_results$analysis,
    estimate = yield_sensitivity_results$absolute_difference_percentage_points,
    numerator = yield_sensitivity_results$concordant_positive,
    denominator = yield_sensitivity_results$concordant_denominator,
    ci_low = yield_sensitivity_results$ci_low_percentage_points,
    ci_high = yield_sensitivity_results$ci_high_percentage_points,
    p_value = yield_sensitivity_results$p_value,
    units = "percentage points",
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "yield_sensitivity_results",
    notes = paste0(
      "Discordant yield ", yield_sensitivity_results$discordant_positive, "/",
      yield_sensitivity_results$discordant_denominator
    )
  ),
  manuscript_value(
    paste0("economic_", seq_len(nrow(economic_base_results))), "Economic model",
    economic_base_results$measure,
    estimate = economic_base_results$value,
    units = ifelse(
      grepl("rate", tolower(economic_base_results$measure)),
      "proportion",
      ifelse(grepl("cases|ctas", tolower(economic_base_results$measure)), "count", "2022 USD")
    ),
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "economic_base_results",
    notes = "Modeled, not observed",
    status = "Preliminary modeled"
  )
)

write_aggregate_csv(manuscript_values, "manuscript_values.csv")

# ---- SAS/manuscript-versus-R discrepancy log ----------------------------------
# I record comparisons with historical values for reconciliation, rather than treating them as required results.

format_usd <- function(x) format(x, big.mark = ",", scientific = FALSE)

discrepancy <- function(
  discrepancy_id,
  topic,
  historical_source,
  historical_value,
  current_r_value,
  explanation,
  generating_script,
  generating_object,
  status = "Resolved in preliminary R analysis"
) {
  data.frame(
    discrepancy_id = discrepancy_id,
    topic = topic,
    historical_source = historical_source,
    historical_value = historical_value,
    current_r_value = current_r_value,
    status = status,
    explanation = explanation,
    generating_script = generating_script,
    generating_object = generating_object,
    stringsAsFactors = FALSE
  )
}

historical_or <- c(1.05, 0.84, 1.47, 1.19, 1.32, 0.41, 0.82, 0.59)

sas_r_discrepancy_log <- rbind(
  discrepancy(
    "D001", "Primary cohort", "August 2 manuscript/Tables 1-5",
    "1,194 observations described as patients",
    "1,193 eligible encounters; 1,120 unique patients",
    "The current source has one age-16 encounter excluded under the adult definition; repeat encounters make patients and observations non-equivalent.",
    "R/03_cohort_construction.R", "final_cohort_encounters"
  ),
  discrepancy(
    paste0("D00", seq_len(nrow(provider_reconciliation)) + 1L),
    paste("Provider denominator:", provider_reconciliation$measure),
    "August 2 manuscript/Table 1 and historical SAS",
    as.character(provider_reconciliation$historical_manuscript_sas_n),
    as.character(provider_reconciliation$r_earliest_order_n),
    "R uses the approved earliest-order attribution and treats No Access as noninformative.",
    "R/04_variable_derivation.R", "provider_reconciliation"
  ),
  discrepancy(
    "D008", "D-dimer obtained", "August 2 manuscript/Table 1",
    "127/1,194", "126/1,193",
    "One fewer adult encounter after excluding the age-16 encounter.",
    "R/04_variable_derivation.R", "analytic_data$d_dimer_status"
  ),
  discrepancy(
    "D009", "D-dimer positive", "August 2 manuscript/Table 1",
    "112/127", "71/126",
    "Historical SAS used a 0.5 numeric cutoff; current abstracted values and inequalities are on a scale consistent with a 500 cutoff.",
    "R/04_variable_derivation.R", "analytic_data$d_dimer_status"
  ),
  discrepancy(
    "D010", "D-dimer negative", "August 2 manuscript/Table 1",
    "15/127", "55/126",
    "R correctly recognizes numeric values below 500 and explicit lower-bound values as negative.",
    "R/04_variable_derivation.R", "analytic_data$d_dimer_status"
  ),
  discrepancy(
    "D011", "Algorithm concordance", "August 2 manuscript/Table 2",
    "317 concordant; 836 discordant; 41 unclassifiable",
    "280 concordant; 873 discordant; 40 unclassifiable",
    "Correct D-dimer parsing moves low-risk encounters with values below 500 from concordant to discordant; current adult source has 40 fully missing ADD-RS records.",
    "R/04_variable_derivation.R", "analytic_data$algorithm_class"
  ),
  discrepancy(
    "D012", "AAS outcome", "August 2 manuscript/Table 3",
    "80/1,194 positive; all remaining observations treated negative",
    "79/1,190 known outcomes; 3 unknown",
    "The excluded age-16 encounter was AAS-positive, and R does not force missing AAS abstraction to negative.",
    "R/04_variable_derivation.R", "analytic_data$aas_outcome"
  ),
  discrepancy(
    "D013", "Primary yield comparison", "August 2 manuscript/Table 2",
    "34/317 (10.7%) vs 42/836 (5.0%); difference 5.7 points",
    sprintf(
      "%d/%d (%.1f%%) vs %d/%d (%.1f%%); difference %.1f points",
      primary_yield_comparison$concordant_positive,
      primary_yield_comparison$concordant_denominator,
      primary_yield_comparison$concordant_percent,
      primary_yield_comparison$discordant_positive,
      primary_yield_comparison$discordant_denominator,
      primary_yield_comparison$discordant_percent,
      primary_yield_comparison$absolute_difference_percentage_points
    ),
    "D-dimer reclassification changes the concordant denominator and one AAS-positive concordant case; unknown outcomes are excluded.",
    "R/05_descriptive_analysis.R", "primary_yield_comparison"
  ),
  discrepancy(
    "D014", "Clinically meaningful alternative diagnosis", "August 2 manuscript/Table 3",
    "199/1,114 (17.9%)",
    sprintf(
      "%d/%d (%.1f%%)",
      table3_panel_a$numerator[1L],
      table3_panel_a$denominator[1L],
      table3_panel_a$percent[1L]
    ),
    "R uses known AAS-negative adult encounters and the documented historical broad-net text rules.",
    "R/05_descriptive_analysis.R", "table3_source"
  ),
  discrepancy(
    paste0("D", sprintf("%03d", 14L + seq_len(nrow(table4_source)))),
    paste("Primary regression:", table4_source$comparison),
    "August 2 manuscript/Table 4",
    paste0("OR ", historical_or, "; N=1,153"),
    sprintf("OR %.4f; N=%d", table4_source$odds_ratio, table4_source$analytic_n),
    "R refits the prespecified model after corrected D-dimer classification and excludes the single unknown provider.",
    "R/06_primary_models.R", "table4_source"
  ),
  discrepancy(
    "D023", "Standardized gross imaging expenditure", "August 2 manuscript/Table 5",
    "$710,430 total; $497,420 discordant",
    sprintf(
      "$%s total; $%s discordant",
      format_usd(n_encounters * 595),
      format_usd(economic_discordant_n * 595)
    ),
    "Updated cohort and discordant counts are multiplied by the unchanged $595 standardized unit price.",
    "R/07_sensitivity_analyses.R", "economic_base_results"
  ),
  discrepancy(
    "D024", "Modeled base-case net savings", "August 2 manuscript/Table 5",
    "$108,954", paste0("$", format_usd(round(economic_base$net_savings))),
    "The historical decision-model formula is reproduced with the updated discordant count and observed discordant AAS prevalence.",
    "R/07_sensitivity_analyses.R", "economic_base_results",
    status = "Preliminary modeled; input provenance limitations remain"
  )
)

write_aggregate_csv(sas_r_discrepancy_log, "sas_r_discrepancy_log.csv")

# ---- Manuscript-facing results handoff ----------------------------------------
# I assemble formatted results from the analysis objects for the manuscript handoff.

format_p <- function(x) {
  ifelse(is.na(x), "NA", ifelse(x < 0.001, "<.001", sub("^0", "", sprintf("%.3f", x))))
}

format_n_pct <- function(n, denominator) {
  sprintf("%d/%d (%.1f%%)", n, denominator, 100 * n / denominator)
}

# sprintf() with the values supplied as a list.
fill <- function(template, values) do.call(sprintf, c(list(template), values))

# "n/N (%)" of the full cohort for each level of x.
cohort_pct <- function(x, levels, na.rm = FALSE) {
  as.list(format_n_pct(count_levels(x, levels, na.rm = na.rm), n_encounters))
}

# Odds ratio and CI bounds for the three terms quoted in the handoff, in the
# order ESI 3-4, non-physician, Hospital A.
key_or_ci <- function(estimates) {
  key_terms <- c("esi_modelESI 3-4", "provider_modelNon-physician", "site_modelHospital A")
  rows <- estimates[match(key_terms, estimates$term), c("odds_ratio", "ci_low", "ci_high")]
  as.list(t(as.matrix(rows)))
}

# One row of provider_yield_comparison.
yield_row <- function(comparison, provider_group) {
  provider_yield_comparison[
    provider_yield_comparison$comparison == comparison &
      provider_yield_comparison$provider_group == provider_group,
  ]
}

physician_yield <- yield_row("Physician vs non-physician", "Physician")
nonphysician_yield <- yield_row("Physician vs non-physician", "Non-physician")
attending_yield <- yield_row("Attending physician vs APP", "Attending physician")
app_yield <- yield_row("Attending physician vs APP", "APP")

# sprintf() values for a concordant-versus-discordant yield sentence.
yield_values <- function(yield) {
  list(
    format_n_pct(yield$concordant_positive, yield$concordant_denominator),
    format_n_pct(yield$discordant_positive, yield$discordant_denominator),
    yield$absolute_difference_percentage_points,
    yield$ci_low_percentage_points,
    yield$ci_high_percentage_points,
    format_p(yield$p_value)
  )
}

handoff <- c(
  "# Preliminary R Results Handoff",
  "",
  "All values below are encounter-level unless explicitly labeled as patients. Values are preliminary verified R results and should replace historical values only with the accompanying discrepancy explanation.",
  "",
  "## Cohort and provider attribution",
  "",
  sprintf(
    "- Primary adult cohort: **%d eligible encounters from %d unique patients**; %d patients contributed more than one encounter. [R/03_cohort_construction.R; `final_cohort_encounters`, `n_final_unique_patients`, `n_repeat_patients`]",
    n_encounters, length(unique(analysis_data$patient_empi)),
    sum(table(analysis_data$patient_empi) > 1L)
  ),
  fill(
    "- Provider roles: attending %s; resident %s; APP %s; unknown %s. [R/04_variable_derivation.R; `provider_reconciliation`]",
    cohort_pct(analysis_data$provider_role, provider_roles)
  ),
  fill(
    "- Primary provider groups: physician %s; non-physician %s; unknown %s. [R/04_variable_derivation.R; `provider_reconciliation`]",
    cohort_pct(analysis_data$provider_group, provider_groups)
  ),
  "",
  "## Table 1 characteristics",
  "",
  sprintf(
    "- Age: mean %.1f years (SD %.1f; n=%d). Male sex: %s. STAT priority: %s. [R/05_descriptive_analysis.R; `table1_source`]",
    mean(analysis_data$age), stats::sd(analysis_data$age), sum(!is.na(analysis_data$age)),
    format_n_pct(sum(analysis_data$sex == "Male"), n_encounters),
    format_n_pct(sum(analysis_data$order_priority == "STAT"), n_encounters)
  ),
  fill(
    "- ESI: level 1 %s; level 2 %s; level 3 %s; level 4 %s. [R/05_descriptive_analysis.R; `table1_source`]",
    cohort_pct(analysis_data$esi_num, 1:4)
  ),
  fill(
    "- Sites: Hospital A %s; B %s; C %s; D %s. [R/05_descriptive_analysis.R; `table1_source`]",
    cohort_pct(analysis_data$site, sites)
  ),
  fill(
    "- Race: Black %s; White %s; Asian %s; other/multiracial %s; unknown %s. [R/05_descriptive_analysis.R; `table1_source`]",
    cohort_pct(
      analysis_data$race_group,
      c("Black", "White", "Asian", "Other/Multiracial", "Unknown")
    )
  ),
  fill(
    "- Chief concern: chest pain %s; back/flank pain %s; SOB/respiratory %s; other/not recorded %s. [R/05_descriptive_analysis.R; `table1_source`]",
    cohort_pct(
      analysis_data$chief_concern,
      c("Chest pain", "Back/flank pain", "SOB/respiratory", "Other/not recorded")
    )
  ),
  fill(
    "- Disposition: discharged home %s; transferred/other %s; expired %s; left AMA %s. [R/05_descriptive_analysis.R; `table1_source`]",
    cohort_pct(
      analysis_data$hospital_disposition,
      c("Discharged home", "Transferred/other", "Expired", "Left AMA")
    )
  ),
  "- Provider-stratified versions and comparison P values are in `results/table1_patient_provider_characteristics.csv`. [R/05_descriptive_analysis.R; `table1_source`]",
  "",
  "## ADD-RS, D-dimer, and algorithm classification",
  "",
  fill(
    "- ADD-RS: 0, %s; 1, %s; 2, %s; 3, %s; missing, %s. Two encounters had partial component missingness. [R/04_variable_derivation.R; `analytic_data$add_rs`, `add_rs_partial_missing`]",
    cohort_pct(add_rs_display, add_rs_levels)
  ),
  sprintf(
    "- D-dimer: obtained %s; absent %s. Among obtained tests, positive %s and negative %s; indeterminate 0/126. [R/04_variable_derivation.R; `analytic_data$d_dimer_status`]",
    format_n_pct(n_d_dimer_obtained, n_encounters),
    format_n_pct(sum(!analysis_data$d_dimer_obtained), n_encounters),
    format_n_pct(sum(analysis_data$d_dimer_status == "Positive"), n_d_dimer_obtained),
    format_n_pct(sum(analysis_data$d_dimer_status == "Negative"), n_d_dimer_obtained)
  ),
  fill(
    "- Algorithm classification: concordant %s; discordant %s; unclassifiable %s. Discordance comprised 825 absent-D-dimer encounters (42/824 AAS-positive among known outcomes) and 48 negative-D-dimer encounters (0/48 AAS-positive). [R/04_variable_derivation.R; `analytic_data$algorithm_class`; R/07_sensitivity_analyses.R; `discordance_reason_results`]",
    cohort_pct(analysis_data$algorithm_class, algorithm_classes)
  ),
  "",
  "## AAS outcome and primary yield",
  "",
  sprintf(
    "- Full cohort: AAS-positive %s among known outcomes; 3/1,193 outcomes were unknown. Classifiable cohort: AAS-positive %s among known outcomes. [R/04_variable_derivation.R; `analytic_data$aas_positive`; R/05_descriptive_analysis.R; `table2_source`]",
    format_n_pct(aas_positive_n, known_aas_n),
    format_n_pct(classifiable_positive, sum(classifiable_known))
  ),
  fill(
    "- Concordant yield %s versus discordant yield %s; absolute difference %.1f percentage points (95%% CI, %.1f to %.1f; P %s). [R/05_descriptive_analysis.R; `primary_yield_comparison`]",
    yield_values(primary_yield_comparison)
  ),
  sprintf(
    "- Physician yield %s versus non-physician %s (P %s). Attending yield %s versus APP %s (P %s). [R/05_descriptive_analysis.R; `provider_yield_comparison`]",
    format_n_pct(physician_yield$aas_positive_n, physician_yield$aas_known_denominator),
    format_n_pct(nonphysician_yield$aas_positive_n, nonphysician_yield$aas_known_denominator),
    format_p(physician_yield$p_value),
    format_n_pct(attending_yield$aas_positive_n, attending_yield$aas_known_denominator),
    format_n_pct(app_yield$aas_positive_n, app_yield$aas_known_denominator),
    format_p(attending_yield$p_value)
  ),
  "",
  "## Low-acuity and alternative diagnoses",
  "",
  sprintf(
    "- ESI 3-4 classifiable subgroup: physician concordance 120/573 and AAS positivity 23/571; non-physician concordance 15/75 and AAS positivity 6/75. Concordance P=%s; yield P=%s. [R/07_sensitivity_analyses.R; `low_acuity_results`]",
    format_p(low_acuity_concordance_p), format_p(low_acuity_yield_p)
  ),
  sprintf(
    "- Clinically meaningful alternative diagnosis: %s of known AAS-negative encounters (exact 95%% CI, %.1f%%-%.1f%%). Concordant negative examinations: 63/246; discordant negative examinations: 130/830 (P %s). [R/05_descriptive_analysis.R; `table3_source`; R/07_sensitivity_analyses.R; `alternative_diagnosis_by_algorithm`]",
    format_n_pct(table3_panel_a$numerator[1L], table3_panel_a$denominator[1L]),
    table3_panel_a$ci_low_percent[1L], table3_panel_a$ci_high_percent[1L],
    format_p(alternative_p)
  ),
  "",
  "## Primary regression",
  "",
  sprintf(
    "- Analytic N=%d (%d discordant, %d concordant). [R/06_primary_models.R; `table4_source`]",
    nrow(model_data), sum(model_data$algorithm_discordant), sum(!model_data$algorithm_discordant)
  ),
  sprintf(
    "- %s: adjusted OR %.2f (95%% CI, %.2f-%.2f; P %s). [R/06_primary_models.R; `table4_source`]",
    table4_source$comparison, table4_source$odds_ratio, table4_source$ci_low,
    table4_source$ci_high, format_p(table4_source$p_value)
  ),
  "",
  "## Repeated observations, clustering, and timing",
  "",
  fill(
    "- Patient-clustered CR1: ESI 3-4 OR %.2f (95%% CI %.2f-%.2f); non-physician OR %.2f (%.2f-%.2f); Hospital A OR %.2f (%.2f-%.2f). [R/06_primary_models.R; `clustered_model_estimates`]",
    key_or_ci(patient_cluster_estimates)
  ),
  fill(
    "- Provider-clustered CR1: ESI 3-4 OR %.2f (95%% CI %.2f-%.2f); non-physician OR %.2f (%.2f-%.2f); Hospital A OR %.2f (%.2f-%.2f). [R/06_primary_models.R; `clustered_model_estimates`]",
    key_or_ci(provider_cluster_estimates)
  ),
  fill(
    "- First encounter per patient: concordant %s versus discordant %s; difference %.1f points (95%% CI %.1f-%.1f; P %s). [R/07_sensitivity_analyses.R; `yield_sensitivity_results`]",
    yield_values(first_encounter_yield)
  ),
  fill(
    "- First-encounter model (N=%d): ESI 3-4 OR %.2f (95%% CI %.2f-%.2f); non-physician OR %.2f (%.2f-%.2f); Hospital A OR %.2f (%.2f-%.2f). [R/07_sensitivity_analyses.R; `model_sensitivity_results`]",
    c(
      list(unique(first_encounter_model_estimates$analytic_n)),
      key_or_ci(first_encounter_model_estimates)
    )
  ),
  fill(
    "- <=60-minute cohort (n=%d): concordant %s versus discordant %s; difference %.1f points (95%% CI %.1f-%.1f; P %s). [R/07_sensitivity_analyses.R; `timing_yield`]",
    c(list(nrow(timing_data)), yield_values(timing_yield))
  ),
  fill(
    "- <=60-minute model (N=%d): ESI 3-4 OR %.2f (95%% CI %.2f-%.2f); non-physician OR %.2f (%.2f-%.2f); Hospital A OR %.2f (%.2f-%.2f). [R/07_sensitivity_analyses.R; `model_sensitivity_results`]",
    c(list(unique(timing_model_estimates$analytic_n)), key_or_ci(timing_model_estimates))
  ),
  "- The patient/provider random-intercept logistic model converged numerically but produced an extreme patient variance and is not considered stable; site-clustered standard errors are not reliable with only 4 sites. [R/06_primary_models.R; `mixed_model_status`, `cluster_summary`]",
  "",
  "## Preliminary economic quantities",
  "",
  sprintf(
    "- Standardized gross expenditure: $%s for %d encounters and $%s for %d discordant encounters at $595 per composite CTA. [R/07_sensitivity_analyses.R; `economic_base_results`]",
    format_usd(n_encounters * 595),
    n_encounters,
    format_usd(economic_discordant_n * 595),
    economic_discordant_n
  ),
  sprintf(
    "- Historical decision-model formula with updated cohort quantities: projected %.1f%% D-dimer-negative rate, %.1f deferred CTAs, %.2f expected missed AAS cases, and $%s modeled net savings over the six-year cohort. [R/07_sensitivity_analyses.R; `economic_base_results`]",
    100 * economic_base$projected_d_dimer_negative_rate,
    economic_base$projected_deferred_cta,
    economic_base$expected_missed_aas,
    format_usd(round(economic_base$net_savings))
  ),
  "",
  "## Issues preventing final lock",
  "",
  "- D-dimer assay, units, assay-specific cutoff, and age-adjustment metadata remain unavailable; 500 is the source-compatible preliminary cutoff. [R/04_variable_derivation.R; `derive_d_dimer_status`]",
  "- Three encounters lack an informative abstracted AAS outcome and are excluded from yield denominators. [R/04_variable_derivation.R; `analytic_data$aas_outcome`]",
  "- Alternative diagnoses use the documented historical broad-net text rules and have not undergone new blinded clinical adjudication. [R/04_variable_derivation.R; `alternative_diagnosis_definite`]",
  "- The economic analysis is hypothetical and retains historical price, test-performance, and missed-diagnosis-cost assumptions whose external provenance is not fully locked. [R/07_sensitivity_analyses.R; `economic_inputs`]",
  "- Clustering by site cannot be estimated reliably with four sites; the mixed model is unstable because most patients contribute only one encounter. [R/06_primary_models.R; `cluster_summary`, `mixed_model_status`]"
)

writeLines(handoff, "docs/results_handoff.md", useBytes = TRUE)

# ---- Validate aggregate deliverables ------------------------------------------
# I check that expected files exist and that the registry and table shapes pass their checks.

required_outputs <- file.path(
  results_dir,
  c(
    "manuscript_values.csv",
    "sas_r_discrepancy_log.csv",
    "table1_patient_provider_characteristics.csv",
    "table2_algorithm_concordance_yield.csv",
    "table3_alternative_diagnoses_provider_yield.csv",
    "table4_primary_regression.csv",
    "table5_economic_sensitivity.csv"
  )
)

if (!all(file.exists(required_outputs))) {
  stop("One or more required aggregate outputs were not created.", call. = FALSE)
}

if (anyDuplicated(manuscript_values$value_id)) {
  stop("manuscript_values.csv contains duplicated value IDs.", call. = FALSE)
}

if (nrow(table4_source) != 8L || nrow(table5_source) != 5L) {
  stop("Table 4 or Table 5 source row counts are unexpected.", call. = FALSE)
}

message("\nAggregate deliverables created:")
message(paste(c(required_outputs, "docs/results_handoff.md"), collapse = "\n"))
