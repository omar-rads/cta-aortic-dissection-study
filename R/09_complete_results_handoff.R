# Why this script is here
# I assemble a detailed aggregate results ledger from saved derived objects.
# The ledger depends on the versions of those saved objects.

# 09_complete_results_handoff.R
# Aggregate-only complete Results-section handoff.
#
# Reads the validated stage 04-07 RDS outputs. It does not alter cohort,
# exposure, outcome, model, D-dimer, provider, or economic definitions.

source("R/00_setup.R")

analysis_data <- readRDS(file.path(aas_derived_dir, "aas_analytic_encounters.rds"))
descriptive <- readRDS(file.path(aas_derived_dir, "aas_descriptive_results.rds"))
models <- readRDS(file.path(aas_derived_dir, "aas_primary_model_results.rds"))
sensitivity <- readRDS(file.path(aas_derived_dir, "aas_sensitivity_results.rds"))

output_dir <- file.path("results", "results_handoff")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_output <- function(x, filename) {
  utils::write.csv(x, file.path(output_dir, filename), row.names = FALSE, na = "")
}

fmt <- function(x) {
  ifelse(is.na(x), "—", format(x, digits = 9, trim = TRUE, scientific = FALSE))
}

md_table <- function(x) {
  y <- as.data.frame(lapply(x, fmt), check.names = FALSE)
  c(
    paste0("| ", paste(names(y), collapse = " | "), " |"),
    paste0("| ", paste(rep("---", ncol(y)), collapse = " | "), " |"),
    apply(y, 1L, function(z) paste0("| ", paste(z, collapse = " | "), " |")),
    ""
  )
}

# Physician versus non-physician Pearson chi-square without continuity correction.
chisq_details <- function(group, outcome) {
  keep <- !is.na(group) & !is.na(outcome) & group %in% c("Physician", "Non-physician")
  tab <- table(factor(group[keep], levels = c("Physician", "Non-physician")), outcome[keep])
  tab <- tab[, colSums(tab) > 0L, drop = FALSE]
  if (ncol(tab) < 2L) return(c(statistic = NA, df = NA, p = NA))
  z <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  c(statistic = unname(z$statistic), df = unname(z$parameter), p = z$p.value)
}

# Exact (Clopper-Pearson) 95% CI in percent.
binom_ci <- function(n, d) {
  if (!d) return(c(low = NA, high = NA))
  100 * stats::binom.test(n, d)$conf.int
}

n_encounters <- nrow(analysis_data)
known_outcome <- !is.na(analysis_data$aas_positive)
aas_positive <- analysis_data$aas_positive == 1L
classifiable <- analysis_data$algorithm_class %in% c("Concordant", "Discordant")
discordant <- analysis_data$algorithm_class == "Discordant"
unclassifiable <- analysis_data$algorithm_class == "Unclassifiable"
physician <- analysis_data$provider_group == "Physician"
nonphysician <- analysis_data$provider_group == "Non-physician"
d_dimer_obtained <- analysis_data$d_dimer_obtained

# ---- Cohort -------------------------------------------------------------------
# I summarize the saved encounter cohort and patient contribution for the ledger.

patient_counts <- table(analysis_data$patient_empi)

cohort <- data.frame(
  result = c(
    "Eligible encounters", "Unique patients", "Patients with >1 encounter",
    "Encounters contributed by patients with >1 encounter",
    "Repeated encounters beyond one per patient", "<=60-minute encounters",
    "Known AAS outcomes", "Unknown AAS outcomes", "AAS positive",
    "AAS negative", "Algorithm classifiable", "Algorithm unclassifiable"
  ),
  numerator = c(
    n_encounters, length(patient_counts), sum(patient_counts > 1L),
    sum(patient_counts[patient_counts > 1L]),
    n_encounters - length(patient_counts), sum(analysis_data$within_60_minutes),
    sum(known_outcome), sum(!known_outcome),
    sum(aas_positive, na.rm = TRUE),
    sum(analysis_data$aas_positive == 0L, na.rm = TRUE),
    sum(classifiable),
    sum(unclassifiable)
  ),
  denominator = c(
    n_encounters, NA_integer_, length(patient_counts), n_encounters,
    n_encounters, n_encounters, n_encounters,
    n_encounters, sum(known_outcome), sum(known_outcome),
    n_encounters, n_encounters
  ),
  generating_script = c(
    rep("R/03_cohort_construction.R", 6L),
    rep("R/04_variable_derivation.R", 6L)
  ),
  generating_object = c(
    "analytic_data", "analytic_data$patient_empi", "table(analytic_data$patient_empi)",
    "sum(table(analytic_data$patient_empi)[table(analytic_data$patient_empi)>1])",
    "nrow(analytic_data)-length(unique(patient_empi))", "analytic_data$within_60_minutes",
    rep("analytic_data", 6L)
  ),
  stringsAsFactors = FALSE
)
cohort$percent <- 100 * cohort$numerator / cohort$denominator
cohort$missing_unknown <- c(rep(0L, 6L), 3L, 3L, 3L, 3L, 40L, 40L)
cohort$denominator_definition <- c(
  "Eligible encounters", "Not applicable to a unique count", "Unique patients",
  "Eligible encounters", "Eligible encounters", "Eligible encounters", "Eligible encounters",
  "Eligible encounters", "Known AAS outcomes", "Known AAS outcomes",
  "Eligible encounters", "Eligible encounters"
)
cohort$result_status <- ifelse(
  grepl("Algorithm", cohort$result),
  "Provisional pending D-dimer cutoff confirmation",
  "Current R final under approved definitions"
)
write_output(cohort, "01_cohort_and_analytic_units.csv")

# ---- Complete Table 1 ---------------------------------------------------------
# I extend the saved descriptive table with the specified provider comparisons and tests.

# The single unknown-provider encounter is not cross-tabulated.
table1 <- descriptive$table1_source
table1 <- table1[!grepl("^unknown_provider_", names(table1))]
table1$test_statistic <- NA_real_
table1$test_df <- NA_real_
table1$test_method <- ifelse(
  table1$summary_type == "mean_sd", "Welch two-sample t test",
  "Pearson chi-square without continuity correction"
)

age_test <- stats::t.test(
  age ~ provider_group,
  data = analysis_data[analysis_data$provider_group %in% c("Physician", "Non-physician"), ]
)
age_rows <- table1$characteristic == "Age, years"
table1$test_statistic[age_rows] <- unname(age_test$statistic)
table1$test_df[age_rows] <- unname(age_test$parameter)

d_dimer_result_test <- chisq_details(
  analysis_data$provider_group[d_dimer_obtained],
  analysis_data$d_dimer_status[d_dimer_obtained]
)

categorical_map <- list(
  "Sex" = analysis_data$sex,
  "Race" = analysis_data$race_group,
  "Order priority" = analysis_data$order_priority,
  "Emergency Severity Index" =
    ifelse(is.na(analysis_data$esi_num), "Missing", as.character(analysis_data$esi_num)),
  "Chief concern" = analysis_data$chief_concern,
  "Hospital disposition" = analysis_data$hospital_disposition,
  "Site" = analysis_data$site,
  "ADD-RS" = ifelse(is.na(analysis_data$add_rs), "Missing", as.character(analysis_data$add_rs)),
  "D-dimer testing" = ifelse(d_dimer_obtained, "Obtained", "Absent/not obtained")
)

categorical_tests <- lapply(
  categorical_map,
  function(outcome) chisq_details(analysis_data$provider_group, outcome)
)
categorical_tests[["D-dimer result among obtained tests"]] <- d_dimer_result_test

for (characteristic in names(categorical_tests)) {
  rows <- table1$characteristic == characteristic
  table1$test_statistic[rows] <- categorical_tests[[characteristic]]["statistic"]
  table1$test_df[rows] <- categorical_tests[[characteristic]]["df"]
}

# Table 1 rows for a characteristic cross-tabulated by provider group, built on
# the layout of an existing row.
provider_stratified_rows <- function(template, characteristic, variable, levels) {
  count_level <- function(include) {
    vapply(levels, function(level) sum(include & variable == level), integer(1), USE.NAMES = FALSE)
  }

  rows <- template[rep(1L, length(levels)), ]
  rows$characteristic <- characteristic
  rows$level <- levels
  rows$overall_n <- count_level(TRUE)
  rows$overall_denominator <- n_encounters
  rows$overall_percent <- 100 * rows$overall_n / rows$overall_denominator
  rows$physician_n <- count_level(physician)
  rows$physician_denominator <- sum(physician)
  rows$physician_percent <- 100 * rows$physician_n / rows$physician_denominator
  rows$nonphysician_n <- count_level(nonphysician)
  rows$nonphysician_denominator <- sum(nonphysician)
  rows$nonphysician_percent <- 100 * rows$nonphysician_n / rows$nonphysician_denominator
  rows
}

# The grouped ESI rows use the same comparison.
esi_group_test <- chisq_details(analysis_data$provider_group, analysis_data$esi_group)
esi_groups <- provider_stratified_rows(
  table1[table1$characteristic == "Emergency Severity Index", ],
  "Emergency Severity Index grouped",
  analysis_data$esi_group,
  c("ESI 1-2", "ESI 3-4")
)
esi_groups$p_value <- esi_group_test["p"]
esi_groups$test_statistic <- esi_group_test["statistic"]
esi_groups$test_df <- esi_group_test["df"]
esi_groups$test_method <- "Pearson chi-square without continuity correction"

provider_role_rows <- table1$characteristic == "Provider role"
provider_groups <- provider_stratified_rows(
  table1[provider_role_rows, ],
  "Provider group",
  analysis_data$provider_group,
  c("Physician", "Non-physician", "Unknown")
)
provider_groups$p_value <- NA_real_
provider_groups$generating_object <- "analytic_data$provider_group"
provider_groups$test_method <- "Not applicable: these rows define the comparison groups"

table1 <- rbind(
  table1[provider_role_rows, ],
  provider_groups,
  table1[!provider_role_rows, ],
  esi_groups
)
table1$generating_script <- "R/05_descriptive_analysis.R; R/09_complete_results_handoff.R"
table1$missing_unknown <- 0L
table1$missing_unknown[table1$characteristic %in% c("Provider role", "Provider group")] <-
  sum(analysis_data$provider_group == "Unknown")
table1$missing_unknown[table1$characteristic == "Race"] <-
  sum(analysis_data$race_group %in% c("Unknown", "Missing"))
table1$missing_unknown[table1$characteristic == "ADD-RS"] <- sum(is.na(analysis_data$add_rs))
table1$result_status <- ifelse(
  table1$characteristic %in% c("D-dimer result among obtained tests"),
  "Provisional pending D-dimer cutoff confirmation",
  "Current R final under approved definitions"
)
write_output(table1, "02_complete_table1.csv")

# ---- D-dimer and algorithm ----------------------------------------------------
# I keep tested and classifiable denominators distinct when summarizing these results.

count_rows <- function(section, group, level, numerator, denominator, missing_unknown,
                       generating_script, generating_object) {
  denominator <- rep_len(denominator, length(numerator))

  data.frame(
    section = section, group = group, level = level,
    numerator = numerator, denominator = denominator,
    percent = ifelse(denominator > 0L, 100 * numerator / denominator, NA_real_),
    missing_unknown = missing_unknown,
    generating_script = generating_script, generating_object = generating_object,
    stringsAsFactors = FALSE
  )
}

groups <- list(
  Overall = rep(TRUE, n_encounters),
  Physician = physician,
  `Non-physician` = nonphysician,
  `Attending physician` = analysis_data$provider_role == "Attending physician",
  APP = analysis_data$provider_role == "APP"
)

# D-dimer results use tested encounters as the denominator. Concordant and
# discordant use classifiable encounters.
group_status_rows <- function(group, keep) {
  group_n <- sum(keep)
  tested_n <- sum(keep & d_dimer_obtained)
  classifiable_n <- sum(keep & classifiable)
  unclassifiable_n <- sum(keep & unclassifiable)

  rbind(
    count_rows(
      "D-dimer", group,
      c("Obtained", "Absent/not obtained", "Positive", "Negative", "Indeterminate"),
      numerator = c(
        tested_n,
        sum(keep & !d_dimer_obtained),
        sum(keep & analysis_data$d_dimer_status == "Positive"),
        sum(keep & analysis_data$d_dimer_status == "Negative"),
        sum(keep & analysis_data$d_dimer_status == "Indeterminate")
      ),
      denominator = c(group_n, group_n, tested_n, tested_n, tested_n),
      missing_unknown = 0L,
      generating_script = "R/04_variable_derivation.R",
      generating_object = "analytic_data$d_dimer_status"
    ),
    count_rows(
      "Algorithm", group,
      c("Classifiable", "Concordant", "Discordant", "Unclassifiable"),
      numerator = c(
        classifiable_n,
        sum(keep & analysis_data$algorithm_class == "Concordant"),
        sum(keep & discordant),
        unclassifiable_n
      ),
      denominator = c(group_n, classifiable_n, classifiable_n, group_n),
      missing_unknown = c(unclassifiable_n, 0L, 0L, unclassifiable_n),
      generating_script = "R/04_variable_derivation.R",
      generating_object = "analytic_data$algorithm_class"
    )
  )
}

discordance_decomposition_rows <- function(reason) {
  in_reason <- discordant & analysis_data$discordance_reason == reason

  count_rows(
    "Discordance decomposition", c("ADD-RS 0-1", "ADD-RS 0", "ADD-RS 1"), reason,
    numerator = c(
      sum(in_reason & analysis_data$add_rs %in% 0:1),
      sum(in_reason & analysis_data$add_rs == 0),
      sum(in_reason & analysis_data$add_rs == 1)
    ),
    denominator = sum(discordant),
    missing_unknown = 0L,
    generating_script = "R/04_variable_derivation.R",
    generating_object = "analytic_data$discordance_reason + analytic_data$add_rs"
  )
}

# Sensitivity classification: absent D-dimer at ADD-RS 0-1 becomes unclassifiable.
absent_sensitivity_class <- as.character(analysis_data$algorithm_class)
absent_low_risk <- !is.na(analysis_data$add_rs) &
  analysis_data$add_rs <= 1L &
  analysis_data$d_dimer_status == "Absent/not obtained"
absent_sensitivity_class[absent_low_risk] <- "Unclassifiable"
absent_sensitivity_unclassifiable_n <- sum(absent_sensitivity_class == "Unclassifiable")

dimer_algorithm <- rbind(
  do.call(rbind, Map(group_status_rows, names(groups), groups)),
  discordance_decomposition_rows("Low ADD-RS with absent D-dimer"),
  discordance_decomposition_rows("Low ADD-RS with negative D-dimer"),
  count_rows(
    "Absent-D sensitivity classification", "Overall",
    c("Classifiable", "Concordant", "Discordant", "Unclassifiable"),
    numerator = c(
      sum(absent_sensitivity_class %in% c("Concordant", "Discordant")),
      sum(absent_sensitivity_class == "Concordant"),
      sum(absent_sensitivity_class == "Discordant"),
      absent_sensitivity_unclassifiable_n
    ),
    denominator = n_encounters,
    missing_unknown = c(
      absent_sensitivity_unclassifiable_n, 0L, 0L, absent_sensitivity_unclassifiable_n
    ),
    generating_script = "R/07_sensitivity_analyses.R",
    generating_object = "observed_d_dimer_data$algorithm_class"
  )
)

# Physician versus non-physician tests, attached to the rows they compare.
dimer_algorithm$comparison_statistic <- NA_real_
dimer_algorithm$comparison_df <- NA_real_
dimer_algorithm$p_value <- NA_real_
dimer_algorithm$test_method <- NA_character_

provider_rows <- dimer_algorithm$group %in% c("Physician", "Non-physician")
provider_tests <- list(
  list(
    rows = dimer_algorithm$section == "D-dimer" &
      dimer_algorithm$level %in% c("Obtained", "Absent/not obtained"),
    test = chisq_details(analysis_data$provider_group, d_dimer_obtained)
  ),
  list(
    rows = dimer_algorithm$section == "D-dimer" &
      dimer_algorithm$level %in% c("Positive", "Negative", "Indeterminate"),
    test = d_dimer_result_test
  ),
  list(
    rows = dimer_algorithm$section == "Algorithm" &
      dimer_algorithm$level %in% c("Concordant", "Discordant"),
    test = chisq_details(
      analysis_data$provider_group[classifiable],
      analysis_data$algorithm_class[classifiable]
    )
  )
)

for (provider_test in provider_tests) {
  rows <- provider_rows & provider_test$rows
  dimer_algorithm$comparison_statistic[rows] <- provider_test$test["statistic"]
  dimer_algorithm$comparison_df[rows] <- provider_test$test["df"]
  dimer_algorithm$p_value[rows] <- provider_test$test["p"]
}

dimer_algorithm$test_method[!is.na(dimer_algorithm$p_value)] <-
  "Physician vs non-physician Pearson chi-square without continuity correction"
dimer_algorithm$denominator_definition <- ifelse(
  dimer_algorithm$section == "Discordance decomposition", "Primary discordant encounters",
  ifelse(
    dimer_algorithm$section == "Absent-D sensitivity classification", "Eligible encounters",
    ifelse(
      dimer_algorithm$section == "Algorithm" &
        dimer_algorithm$level %in% c("Concordant", "Discordant"),
      "Algorithm-classifiable encounters within group",
      ifelse(
        dimer_algorithm$section == "D-dimer" &
          dimer_algorithm$level %in% c("Positive", "Negative", "Indeterminate"),
        "D-dimer-tested encounters within group", "All encounters within group"
      )
    )
  )
)
dimer_algorithm$result_status <- ifelse(
  dimer_algorithm$section == "D-dimer" &
    dimer_algorithm$level %in% c("Obtained", "Absent/not obtained"),
  "Current R final under abstracted D-dimer ascertainment",
  "Provisional pending D-dimer cutoff confirmation"
)
write_output(dimer_algorithm, "03_d_dimer_and_algorithm.csv")

# ---- Yield comparisons --------------------------------------------------------
# I calculate yield among encounters with known outcomes for each specified comparison.

# AAS positivity among encounters in `group` with known outcome, restricted to
# the analysis population `include`, with an exact 95% CI.
yield_row <- function(analysis, stratum, group, include) {
  known <- include & group & known_outcome
  n <- sum(known)
  pos <- sum(known & aas_positive)
  ci <- binom_ci(pos, n)

  data.frame(
    analysis = analysis, stratum = stratum, numerator = pos,
    stratum_total_n = sum(include & group), known_outcome_n = n,
    denominator = n, percent = 100 * pos / n, ci_low_percent = ci[1L],
    ci_high_percent = ci[2L], missing_unknown = sum(include & group & !known_outcome),
    generating_script = "R/04_variable_derivation.R; R/05_descriptive_analysis.R",
    generating_object = "analytic_data$aas_positive; primary_yield_comparison",
    stringsAsFactors = FALSE
  )
}

# One row per level of `variable`, within the encounters at either level.
yield_by_level <- function(analysis, variable, levels) {
  do.call(rbind, lapply(levels, function(level) {
    yield_row(analysis, level, variable == level, variable %in% levels)
  }))
}

# Pearson chi-square of AAS outcome across the levels of `variable`.
yield_test <- function(variable, levels) {
  keep <- variable %in% levels & known_outcome
  suppressWarnings(stats::chisq.test(
    table(variable[keep], analysis_data$aas_positive[keep]),
    correct = FALSE
  ))
}

everyone <- rep(TRUE, n_encounters)
algorithm_levels <- c("Concordant", "Discordant")
provider_group_levels <- c("Physician", "Non-physician")
provider_role_levels <- c("Attending physician", "APP")

yield <- rbind(
  yield_row("Full cohort AAS", "All", everyone, everyone),
  yield_row("Classifiable-cohort AAS", "All", everyone, !unclassifiable),
  yield_by_level("Primary algorithm yield", analysis_data$algorithm_class, algorithm_levels),
  yield_by_level("Provider positivity", analysis_data$provider_group, provider_group_levels),
  yield_by_level("Attending vs APP positivity", analysis_data$provider_role, provider_role_levels),
  yield_by_level("Absent-D sensitivity yield", absent_sensitivity_class, algorithm_levels)
)

yield$comparison_statistic <- NA_real_
yield$comparison_df <- NA_real_
yield$p_value <- NA_real_
yield$test_method <- NA_character_

yield_tests <- list(
  "Primary algorithm yield" = list(
    test = yield_test(analysis_data$algorithm_class, algorithm_levels),
    method = "Pearson chi-square without continuity correction; difference CI is unpooled Wald"
  ),
  "Provider positivity" = list(
    test = yield_test(analysis_data$provider_group, provider_group_levels),
    method = "Pearson chi-square without continuity correction"
  ),
  "Attending vs APP positivity" = list(
    test = yield_test(analysis_data$provider_role, provider_role_levels),
    method = "Pearson chi-square without continuity correction"
  )
)

for (analysis in names(yield_tests)) {
  rows <- yield$analysis == analysis
  yield$comparison_statistic[rows] <- unname(yield_tests[[analysis]]$test$statistic)
  yield$comparison_df[rows] <- unname(yield_tests[[analysis]]$test$parameter)
  yield$p_value[rows] <- yield_tests[[analysis]]$test$p.value
  yield$test_method[rows] <- yield_tests[[analysis]]$method
}

primary_rows <- yield$analysis == "Primary algorithm yield"
absent_rows <- yield$analysis == "Absent-D sensitivity yield"

observed_dd <- sensitivity$yield_sensitivity_results[
  sensitivity$yield_sensitivity_results$analysis == "Absent D-dimer treated as unclassifiable", ]
yield$p_value[absent_rows] <- observed_dd$p_value
yield$test_method[absent_rows] <- "Fisher exact test; difference CI is unpooled Wald"

yield$absolute_difference_percentage_points <- NA_real_
yield$difference_ci_low <- NA_real_
yield$difference_ci_high <- NA_real_
yield$absolute_difference_percentage_points[primary_rows] <-
  descriptive$primary_yield_comparison$absolute_difference_percentage_points
yield$difference_ci_low[primary_rows] <-
  descriptive$primary_yield_comparison$ci_low_percentage_points
yield$difference_ci_high[primary_rows] <-
  descriptive$primary_yield_comparison$ci_high_percentage_points
yield$absolute_difference_percentage_points[absent_rows] <-
  observed_dd$absolute_difference_percentage_points
yield$difference_ci_low[absent_rows] <- observed_dd$ci_low_percentage_points
yield$difference_ci_high[absent_rows] <- observed_dd$ci_high_percentage_points
yield$generating_script[absent_rows] <- "R/07_sensitivity_analyses.R"
yield$generating_object[absent_rows] <- "observed_d_dimer_yield; yield_sensitivity_results"
yield$result_status <- ifelse(
  yield$analysis %in% c("Full cohort AAS", "Provider positivity", "Attending vs APP positivity"),
  "Current R final under approved provider/outcome definitions",
  "Provisional pending D-dimer cutoff confirmation"
)
write_output(yield, "04_diagnostic_yield_and_provider_positivity.csv")

# ---- Low acuity ---------------------------------------------------------------
# I describe the ESI 3-4 subgroup overall and within provider groups.

la_all <- analysis_data$esi_group == "ESI 3-4"

# Counts for the ESI 3-4 encounters in `keep`. The first row is the subgroup size.
low_acuity_rows <- function(stratum, keep, size_label) {
  keep_classifiable <- keep & !unclassifiable
  numerator <- c(
    sum(keep),
    sum(keep_classifiable),
    sum(keep & analysis_data$algorithm_class == "Concordant"),
    sum(keep & discordant),
    sum(keep & aas_positive, na.rm = TRUE),
    sum(keep_classifiable & aas_positive, na.rm = TRUE)
  )
  denominator <- c(
    rep(sum(keep), 2L),
    rep(sum(keep_classifiable), 2L),
    sum(keep & known_outcome),
    sum(keep_classifiable & known_outcome)
  )

  data.frame(
    metric = c(
      size_label, "Classifiable", "Concordant", "Discordant",
      "AAS positive among known full subgroup",
      "AAS positive among known classifiable subgroup"
    ),
    stratum = stratum,
    numerator = numerator,
    denominator = denominator,
    percent = 100 * numerator / denominator,
    missing_unknown = c(
      rep(0L, 4L),
      sum(keep & !known_outcome),
      sum(keep_classifiable & !known_outcome)
    ),
    stringsAsFactors = FALSE
  )
}

low_acuity_overall <- low_acuity_rows("Overall", la_all, "Total subgroup")
low_acuity_overall_only <- data.frame(
  metric = c("ESI 3", "ESI 4", "Unclassifiable"),
  stratum = "Overall",
  numerator = c(
    sum(analysis_data$esi_num == 3L),
    sum(analysis_data$esi_num == 4L),
    sum(la_all & unclassifiable)
  ),
  denominator = sum(la_all),
  stringsAsFactors = FALSE
)
low_acuity_overall_only$percent <-
  100 * low_acuity_overall_only$numerator / low_acuity_overall_only$denominator
low_acuity_overall_only$missing_unknown <- 0L

low_acuity <- rbind(
  low_acuity_overall[1L, ],
  low_acuity_overall_only[1:2, ],
  low_acuity_overall[2L, ],
  low_acuity_overall_only[3L, ],
  low_acuity_overall[3:6, ],
  low_acuity_rows("Physician", la_all & physician, "Provider group N"),
  low_acuity_rows("Non-physician", la_all & nonphysician, "Provider group N")
)
low_acuity$concordance_p_value <- sensitivity$low_acuity_results$concordance_p_value[1L]
low_acuity$yield_p_value <- sensitivity$low_acuity_results$yield_p_value[1L]
low_acuity$test_method <-
  "Physician vs non-physician Pearson chi-square without continuity correction"
low_acuity$generating_script <- "R/07_sensitivity_analyses.R; R/09_complete_results_handoff.R"
low_acuity$generating_object <- "low_acuity_results; analytic_data"
low_acuity$result_status <- ifelse(
  low_acuity$metric %in% c(
    "Total subgroup", "ESI 3", "ESI 4", "Provider group N",
    "AAS positive among known full subgroup"
  ),
  "Current R final under approved definitions",
  "Provisional pending D-dimer cutoff confirmation"
)
write_output(low_acuity, "05_low_acuity_subgroup.csv")

# ---- Alternative diagnoses ----------------------------------------------------
# I summarize recorded alternative-diagnosis flags among encounters with known negative AAS outcomes.

known_negative <- analysis_data$aas_outcome == "Negative"
none_n <- sum(known_negative & analysis_data$alternative_diagnosis_definite == 0L)
none_ci <- binom_ci(none_n, sum(known_negative))

alternative <- rbind(
  descriptive$table3_source[descriptive$table3_source$panel == "3A", ],
  data.frame(
    panel = "3A", category = "No clinically meaningful alternative diagnosis",
    numerator = none_n, denominator = sum(known_negative),
    percent = 100 * none_n / sum(known_negative),
    ci_low_percent = none_ci[1L], ci_high_percent = none_ci[2L], p_value = NA_real_,
    generating_object = "analytic_data$alternative_diagnosis_definite", stringsAsFactors = FALSE
  ),
  with(sensitivity$alternative_diagnosis_by_algorithm, data.frame(
    panel = "3B algorithm comparison", category = algorithm_class,
    numerator = alternative_diagnosis_n, denominator = aas_negative_denominator,
    percent = alternative_diagnosis_percent,
    ci_low_percent = NA_real_, ci_high_percent = NA_real_, p_value = p_value,
    generating_object = "alternative_diagnosis_by_algorithm", stringsAsFactors = FALSE
  ))
)
alternative$categories_mutually_exclusive <- FALSE
alternative$aas_outcome_unknown_excluded <- 3L
alternative$generating_script <- ifelse(
  alternative$panel == "3A", "R/05_descriptive_analysis.R", "R/07_sensitivity_analyses.R"
)
alternative$result_status <- ifelse(
  alternative$panel == "3A",
  "Current R under broad text-derived rules; no new blinded adjudication",
  "Provisional pending D-dimer cutoff confirmation; broad text-derived rules"
)
write_output(alternative, "06_alternative_diagnoses.csv")

# ---- Primary model ------------------------------------------------------------
# I carry the saved primary model estimates into the results ledger.

primary_model <- models$table4_source
primary_model$reference_levels <- "Male; ESI 1-2; Physician; STAT; Hospital D"
primary_model$model_outcome <- "Algorithm discordant (event=1) vs concordant"
primary_model$missing_excluded <- n_encounters - unique(primary_model$analytic_n)
primary_model$predictor_missing_after_class_provider_filter <- 0L
primary_model$converged <- TRUE
primary_model$aic <- models$primary_model_diagnostics$aic
primary_model$null_deviance <- models$primary_model_diagnostics$null_deviance
primary_model$residual_deviance <- models$primary_model_diagnostics$residual_deviance
primary_model$residual_df <- models$primary_model_diagnostics$degrees_freedom_residual
primary_model$c_statistic <- models$primary_model_diagnostics$c_statistic
primary_model$ci_method <- "Wald log-odds coefficient +/- 1.96 model-based SE"
primary_model$p_method <- "Two-sided Wald z test"
primary_model$generating_script <- "R/06_primary_models.R"
primary_model$result_status <- "Provisional pending D-dimer cutoff confirmation"
write_output(primary_model, "07_primary_regression_complete.csv")

# ---- Cluster/model/yield sensitivity ------------------------------------------
# I bring together the saved clustering and sensitivity estimates for comparison.

n_clusters <- function(cluster_type) {
  models$cluster_summary$n_clusters[models$cluster_summary$cluster_type == cluster_type]
}

random_intercept_variance <- function(group) {
  models$random_effect_variance$vcov[models$random_effect_variance$grp == group]
}

clustered <- models$clustered_model_estimates
clustered$analysis <- clustered$covariance
clustered$analytic_n <- models$primary_model_diagnostics$analytic_n
clustered$discordant_events <- models$primary_model_diagnostics$discordant_events
clustered$concordant_events <- models$primary_model_diagnostics$concordant_events
clustered$generating_script <- "R/06_primary_models.R"

model_sens <- sensitivity$model_sensitivity_results
model_sens$covariance <- "Model-based"
model_sens$generating_script <- "R/07_sensitivity_analyses.R"

all_sens_names <- union(names(clustered), names(model_sens))
fill <- function(x) {
  for (nm in setdiff(all_sens_names, names(x))) x[[nm]] <- NA
  x[all_sens_names]
}

model_sensitivity <- rbind(fill(clustered), fill(model_sens))
model_sensitivity <- model_sensitivity[model_sensitivity$term != "(Intercept)", ]
model_sensitivity$ci_method <- "Wald log-odds coefficient +/- 1.96 SE"
model_sensitivity$p_method <- "Two-sided Wald z test"
model_sensitivity$result_status <- "Provisional pending D-dimer cutoff confirmation"
model_sensitivity$patient_clusters <- n_clusters("Patient")
model_sensitivity$provider_clusters <- n_clusters("Provider")
model_sensitivity$site_clusters <- n_clusters("Site")
model_sensitivity$random_intercept_status <- models$mixed_model_status
model_sensitivity$patient_random_intercept_variance <- random_intercept_variance("patient_empi")
model_sensitivity$provider_random_intercept_variance <-
  random_intercept_variance("primary_provider_id")
write_output(model_sensitivity, "08_regression_and_clustering_sensitivities.csv")

# ---- Historical economic reconciliation ---------------------------------------
# I retain the older economic results as a reconciliation record alongside their source.
# These values come from the older model retained in R/07.
# Current manuscript economic results come from R/14, R/15, and R/16.

economic_record <- function(record_type, item, value, source,
                            lower_input = NA_character_, base_input = NA_character_,
                            upper_input = NA_character_, net_savings_lower_input = NA_real_,
                            net_savings_base = NA_real_, net_savings_upper_input = NA_real_,
                            sensitivity_range = NA_real_) {
  data.frame(
    record_type = record_type, item = item, value = value, source = source,
    lower_input = lower_input, base_input = base_input, upper_input = upper_input,
    net_savings_lower_input = net_savings_lower_input, net_savings_base = net_savings_base,
    net_savings_upper_input = net_savings_upper_input, sensitivity_range = sensitivity_range,
    stringsAsFactors = FALSE
  )
}

economic <- rbind(
  economic_record(
    "Input", sensitivity$economic_inputs$input, sensitivity$economic_inputs$value,
    sensitivity$economic_inputs$source
  ),
  economic_record(
    "Base-case output", sensitivity$economic_base_results$measure,
    sensitivity$economic_base_results$value,
    "R/07_sensitivity_analyses.R; economic_base_results"
  ),
  economic_record(
    "Standardized output", "Discordant standardized expenditure percentage of total",
    100 * 519435 / 709835, "R/07_sensitivity_analyses.R; economic_base_results"
  ),
  with(sensitivity$table5_source, economic_record(
    "One-way sensitivity", parameter, NA_real_, "R/07_sensitivity_analyses.R; table5_source",
    lower_input = lower_input_label, base_input = base_input_label,
    upper_input = upper_input_label, net_savings_lower_input = net_savings_lower_input,
    net_savings_base = net_savings_base, net_savings_upper_input = net_savings_upper_input,
    sensitivity_range = sensitivity_range
  ))
)
economic$result_status <- ifelse(
  economic$record_type == "Input" & economic$item == "Eligible encounters",
  "Observed current R count under approved cohort definition",
  ifelse(
    economic$record_type == "Input" & economic$item %in% c(
      "Algorithm-discordant encounters", "Known AAS prevalence among discordant encounters"
    ),
    "Observed current R quantity provisional pending D-dimer cutoff confirmation",
    ifelse(
      economic$record_type == "Input", "Modeled input; provenance not fully locked",
      "Provisional modeled result; D-dimer-dependent discordance and economic-input provenance"
    )
  )
)
write_output(economic, "09_economic_analysis_complete.csv")

# ---- Manuscript change table ------------------------------------------------
# I record the manuscript changes tied to this results handoff.

changes <- data.frame(
  result = c(
    "Cohort N", "Unique patients", "Attending physician", "Resident physician", "APP",
    "Physician", "Non-physician", "Unknown provider", "D-dimer tested",
    "D-dimer positive", "D-dimer negative", "ADD-RS distribution",
    "Algorithm classifiable", "Algorithm concordant", "Algorithm discordant",
    "AAS positive", "Concordant yield", "Discordant yield", "Yield difference/CI/P",
    "Physician positivity", "Attending positivity", "Alternative diagnosis total",
    primary_model$comparison,
    "Gross expenditure", "Discordant expenditure", "Base-case modeled savings"
  ),
  august_2_value = c(
    "1,194", "Not reported as unique patients", "957", "119", "118", "1,076", "118", "0",
    "127/1,194", "112/127", "15/127", "0:372; 1:550; 2:212; 3:19; missing:41",
    "1,153", "317", "836", "80/1,194", "34/317 (10.7%)", "42/836 (5.0%)",
    "5.7 points (95% CI 2.0-9.4; P<.001)", "70/1,076 (6.5%; P=.42)",
    "66/957 (6.9%; historical displayed denominator conflicted; P=.53)", "199/1,114 (17.9%)",
    "OR 1.05 (0.96-1.15; P=.26)", "OR 0.84 (0.45-1.10; P=.21)",
    "OR 1.47 (1.13-1.93; P=.005)", "OR 1.19 (0.75-1.89; P=.46)",
    "OR 1.32 (0.95-1.84; P=.10)", "OR 0.41 (0.27-0.62; P<.001)",
    "OR 0.82 (0.60-1.10; P=.182)", "OR 0.59 (0.35-0.97; P=.04)",
    "$710,430", "$497,420", "$108,954"
  ),
  stringsAsFactors = FALSE
)

changes$current_r_value <- c(
  "1,193 encounters", "1,120", "957", "120", "115", "1,077", "115", "1",
  "126/1,193", "71/126", "55/126", "0:373; 1:550; 2:211; 3:19; missing:40",
  "1,153", "280", "873", "79/1,190 known; 3 unknown", "33/279 (11.8%)", "42/872 (4.8%)",
  "7.011 points (95% CI 2.964-11.059; P=.00003625)", "69/1,074 known (6.4%; P=.353)",
  "65/954 known (6.8%; P=.455)", "200/1,111 (18.0%)",
  sprintf("OR %.4f (%.4f-%.4f; P=%.6g)", primary_model$odds_ratio, primary_model$ci_low, primary_model$ci_high, primary_model$p_value),
  "$709,835", "$519,435", "$117,162.76"
)

changes$reason_for_change <- c(
  "Age-16 encounter excluded", "Encounter-level cohort now distinguishes repeat patients",
  "Approved earliest-order attribution", "Approved earliest-order attribution", "Approved earliest-order attribution",
  "Approved earliest-order attribution", "Approved earliest-order attribution", "No informative role",
  "Age-16 encounter excluded", "Comparator-aware parsing and provisional 500 FEU cutoff",
  "Comparator-aware parsing and provisional 500 FEU cutoff",
  "Age exclusion reduces score 2 by one; consensus recovery moves one missing score to 0",
  "Unclassifiable decreases by one while cohort decreases by one", "D-dimer reclassification",
  "D-dimer reclassification", "Age-16 AAS-positive excluded; missing outcomes retained unknown",
  "D-dimer reclassification and unknown outcomes excluded", "Known-outcome denominator",
  "Updated class allocation and denominators", "Adult known-outcome denominator",
  "Adult known-outcome denominator and corrected attending denominator", "Adult known-negative denominator and broad-net rules",
  rep("Primary model refit after D-dimer-dependent outcome reclassification; unknown provider excluded", nrow(primary_model)),
  "1,193 x unchanged $595", "873 x unchanged $595", "Updated discordant N and prevalence"
)
changes$final_or_provisional <- c(
  rep("Current R final under approved definitions", 9L),
  rep("Provisional pending D-dimer cutoff confirmation", 2L),
  "Current R final under approved ADD-RS definition",
  rep("Provisional pending D-dimer cutoff confirmation", 3L),
  "Current R final; 3 outcomes unknown",
  rep("Provisional pending D-dimer cutoff confirmation", 3L),
  rep("Current R final under approved provider/outcome definitions", 3L),
  rep("Provisional pending D-dimer cutoff confirmation", nrow(primary_model)),
  "Provisional standardized expenditure: unit-price provenance",
  "Provisional: D-dimer-dependent count and unit-price provenance",
  "Provisional modeled: D-dimer-dependent count and economic-input provenance"
)
changes$generating_script <- c(
  rep("R/03_cohort_construction.R", 2L), rep("R/04_variable_derivation.R", 14L),
  rep("R/05_descriptive_analysis.R", 6L), rep("R/06_primary_models.R", nrow(primary_model)),
  rep("R/07_sensitivity_analyses.R", 3L)
)
changes$generating_object <- c(
  "analytic_data", "analytic_data$patient_empi", rep("analytic_data", 14L),
  "primary_yield_comparison", "primary_yield_comparison", "primary_yield_comparison",
  "provider_yield_comparison", "provider_yield_comparison", "table3_source",
  rep("table4_source", nrow(primary_model)), rep("economic_base_results", 3L)
)
write_output(changes, "10_manuscript_change_table.csv")

# ---- Handoff document -------------------------------------------------------
# I assemble the detailed handoff from the ledger and saved analysis results.

observed_classifiable <- sum(
  ifelse(
    analysis_data$add_rs <= 1L & analysis_data$d_dimer_status == "Absent/not obtained",
    "Unclassifiable", analysis_data$algorithm_class
  ) %in% c("Concordant", "Discordant")
)

handoff <- c(
  "# Complete Results-Section Statistical Handoff",
  "",
  "All primary observations are eligible CTA encounters, not unique patients. This document uses the completed R pipeline on `codex/r-rebuild-first-pass`. A fresh stage-07 run was completed before export. No cohort, provider, ADD-RS, D-dimer, outcome, model, or economic definition was changed.",
  "",
  "> **Provisional D-dimer rule:** D-dimer classification uses the current 500 FEU implementation. FEU units are externally confirmed for Emory, but the historical/reference cutoff is not formally locked. All D-dimer status, algorithm class, yield-by-algorithm, discordance regression, and discordant-count economic results remain provisional pending cutoff confirmation.",
  "",
  "## 1. Cohort and analytic unit", "", md_table(cohort),
  "The 56 repeat patients contribute 129 encounters in total, including 73 encounters beyond one per patient. The full cohort has 1,190 known AAS outcomes and 3 unknown. Algorithm class and AAS outcome are different missingness dimensions.", "",
  "## 2. Complete Table 1 baseline characteristics", "",
  "Percentages use the displayed overall or provider-group denominator. P values compare physician with nonphysician encounters only; the one unknown provider is excluded from tests and is not cross-tabulated to avoid disclosing a one-encounter clinical profile. Age is summarized only as mean (SD) in the current output and uses a Welch two-sample t test. Categorical tests are Pearson chi-square tests without continuity correction. ESI 5 is not represented and is not a current analytic category. The manuscript Table 1 disposition is `hospital_disposition`, derived from `Discharge Disposition DESC`, not `ED Disposition`.", "",
  md_table(table1[, c("section", "characteristic", "level", "overall_n", "overall_denominator", "overall_percent", "physician_n", "physician_denominator", "physician_percent", "nonphysician_n", "nonphysician_denominator", "nonphysician_percent", "missing_unknown", "p_value", "test_statistic", "test_df", "overall_mean", "overall_sd", "physician_mean", "physician_sd", "nonphysician_mean", "nonphysician_sd", "generating_script", "generating_object", "result_status")]),
  "## 3. ADD-RS distribution", "",
  "The complete overall and provider-stratified ADD-RS rows appear above. Counts are 373/1,193 score 0; 550/1,193 score 1; 211/1,193 score 2; 19/1,193 score 3; and 40/1,193 missing. Relative to August 2, consensus recovery moved one historical missing case to ADD-RS 0 and algorithm-discordant; adult exclusion removed one score-2 case. Thus missing changed 41 to 40, score 0 changed 372 to 373, and score 2 changed 212 to 211.", "",
  "## 4-5. D-dimer and algorithm classification", "", md_table(dimer_algorithm),
  "Current physician/nonphysician D-dimer testing comparison: Pearson chi-square P=0.364175. Among tested results: P=0.390658. Algorithm class comparison among classifiable encounters: P=0.768948. Concordant and discordant percentages use classifiable encounters; classifiable and unclassifiable percentages use all encounters in the corresponding provider group. Detailed attending-versus-APP D-dimer counts are included above; this was calculated for the handoff using the approved groups, not a new sensitivity definition.", "",
  "## 6. Diagnostic yield and provider positivity", "", md_table(yield),
  "Primary difference: 7.011 percentage points (unpooled Wald 95% CI 2.964 to 11.059); Pearson chi-square without correction, statistic 17.058, 1 df, P=0.00003625. Exact binomial CIs for each proportion are included in the CSV/table above.", "",
  "## 7. Low-acuity ESI 3-4 subgroup", "", md_table(low_acuity),
  "The subgroup has 670 encounters: 660 ESI 3 and 10 ESI 4. There are 649 classifiable and 21 unclassifiable encounters. Among classifiable encounters with known outcomes, AAS positivity is 23/571 for physicians and 6/75 for nonphysicians. Physician/nonphysician concordance P=0.850111 and classifiable AAS positivity P=0.118331. The one unknown-provider encounter is included only in overall rows and excluded from both comparisons.", "",
  "## 8. Alternative diagnoses", "", md_table(alternative),
  "Individual categories are nonmutually exclusive. The any/no-any rows are complementary within 1,111 known AAS-negative encounters. The any-diagnosis interval is exact Clopper-Pearson. Algorithm comparison P=0.000354945 (Pearson chi-square without continuity correction). These text-derived categories have not undergone new blinded adjudication.", "",
  "## 9. Primary regression", "", md_table(primary_model),
  "Model: binomial-logit GLM; outcome discordant=1. Reference levels are male, ESI 1-2, physician, STAT, and Hospital D. Model-based covariance; two-sided Wald z tests and Wald 95% CIs. Analytic N=1,152 (872 discordant, 280 concordant). Excluded from the 1,193 cohort: 40 algorithm-unclassifiable and 1 unknown provider; no further predictor missingness. The GLM converged. AIC 1274.564; null deviance 1277.744; residual deviance 1256.564 on 1,143 df; c statistic 0.594860.", "",
  "## 10. Regression and clustering sensitivity", "", md_table(model_sensitivity),
  paste0("Patient clusters: ", models$cluster_summary$n_clusters[models$cluster_summary$cluster_type == "Patient"], "; provider clusters: ", models$cluster_summary$n_clusters[models$cluster_summary$cluster_type == "Provider"], "; sites: 4. Patient/provider estimates retain the primary coefficients and substitute one-way CR1 covariance. Site-clustered inference was not used because four clusters are insufficient. Random-intercept status: ", models$mixed_model_status, ". Patient variance=", fmt(models$random_effect_variance$vcov[models$random_effect_variance$grp == "patient_empi"]), "; provider variance=", fmt(models$random_effect_variance$vcov[models$random_effect_variance$grp == "primary_provider_id"]), "."), "",
  "## 11. Absent-D-dimer sensitivity", "",
  paste0("Treating absent D-dimer as unclassifiable yields classifiable N=", observed_classifiable, ": concordant 280 total (33/279 known-outcome positive, 11.828%) and discordant 48 (0/48, 0%). Difference 11.828 points (Wald 95% CI 8.039 to 15.617; Fisher exact P=0.007460). The direction of the primary yield contrast persists, but the classifiable cohort falls from 1,153 to 328. This diagnostic sensitivity isolates observed negative-D-dimer discordance and does not validate the provisional cutoff."), "",
  "## 12. Provider-attribution sensitivity", "",
  "No formal alternative provider-attribution sensitivity was generated. The primary rule remains earliest CTA-component order, with recognized-role fallback for the same person when `No Access` is noninformative. The attending-versus-APP comparison is a prespecified secondary provider comparison, not an attribution-rule sensitivity.", "",
  "## 13. Economic analysis", "", md_table(economic),
  "Observed cohort counts are 1,193 eligible and 873 algorithm-discordant encounters. Prices and all decision-model results are standardized or modeled, not observed payments or avoidable expenditure. Discordant standardized expenditure is 73.18% of total. The decision model applies D-dimer to all 873 discordant encounters. D-dimer cost was not varied. The Asha-Miers base case uses sensitivity 0.98 and specificity 0.419; the Watanabe source uses 0.952 and 0.604.", "",
  "## 14. Manuscript-change table", "", md_table(changes),
  "## 15. Integrity checks", "",
  "- Counts were recomputed from the fresh `aas_analytic_encounters.rds` and current stage 05-07 result objects.",
  "- Every displayed percentage was recomputed from its exported numerator and denominator where applicable.",
  "- Overall provider roles sum to 1,193; physician/nonphysician/unknown sum to 1,193; each categorical Table 1 characteristic sums to its denominator; ADD-RS sums to 1,193; D-dimer status sums to 1,193; algorithm class sums to 1,193.",
  "- Full cohort (1,193), known-AAS cohort (1,190), algorithm-classifiable cohort (1,153), known-outcome classifiable cohort (1,151), and primary-model cohort (1,152) remain distinct.",
  "- Primary yield denominators are 279 and 872 rather than class totals 280 and 873 because each class contains one unknown AAS outcome.",
  "- No historical SAS value was substituted for a current R result.",
  "",
  "## Items not currently generated or not final", "",
  "- No formal alternative provider-attribution model was performed.",
  "- No site-clustered SE model is reported because four site clusters are insufficient for reliable cluster-robust inference.",
  "- The random-intercept model converged numerically but is unstable because the patient variance is extreme and most patients contribute only one encounter.",
  "- D-dimer-dependent results remain provisional pending historical cutoff confirmation.",
  "- Alternative-diagnosis categories remain dependent on broad text rules rather than new blinded clinical adjudication.",
  "- Economic results remain modeled and provisional because CTA price, D-dimer cost/performance, and missed-diagnosis-cost provenance are not fully locked."
)

writeLines(handoff, "docs/complete_results_handoff.md", useBytes = TRUE)

# ---- Validation -------------------------------------------------------------
# I check the handoff outputs and key table values before finishing.

stopifnot(
  nrow(analysis_data) == 1193L,
  length(unique(analysis_data$patient_empi)) == 1120L,
  sum(table(analysis_data$patient_empi) > 1L) == 56L,
  sum(table(analysis_data$provider_role)) == 1193L,
  sum(table(analysis_data$d_dimer_status)) == 1193L,
  sum(table(analysis_data$algorithm_class)) == 1193L,
  sum(table(analysis_data$aas_outcome)) == 1193L,
  nrow(primary_model) == 8L,
  nrow(changes) == 33L,
  length(list.files(output_dir, pattern = "\\.csv$")) == 10L
)

for (x in list(cohort, dimer_algorithm, yield, low_acuity, alternative)) {
  keep <- !is.na(x$numerator) & !is.na(x$denominator) & x$denominator > 0L & "percent" %in% names(x)
  stopifnot(all(abs(x$percent[keep] - 100 * x$numerator[keep] / x$denominator[keep]) < 1e-10))
}
