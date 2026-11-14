# Why this script is here
# I summarize the cohort, algorithm classification, diagnostic yield, and providers.
# These summaries describe encounters that received CTA.

# 05_descriptive_analysis.R
# AAS CTA reanalysis, stage 6: descriptive analysis and aggregate table sources.
#
# Input: analytic_data from R/04_variable_derivation.R.
# Output objects: table1_source, table2_source, table3_source,
# primary_yield_comparison and provider_yield_comparison.

source("R/04_variable_derivation.R")

analysis_data <- analytic_data

message("\nDescriptive-analysis cohort loaded: ", nrow(analysis_data), " encounters.")

# ---- Summary helpers ----------------------------------------------------------
# These helpers keep numerators, denominators, and provider groups together in the table sources.

provider_groups <- c("Physician", "Non-physician", "Unknown")

# Pearson chi-square P value without continuity correction.
chisq_p <- function(counts) {
  suppressWarnings(stats::chisq.test(counts, correct = FALSE)$p.value)
}

# Physician versus non-physician comparison of a categorical outcome. Outcome
# levels with no observations are dropped before testing.
safe_chisq_p <- function(group, outcome) {
  keep <- !is.na(group) & !is.na(outcome) & group %in% c("Physician", "Non-physician")

  comparison <- table(
    factor(group[keep], levels = c("Physician", "Non-physician")),
    outcome[keep]
  )
  comparison <- comparison[, colSums(comparison) > 0L, drop = FALSE]

  if (nrow(comparison) < 2L || ncol(comparison) < 2L) {
    return(NA_real_)
  }

  chisq_p(comparison)
}

categorical_table_rows <- function(
  data,
  section,
  characteristic,
  variable,
  levels,
  denominator_filter = rep(TRUE, nrow(data)),
  p_value = NULL
) {
  if (is.null(p_value)) {
    p_value <- safe_chisq_p(
      data$provider_group[denominator_filter],
      variable[denominator_filter]
    )
  }

  if (length(levels) == 0L) {
    return(NULL)
  }

  overall_denominator <- sum(denominator_filter)

  rows <- lapply(
    seq_along(levels),
    function(j) {
      level <- levels[j]
      matches_level <- variable == level
      overall_n <- sum(denominator_filter & matches_level, na.rm = TRUE)

      group_values <- lapply(
        provider_groups,
        function(provider_group_value) {
          group_filter <- denominator_filter & data$provider_group == provider_group_value
          denominator <- sum(group_filter)
          n <- sum(group_filter & matches_level, na.rm = TRUE)

          c(
            n = n,
            denominator = denominator,
            percent = ifelse(denominator > 0L, 100 * n / denominator, NA_real_)
          )
        }
      )

      data.frame(
        section = section,
        characteristic = characteristic,
        level = level,
        summary_type = "n_percent",
        overall_n = overall_n,
        overall_denominator = overall_denominator,
        overall_percent = 100 * overall_n / overall_denominator,
        physician_n = group_values[[1L]]["n"],
        physician_denominator = group_values[[1L]]["denominator"],
        physician_percent = group_values[[1L]]["percent"],
        nonphysician_n = group_values[[2L]]["n"],
        nonphysician_denominator = group_values[[2L]]["denominator"],
        nonphysician_percent = group_values[[2L]]["percent"],
        unknown_provider_n = group_values[[3L]]["n"],
        unknown_provider_denominator = group_values[[3L]]["denominator"],
        unknown_provider_percent = group_values[[3L]]["percent"],
        p_value = ifelse(j == 1L, p_value, NA_real_),
        generating_object = "table1_source",
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, rows)
}

continuous_table_row <- function(data, section, characteristic, variable) {
  group_summary <- lapply(
    provider_groups,
    function(provider_group_value) {
      values <- variable[data$provider_group == provider_group_value]
      values <- values[!is.na(values)]
      c(n = length(values), mean = mean(values), sd = stats::sd(values))
    }
  )

  known_provider <- data$provider_group %in% c("Physician", "Non-physician")

  p_value <- stats::t.test(
    variable[known_provider] ~ data$provider_group[known_provider]
  )$p.value

  values <- variable[!is.na(variable)]

  data.frame(
    section = section,
    characteristic = characteristic,
    level = "Mean (SD)",
    summary_type = "mean_sd",
    overall_n = length(values),
    overall_denominator = length(values),
    overall_percent = NA_real_,
    overall_mean = mean(values),
    overall_sd = stats::sd(values),
    physician_n = group_summary[[1L]]["n"],
    physician_denominator = group_summary[[1L]]["n"],
    physician_percent = NA_real_,
    physician_mean = group_summary[[1L]]["mean"],
    physician_sd = group_summary[[1L]]["sd"],
    nonphysician_n = group_summary[[2L]]["n"],
    nonphysician_denominator = group_summary[[2L]]["n"],
    nonphysician_percent = NA_real_,
    nonphysician_mean = group_summary[[2L]]["mean"],
    nonphysician_sd = group_summary[[2L]]["sd"],
    unknown_provider_n = group_summary[[3L]]["n"],
    unknown_provider_denominator = group_summary[[3L]]["n"],
    unknown_provider_percent = NA_real_,
    unknown_provider_mean = group_summary[[3L]]["mean"],
    unknown_provider_sd = group_summary[[3L]]["sd"],
    p_value = p_value,
    generating_object = "table1_source",
    stringsAsFactors = FALSE
  )
}

# rbind for data frames with different columns. Absent columns are filled with NA.
bind_rows_fill <- function(data_frames) {
  all_names <- unique(unlist(lapply(data_frames, names)))

  completed <- lapply(
    data_frames,
    function(x) {
      for (name in setdiff(all_names, names(x))) {
        x[[name]] <- NA
      }
      x[all_names]
    }
  )

  do.call(rbind, completed)
}

# Count, percentage and exact (Clopper-Pearson) 95% CI.
binomial_summary <- function(events, denominator) {
  if (denominator == 0L) {
    return(c(
      numerator = events,
      denominator = denominator,
      percent = NA_real_,
      ci_low_percent = NA_real_,
      ci_high_percent = NA_real_
    ))
  }

  interval <- stats::binom.test(events, denominator)$conf.int

  c(
    numerator = events,
    denominator = denominator,
    percent = 100 * events / denominator,
    ci_low_percent = 100 * interval[1L],
    ci_high_percent = 100 * interval[2L]
  )
}

# ---- Table 1 source -----------------------------------------------------------
# I summarize encounter characteristics overall and by provider group.

add_rs_display <- ifelse(
  is.na(analysis_data$add_rs),
  "Missing",
  as.character(analysis_data$add_rs)
)

d_dimer_obtained_display <- ifelse(
  analysis_data$d_dimer_obtained,
  "Obtained",
  "Absent/not obtained"
)

table1_parts <- list(
  categorical_table_rows(
    analysis_data, "Provider", "Provider role",
    analysis_data$provider_role,
    c("Attending physician", "Resident physician", "APP", "Unknown"),
    p_value = NA_real_
  ),
  continuous_table_row(analysis_data, "Demographics", "Age, years", analysis_data$age),
  categorical_table_rows(
    analysis_data, "Demographics", "Sex",
    analysis_data$sex,
    c("Male", "Female")
  ),
  categorical_table_rows(
    analysis_data, "Demographics", "Race",
    analysis_data$race_group,
    c("Black", "White", "Asian", "Other/Multiracial", "Unknown", "Missing")
  ),
  categorical_table_rows(
    analysis_data, "Clinical", "Order priority",
    analysis_data$order_priority,
    c("STAT", "Non-STAT", "Missing")
  ),
  categorical_table_rows(
    analysis_data, "Clinical", "Emergency Severity Index",
    ifelse(is.na(analysis_data$esi_num), "Missing", as.character(analysis_data$esi_num)),
    c("1", "2", "3", "4", "Missing")
  ),
  categorical_table_rows(
    analysis_data, "Clinical", "Chief concern",
    analysis_data$chief_concern,
    c("Chest pain", "Back/flank pain", "SOB/respiratory", "Other/not recorded")
  ),
  categorical_table_rows(
    analysis_data, "Clinical", "Hospital disposition",
    analysis_data$hospital_disposition,
    c("Discharged home", "Transferred/other", "Expired", "Left AMA")
  ),
  categorical_table_rows(
    analysis_data, "Clinical", "Site",
    analysis_data$site,
    c("Hospital A", "Hospital B", "Hospital C", "Hospital D")
  ),
  categorical_table_rows(
    analysis_data, "Risk stratification", "ADD-RS",
    add_rs_display,
    c("0", "1", "2", "3", "Missing")
  ),
  categorical_table_rows(
    analysis_data, "D-dimer", "D-dimer testing",
    d_dimer_obtained_display,
    c("Obtained", "Absent/not obtained")
  ),
  categorical_table_rows(
    analysis_data, "D-dimer", "D-dimer result among obtained tests",
    analysis_data$d_dimer_status,
    c("Positive", "Negative", "Indeterminate"),
    denominator_filter = analysis_data$d_dimer_obtained
  )
)

table1_source <- bind_rows_fill(table1_parts)
rownames(table1_source) <- NULL

# ---- Table 2 source: algorithm concordance and yield --------------------------
# I compare yield among classified encounters with known outcomes, using the denominators shown in the output.

algorithm_panel_rows <- function(data, panel, subgroup_label) {
  classifiable <- data$algorithm_class %in% c("Concordant", "Discordant")
  panel_data <- data[classifiable, , drop = FALSE]

  strata <- list(
    Total = rep(TRUE, nrow(panel_data)),
    Physician = panel_data$provider_group == "Physician",
    `Non-physician` = panel_data$provider_group == "Non-physician",
    `Unknown provider` = panel_data$provider_group == "Unknown"
  )

  result <- do.call(
    rbind,
    lapply(
      names(strata),
      function(stratum) {
        keep <- strata[[stratum]]
        known_outcome <- keep & !is.na(panel_data$aas_positive)

        data.frame(
          panel = panel,
          subgroup = subgroup_label,
          stratum = stratum,
          n_classifiable = sum(keep),
          concordant_n = sum(keep & panel_data$algorithm_class == "Concordant"),
          discordant_n = sum(keep & panel_data$algorithm_class == "Discordant"),
          aas_positive_n = sum(panel_data$aas_positive[known_outcome]),
          aas_known_denominator = sum(known_outcome),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  result$concordant_percent <- 100 * result$concordant_n / result$n_classifiable
  result$discordant_percent <- 100 * result$discordant_n / result$n_classifiable
  result$aas_positive_percent <- 100 * result$aas_positive_n / result$aas_known_denominator

  provider_keep <- panel_data$provider_group %in% c("Physician", "Non-physician")
  provider_factor <- function(keep) {
    factor(panel_data$provider_group[keep], levels = c("Physician", "Non-physician"))
  }

  concordance_table <- table(
    provider_factor(provider_keep),
    factor(panel_data$algorithm_class[provider_keep], levels = c("Concordant", "Discordant"))
  )

  yield_keep <- provider_keep & !is.na(panel_data$aas_positive)
  yield_table <- table(
    provider_factor(yield_keep),
    factor(panel_data$aas_positive[yield_keep], levels = c(0L, 1L))
  )

  result$concordance_p_value <- chisq_p(concordance_table)
  result$aas_yield_p_value <- chisq_p(yield_table)

  result
}

table2_panel_a <- algorithm_panel_rows(analysis_data, "2A", "Full classifiable cohort")

table2_panel_b <- algorithm_panel_rows(
  analysis_data[analysis_data$esi_group == "ESI 3-4", , drop = FALSE],
  "2B",
  "ESI 3-4 classifiable subgroup"
)

known_classified_outcome <-
  analysis_data$algorithm_class %in% c("Concordant", "Discordant") &
  !is.na(analysis_data$aas_positive)

yield_counts <- table(
  factor(
    analysis_data$algorithm_class[known_classified_outcome],
    levels = c("Concordant", "Discordant")
  ),
  factor(analysis_data$aas_positive[known_classified_outcome], levels = c(0L, 1L))
)

concordant_positive <- yield_counts["Concordant", "1"]
concordant_denominator <- sum(yield_counts["Concordant", ])
discordant_positive <- yield_counts["Discordant", "1"]
discordant_denominator <- sum(yield_counts["Discordant", ])

# Risk difference with an unpooled Wald interval.
concordant_rate <- concordant_positive / concordant_denominator
discordant_rate <- discordant_positive / discordant_denominator
yield_difference <- concordant_rate - discordant_rate
yield_difference_se <- sqrt(
  concordant_rate * (1 - concordant_rate) / concordant_denominator +
    discordant_rate * (1 - discordant_rate) / discordant_denominator
)

yield_test <- stats::prop.test(
  c(concordant_positive, discordant_positive),
  c(concordant_denominator, discordant_denominator),
  correct = FALSE
)

primary_yield_comparison <- data.frame(
  concordant_positive = concordant_positive,
  concordant_denominator = concordant_denominator,
  concordant_percent = 100 * concordant_rate,
  discordant_positive = discordant_positive,
  discordant_denominator = discordant_denominator,
  discordant_percent = 100 * discordant_rate,
  absolute_difference_percentage_points = 100 * yield_difference,
  ci_low_percentage_points = 100 * (yield_difference - 1.96 * yield_difference_se),
  ci_high_percentage_points = 100 * (yield_difference + 1.96 * yield_difference_se),
  p_value = yield_test$p.value,
  generating_object = "primary_yield_comparison",
  stringsAsFactors = FALSE
)

table2_panel_c <- data.frame(
  panel = "2C",
  subgroup = "AAS yield by algorithm class",
  stratum = c("Concordant", "Discordant"),
  n_classifiable = c(concordant_denominator, discordant_denominator),
  concordant_n = c(concordant_denominator, NA_integer_),
  discordant_n = c(NA_integer_, discordant_denominator),
  aas_positive_n = c(concordant_positive, discordant_positive),
  aas_known_denominator = c(concordant_denominator, discordant_denominator),
  concordant_percent = c(100, NA_real_),
  discordant_percent = c(NA_real_, 100),
  aas_positive_percent = c(
    100 * concordant_positive / concordant_denominator,
    100 * discordant_positive / discordant_denominator
  ),
  concordance_p_value = primary_yield_comparison$p_value,
  aas_yield_p_value = primary_yield_comparison$p_value,
  stringsAsFactors = FALSE
)

table2_source <- rbind(table2_panel_a, table2_panel_b, table2_panel_c)
table2_source$generating_object <- "table2_source"

# ---- Table 3 source: alternative diagnoses and provider yield -----------------
# I summarize alternative diagnoses among known negative encounters and compare provider yield.

known_negative <- analysis_data$aas_outcome == "Negative"
negative_denominator <- sum(known_negative)

alternative_categories <- c(
  "Any clinically meaningful alternative diagnosis" = "alternative_diagnosis_definite",
  "GI inflammatory diagnoses" = "alt_gi_inflammatory",
  "Pulmonary infection" = "alt_pulmonary_infection",
  "GU acute diagnoses" = "alt_gu_acute",
  "Other vascular diagnoses" = "alt_other_vascular",
  "Hemorrhage/trauma" = "alt_hemorrhage_trauma",
  "GI obstruction or perforation" = "alt_gi_obstruction_perforation",
  "Pulmonary embolism" = "alt_pe",
  "Pulmonary edema" = "alt_pulmonary_edema",
  "Pneumothorax/hemopericardium" = "alt_ptx_hemopericardium"
)

table3_panel_a <- do.call(
  rbind,
  lapply(
    names(alternative_categories),
    function(category) {
      variable <- alternative_categories[[category]]
      events <- sum(analysis_data[[variable]][known_negative])
      summary <- binomial_summary(events, negative_denominator)

      data.frame(
        panel = "3A",
        category = category,
        numerator = summary["numerator"],
        denominator = summary["denominator"],
        percent = summary["percent"],
        ci_low_percent = summary["ci_low_percent"],
        ci_high_percent = summary["ci_high_percent"],
        p_value = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  )
)

provider_yield_row <- function(label, include, group_variable) {
  known <- include & !is.na(analysis_data$aas_positive)
  group <- group_variable[known]
  outcome <- analysis_data$aas_positive[known]
  groups <- unique(group)

  if (length(groups) != 2L) {
    stop("Provider yield comparison does not contain two groups.", call. = FALSE)
  }

  p_value <- chisq_p(table(group, factor(outcome, levels = c(0L, 1L))))

  do.call(
    rbind,
    lapply(
      sort(groups),
      function(group_value) {
        denominator <- sum(group == group_value)
        events <- sum(outcome[group == group_value])

        data.frame(
          comparison = label,
          provider_group = group_value,
          aas_positive_n = events,
          aas_known_denominator = denominator,
          aas_positive_percent = 100 * events / denominator,
          p_value = p_value,
          stringsAsFactors = FALSE
        )
      }
    )
  )
}

provider_yield_comparison <- rbind(
  provider_yield_row(
    "Physician vs non-physician",
    analysis_data$provider_group %in% c("Physician", "Non-physician"),
    analysis_data$provider_group
  ),
  provider_yield_row(
    "Attending physician vs APP",
    analysis_data$provider_role %in% c("Attending physician", "APP"),
    analysis_data$provider_role
  )
)

table3_panel_b <- data.frame(
  panel = "3B",
  category = paste(
    provider_yield_comparison$comparison,
    provider_yield_comparison$provider_group,
    sep = ": "
  ),
  numerator = provider_yield_comparison$aas_positive_n,
  denominator = provider_yield_comparison$aas_known_denominator,
  percent = provider_yield_comparison$aas_positive_percent,
  ci_low_percent = NA_real_,
  ci_high_percent = NA_real_,
  p_value = provider_yield_comparison$p_value,
  stringsAsFactors = FALSE
)

table3_source <- rbind(table3_panel_a, table3_panel_b)
table3_source$generating_object <- "table3_source"

# ---- Validate and save --------------------------------------------------------
# I check selected table totals before saving the descriptive results.

if (sum(table1_source$overall_n[table1_source$characteristic == "Provider role"]) != 1193L) {
  stop("Table 1 provider-role counts do not reconcile to 1,193.", call. = FALSE)
}

if (
  sum(table2_panel_a$n_classifiable[table2_panel_a$stratum == "Total"]) !=
    sum(analysis_data$algorithm_class != "Unclassifiable")
) {
  stop("Table 2 classifiable total does not reconcile.", call. = FALSE)
}

descriptive_results_file <- file.path(aas_derived_dir, "aas_descriptive_results.rds")

saveRDS(
  list(
    table1_source = table1_source,
    table2_source = table2_source,
    table3_source = table3_source,
    primary_yield_comparison = primary_yield_comparison,
    provider_yield_comparison = provider_yield_comparison
  ),
  descriptive_results_file
)

message("\nPrimary yield comparison:")
print(primary_yield_comparison, row.names = FALSE)

message("\nProvider yield comparisons:")
print(provider_yield_comparison, row.names = FALSE)

message("\nDescriptive result objects written outside Git:")
message(descriptive_results_file)
