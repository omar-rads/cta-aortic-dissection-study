# Why this script is here
# I apply the published-accuracy inputs to the cost-consequence base case.
# Modeled false negatives are a separate consequence from costs and imaging use.

# =============================================================================
# 14_economic_option2_published_meta.R
# AAS CTA R Reanalysis
#
# Independent economic-model implementation: Option 2
# Published 2024 D-dimer meta-analysis parameterization
#
# This script is intentionally isolated from the separate Option 1 analysis.
# It does not source, read, modify, or write any Option 1 artifact. It also does
# not reuse the historical discordant-cohort/net-savings implementation in
# R/07_sensitivity_analyses.R because the population, clinical consequences,
# and economic estimand differ.
# =============================================================================

source("R/00_setup.R")
source("R/economic_cost_consequence_engine.R")


# ---- 1. Fixed analytic provenance ------------------------------------------

option2_script <- "R/14_economic_option2_published_meta.R"
option2_base_branch <- "codex/r-rebuild-first-pass"
option2_base_commit <- "b172783206786ee0a23e84c76493eef7d7e1db24"
option2_packages_used <- paste0(
  "base ", as.character(utils::packageVersion("base")),
  "; utils ", as.character(utils::packageVersion("utils"))
)

current_branch <- trimws(
  system2("git", c("branch", "--show-current"), stdout = TRUE)
)

if (!identical(current_branch, option2_base_branch)) {
  stop(
    "Option 2 must run on branch ", option2_base_branch,
    "; current branch is ", current_branch, ".",
    call. = FALSE
  )
}


# ---- 2. Primary cost inputs with optional paired overrides -----------------

default_cost_inputs <- list(
  cta_unit_cost = 634.96,
  d_dimer_unit_cost = 10.18,
  source = "Current Option 2 specification supplied by the investigator"
)

# Accept cost overrides only as a paired set so both strategies use the same inputs.
parse_cost_arguments <- function(args) {

  allowed <- "^--(cta-unit-cost|d-dimer-unit-cost)="
  unknown <- args[!grepl(allowed, args)]
  if (length(unknown) > 0L) {
    stop(
      "Unknown command-line argument(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  read_one <- function(name) {
    prefix <- paste0("^--", name, "=")
    matches <- grep(prefix, args, value = TRUE)
    if (length(matches) == 0L) return(NA_real_)
    if (length(matches) > 1L) {
      stop("Duplicate --", name, " argument.", call. = FALSE)
    }
    value <- suppressWarnings(as.numeric(sub(prefix, "", matches)))
    if (is.na(value) || !is.finite(value) || value < 0) {
      stop("--", name, " must be a nonnegative finite number.", call. = FALSE)
    }
    value
  }

  cta_unit_cost_override <- read_one("cta-unit-cost")
  d_dimer_unit_cost_override <- read_one("d-dimer-unit-cost")

  if (xor(is.na(cta_unit_cost_override), is.na(d_dimer_unit_cost_override))) {
    stop(
      "Override both --cta-unit-cost and --d-dimer-unit-cost, or neither.",
      call. = FALSE
    )
  }

  use_defaults <- is.na(cta_unit_cost_override)

  list(
    cta_unit_cost = if (use_defaults) {
      default_cost_inputs$cta_unit_cost
    } else {
      cta_unit_cost_override
    },
    d_dimer_unit_cost = if (use_defaults) {
      default_cost_inputs$d_dimer_unit_cost
    } else {
      d_dimer_unit_cost_override
    },
    source = if (use_defaults) {
      default_cost_inputs$source
    } else {
      "Explicit paired command-line override"
    }
  )
}

cost_inputs <- parse_cost_arguments(commandArgs(trailingOnly = TRUE))


# ---- 3. Load and validate the existing encounter-level dataset -------------

analytic_data_file <- file.path(
  aas_derived_dir,
  "aas_analytic_encounters.rds"
)

if (!file.exists(analytic_data_file)) {
  stop(
    "Validated analytic dataset not found: ", analytic_data_file,
    ". Run R/04_variable_derivation.R first.",
    call. = FALSE
  )
}

analysis_data <- readRDS(analytic_data_file)

required_variables <- c(
  "encounter",
  "add_rs",
  "aas_outcome",
  "aas_positive"
)

missing_variables <- setdiff(required_variables, names(analysis_data))
if (length(missing_variables) > 0L) {
  stop(
    "Validated analytic dataset is missing: ",
    paste(missing_variables, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(analysis_data) != 1193L || anyDuplicated(analysis_data$encounter)) {
  stop("Primary analytic dataset failed the 1,193-encounter preflight.", call. = FALSE)
}

# Define the modeled population using low ADD-RS and known AAS outcomes.
low_add_rs <- !is.na(analysis_data$add_rs) & analysis_data$add_rs %in% c(0L, 1L)
known_aas <- low_add_rs & !is.na(analysis_data$aas_positive)

modeled_data <- analysis_data[known_aas, , drop = FALSE]

cohort_inputs <- data.frame(
  input = c(
    "Primary-cohort encounters",
    "ADD-RS 0-1 encounters",
    "ADD-RS 0-1 encounters with known AAS outcome",
    "AAS-positive encounters in modeled population",
    "AAS-negative encounters in modeled population",
    "ADD-RS 0-1 encounters with unknown AAS outcome",
    "Observed AAS prevalence in modeled population"
  ),
  value = c(
    nrow(analysis_data),
    sum(low_add_rs),
    nrow(modeled_data),
    sum(modeled_data$aas_positive == 1L),
    sum(modeled_data$aas_positive == 0L),
    sum(low_add_rs & is.na(analysis_data$aas_positive)),
    mean(modeled_data$aas_positive)
  ),
  unit = c(
    rep("encounters", 6L),
    "proportion"
  ),
  source = c(
    rep("R/04_variable_derivation.R; analytic_data", 7L)
  ),
  stringsAsFactors = FALSE
)

n_add_rs_0_1 <- as.integer(sum(low_add_rs))
n_modeled <- as.integer(nrow(modeled_data))
n_aas_positive <- as.integer(sum(modeled_data$aas_positive == 1L))
n_aas_negative <- as.integer(sum(modeled_data$aas_positive == 0L))
n_aas_unknown <- as.integer(sum(low_add_rs & is.na(analysis_data$aas_positive)))
aas_prevalence <- n_aas_positive / n_modeled

if (!identical(
  c(n_add_rs_0_1, n_modeled, n_aas_positive, n_aas_negative, n_aas_unknown),
  c(923L, 921L, 43L, 878L, 2L)
)) {
  stop(
    "Option 2 cohort inputs no longer reconcile to 923 low-ADD-RS, ",
    "921 known-outcome, 43 positive, 878 negative, and 2 unknown encounters.",
    call. = FALSE
  )
}


# ---- 4. External parameter object ------------------------------------------

# External estimates are defined once here and then retrieved by identifier.
# No new pooling or meta-analysis is performed by this script.
external_parameters <- data.frame(
  parameter_id = c(
    "published_sensitivity_base",
    "published_sensitivity_lower",
    "published_sensitivity_upper",
    "published_specificity_base",
    "published_specificity_lower",
    "published_specificity_upper",
    "ren_crude_sensitivity",
    "ren_crude_specificity"
  ),
  parameter = c(
    "D-dimer sensitivity",
    "D-dimer sensitivity",
    "D-dimer sensitivity",
    "D-dimer specificity",
    "D-dimer specificity",
    "D-dimer specificity",
    "D-dimer sensitivity",
    "D-dimer specificity"
  ),
  value = c(
    0.965,
    0.948,
    0.980,
    0.562,
    0.483,
    0.639,
    0.974,
    0.561
  ),
  unit = "proportion",
  source = c(
    rep(
      "Published 2024 D-dimer meta-analysis; estimate supplied in Option 2 specification",
      6L
    ),
    rep(
      "Ren et al. 2024 supplementary data; user-supplied crude ADD-RS 0-1 conditional reconstruction",
      2L
    )
  ),
  notes = c(
    "Base-case external sensitivity",
    "Lower one-way sensitivity input",
    "Upper one-way sensitivity input",
    "Base-case external specificity",
    "Lower one-way specificity input",
    "Upper one-way specificity input",
    "Secondary consistency scenario; not independent validation",
    "Secondary consistency scenario; not independent validation"
  ),
  stringsAsFactors = FALSE
)

parameter_value <- function(parameter_id) {
  value <- external_parameters$value[
    external_parameters$parameter_id == parameter_id
  ]
  if (length(value) != 1L) {
    stop("Parameter identifier did not resolve uniquely: ", parameter_id, call. = FALSE)
  }
  value
}

# Set the published accuracy scenarios that will be passed to the shared engine.
scenario_parameters <- data.frame(
  scenario_order = seq_len(6L),
  scenario_id = c(
    "base_case_published_meta",
    "sensitivity_lower_one_way",
    "sensitivity_upper_one_way",
    "specificity_lower_one_way",
    "specificity_upper_one_way",
    "ren_crude_consistency"
  ),
  scenario_label = c(
    "Base case: published 2024 meta-analysis",
    "One-way: sensitivity lower bound",
    "One-way: sensitivity upper bound",
    "One-way: specificity lower bound",
    "One-way: specificity upper bound",
    "Secondary consistency: Ren crude ADD-RS 0-1 estimates"
  ),
  scenario_type = c(
    "base_case",
    "one_way_sensitivity",
    "one_way_sensitivity",
    "one_way_specificity",
    "one_way_specificity",
    "secondary_consistency"
  ),
  sensitivity = c(
    parameter_value("published_sensitivity_base"),
    parameter_value("published_sensitivity_lower"),
    parameter_value("published_sensitivity_upper"),
    parameter_value("published_sensitivity_base"),
    parameter_value("published_sensitivity_base"),
    parameter_value("ren_crude_sensitivity")
  ),
  specificity = c(
    parameter_value("published_specificity_base"),
    parameter_value("published_specificity_base"),
    parameter_value("published_specificity_base"),
    parameter_value("published_specificity_lower"),
    parameter_value("published_specificity_upper"),
    parameter_value("ren_crude_specificity")
  ),
  source = c(
    rep(
      "Published 2024 D-dimer meta-analysis; estimates supplied in Option 2 specification",
      5L
    ),
    "Ren et al. 2024 supplementary data; user-supplied crude conditional reconstruction"
  ),
  stringsAsFactors = FALSE
)

long_scenario_parameters <- do.call(
  rbind,
  lapply(seq_len(nrow(scenario_parameters)), function(i) {
    scenario <- scenario_parameters[i, , drop = FALSE]
    data.frame(
      record_type = "test_performance_parameter",
      scenario_order = scenario$scenario_order,
      scenario_id = scenario$scenario_id,
      scenario_label = scenario$scenario_label,
      scenario_type = scenario$scenario_type,
      parameter = c("sensitivity", "specificity"),
      value = c(scenario$sensitivity, scenario$specificity),
      unit = "proportion",
      source = scenario$source,
      notes = if (scenario$scenario_type == "secondary_consistency") {
        "Crude subgroup-consistency estimate; not independent validation; no new pooled meta-analysis"
      } else {
        "External model assumption"
      },
      stringsAsFactors = FALSE
    )
  })
)

cost_parameter_rows <- data.frame(
  record_type = "cost_parameter",
  scenario_order = NA_integer_,
  scenario_id = "all_scenarios",
  scenario_label = "All scenarios",
  scenario_type = "cost_input",
  parameter = c("cta_unit_cost", "d_dimer_unit_cost"),
  value = c(cost_inputs$cta_unit_cost, cost_inputs$d_dimer_unit_cost),
  unit = "currency units per test",
  source = cost_inputs$source,
  notes = c(
    "Standardized combined CTA unit cost; false-negative AAS is not monetized",
    "D-dimer unit cost applied to every modeled encounter"
  ),
  stringsAsFactors = FALSE
)

option2_parameter_table <- rbind(
  long_scenario_parameters,
  cost_parameter_rows
)

option2_parameter_table$analytic_base_branch <- option2_base_branch
option2_parameter_table$analytic_base_commit <- option2_base_commit
option2_parameter_table$generating_script <- option2_script


# ---- 5. Apply the shared parameterized cost-consequence engine -------------

# Calculate testing use, expenditures, and false negatives for each scenario.
scenario_results <- lapply(seq_len(nrow(scenario_parameters)), function(i) {
  scenario <- scenario_parameters[i, , drop = FALSE]
  modeled <- calculate_cost_consequence(
    aas_positive = n_aas_positive,
    aas_negative = n_aas_negative,
    sensitivity = scenario$sensitivity,
    specificity = scenario$specificity,
    cta_unit_cost = cost_inputs$cta_unit_cost,
    d_dimer_unit_cost = cost_inputs$d_dimer_unit_cost
  )
  cbind(scenario, modeled, stringsAsFactors = FALSE)
})

option2_results <- do.call(rbind, scenario_results)

option2_results$n_add_rs_0_1 <- n_add_rs_0_1
option2_results$n_primary_cohort <- nrow(analysis_data)
option2_results$n_modeled_known_aas <- n_modeled
option2_results$n_aas_unknown_excluded <- n_aas_unknown
option2_results$n_aas_positive <- n_aas_positive
option2_results$n_aas_negative <- n_aas_negative
option2_results$observed_aas_prevalence <- aas_prevalence
option2_results$cost_status <- paste0(
  "Calculated from ",
  cost_inputs$source
)
option2_results$interpretation_status <- ifelse(
  option2_results$scenario_type == "secondary_consistency",
  "Secondary consistency scenario; not independent validation",
  "Modeled expected values using external test-performance assumptions"
)
option2_results$analytic_data_source <-
  "R/04_variable_derivation.R output: aas_analytic_encounters.rds"
option2_results$analytic_base_branch <- option2_base_branch
option2_results$analytic_base_commit <- option2_base_commit
option2_results$r_version <- R.version.string
option2_results$packages_used <- option2_packages_used
option2_results$generating_script <- option2_script
option2_results$generating_object <- "option2_results"

preferred_order <- c(
  "scenario_order",
  "scenario_id",
  "scenario_label",
  "scenario_type",
  "sensitivity",
  "specificity",
  "n_primary_cohort",
  "n_add_rs_0_1",
  "n_modeled_known_aas",
  "n_aas_unknown_excluded",
  "n_aas_positive",
  "n_aas_negative",
  "observed_aas_prevalence",
  "expected_tp",
  "expected_fn",
  "expected_tn",
  "expected_fp",
  "expected_d_dimer_positive",
  "expected_d_dimer_negative",
  "cta_for_all_cta_count",
  "d_dimer_first_d_dimer_count",
  "expected_cta_performed",
  "expected_cta_deferred",
  "expected_false_negative_aas",
  "cta_deferred_percent",
  "cta_unit_cost",
  "d_dimer_unit_cost",
  "cta_for_all_expenditure",
  "d_dimer_first_expenditure",
  "testing_expenditure_reduction",
  "testing_expenditure_reduction_percent",
  "cost_status",
  "false_negative_monetary_cost_included",
  "interpretation_status",
  "source",
  "analytic_data_source",
  "analytic_base_branch",
  "analytic_base_commit",
  "r_version",
  "packages_used",
  "generating_script",
  "generating_object"
)

option2_results <- option2_results[preferred_order]

primary_option2_result <- option2_results[
  option2_results$scenario_id == "base_case_published_meta",
  ,
  drop = FALSE
]


# ---- 6. Internal validation -------------------------------------------------

# Check the scenario counts and arithmetic before writing results.
tolerance <- 1e-10

if (nrow(option2_results) != 6L || anyDuplicated(option2_results$scenario_id)) {
  stop("Option 2 did not generate six unique scenarios.", call. = FALSE)
}

if (any(abs(
  option2_results$expected_tp + option2_results$expected_fn - n_aas_positive
) > tolerance)) {
  stop("Expected TP and FN do not reconcile to AAS-positive N.", call. = FALSE)
}

if (any(abs(
  option2_results$expected_tn + option2_results$expected_fp - n_aas_negative
) > tolerance)) {
  stop("Expected TN and FP do not reconcile to AAS-negative N.", call. = FALSE)
}

if (any(abs(
  option2_results$expected_cta_performed +
    option2_results$expected_cta_deferred - n_modeled
) > tolerance)) {
  stop("Expected CTA performed/deferred counts do not reconcile.", call. = FALSE)
}

if (!all(
  option2_results$expected_cta_performed ==
    option2_results$expected_d_dimer_positive
) || !all(
  option2_results$expected_cta_deferred ==
    option2_results$expected_d_dimer_negative
)) {
  stop("D-dimer and CTA expected counts do not reconcile.", call. = FALSE)
}

if (
  any(option2_results$cta_for_all_cta_count != n_modeled) ||
  any(option2_results$d_dimer_first_d_dimer_count != n_modeled)
) {
  stop("Strategy testing counts do not reconcile to modeled N.", call. = FALSE)
}

if (any(abs(
  option2_results$cta_for_all_expenditure -
    n_modeled * option2_results$cta_unit_cost
) > tolerance)) {
  stop("CTA-for-all expenditure does not reconcile.", call. = FALSE)
}

if (any(abs(
  option2_results$d_dimer_first_expenditure -
    (
      n_modeled * option2_results$d_dimer_unit_cost +
      option2_results$expected_cta_performed *
        option2_results$cta_unit_cost
    )
) > tolerance)) {
  stop("D-dimer-first expenditure does not reconcile.", call. = FALSE)
}

if (any(abs(
  option2_results$testing_expenditure_reduction -
    (
      option2_results$cta_for_all_expenditure -
      option2_results$d_dimer_first_expenditure
    )
) > tolerance)) {
  stop("Absolute testing-expenditure reduction does not reconcile.", call. = FALSE)
}

if (any(abs(
  option2_results$testing_expenditure_reduction_percent -
    100 * option2_results$testing_expenditure_reduction /
      option2_results$cta_for_all_expenditure
) > tolerance)) {
  stop("Percentage testing-expenditure reduction does not reconcile.", call. = FALSE)
}

if (any(option2_results$false_negative_monetary_cost_included)) {
  stop("False-negative monetary cost must remain excluded.", call. = FALSE)
}

if (
  identical(cost_inputs$source, default_cost_inputs$source) &&
  (
    cost_inputs$cta_unit_cost != default_cost_inputs$cta_unit_cost ||
    cost_inputs$d_dimer_unit_cost != default_cost_inputs$d_dimer_unit_cost
  )
) {
  stop("Default Option 2 cost inputs changed unexpectedly.", call. = FALSE)
}

primary_expected_counts <- c(
  expected_cta_performed = 426.059,
  expected_cta_deferred = 494.941,
  expected_false_negative_aas = 1.505
)

if (nrow(primary_option2_result) != 1L || any(abs(
  unlist(
    primary_option2_result[names(primary_expected_counts)],
    use.names = TRUE
  ) - primary_expected_counts
) > tolerance)) {
  stop("Primary Option 2 expected counts failed their regression check.", call. = FALSE)
}


# ---- 7. Write machine-readable outputs -------------------------------------

# Save the parameter table, results, and audit information.
parameter_output_file <- file.path(
  "results",
  "economic_option2_published_meta_parameters.csv"
)

results_output_file <- file.path(
  "results",
  "economic_option2_published_meta_results.csv"
)

audit_output_file <- file.path(
  "docs",
  "economic_option2_published_meta_audit.md"
)

utils::write.csv(
  option2_parameter_table,
  parameter_output_file,
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  option2_results,
  results_output_file,
  row.names = FALSE,
  na = ""
)


# ---- 8. Write the audit note ------------------------------------------------

# Format the audit note from the calculated results.
display_number <- function(x, digits = 3L) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

display_currency <- function(x) {
  paste0("$", display_number(x, 2L))
}

primary_result_markdown <- c(
  "| Primary Option 2 result | Value |",
  "|---|---:|",
  paste0(
    "| CTA-for-all standardized testing expenditure | ",
    display_currency(primary_option2_result$cta_for_all_expenditure),
    " |"
  ),
  paste0(
    "| D-dimer-first standardized testing expenditure | ",
    display_currency(primary_option2_result$d_dimer_first_expenditure),
    " |"
  ),
  paste0(
    "| Absolute testing-expenditure reduction | ",
    display_currency(primary_option2_result$testing_expenditure_reduction),
    " |"
  ),
  paste0(
    "| Testing-expenditure reduction, % | ",
    display_number(
      primary_option2_result$testing_expenditure_reduction_percent,
      2L
    ),
    "% |"
  ),
  paste0(
    "| Expected CTAs performed with D-dimer-first | ",
    display_number(primary_option2_result$expected_cta_performed),
    " |"
  ),
  paste0(
    "| Expected CTAs deferred with D-dimer-first | ",
    display_number(primary_option2_result$expected_cta_deferred),
    " |"
  ),
  paste0(
    "| CTA deferral, % | ",
    display_number(primary_option2_result$cta_deferred_percent, 2L),
    "% |"
  ),
  paste0(
    "| Expected false-negative AAS | ",
    display_number(primary_option2_result$expected_false_negative_aas),
    " |"
  )
)

scenario_markdown <- c(
  "| Scenario | Sensitivity | Specificity | Expected CTA performed | Expected CTA deferred | Deferred, % | Expected false-negative AAS |",
  "|---|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(option2_results))) {
  row <- option2_results[i, , drop = FALSE]
  scenario_markdown <- c(
    scenario_markdown,
    paste0(
      "| ", row$scenario_label,
      " | ", display_number(row$sensitivity),
      " | ", display_number(row$specificity),
      " | ", display_number(row$expected_cta_performed),
      " | ", display_number(row$expected_cta_deferred),
      " | ", display_number(row$cta_deferred_percent, 1L),
      " | ", display_number(row$expected_false_negative_aas),
      " |"
    )
  )
}

audit_lines <- c(
  "# Parameterized Cost-Consequence Engine: Primary Option 2 Run",
  "",
  "## Scope",
  "",
  paste0(
    "This is an independent Option 2 implementation based on branch `",
    option2_base_branch, "` at analytic-base commit `", option2_base_commit,
    "`. It does not read, modify, or depend on Option 1 files or outputs."
  ),
  "",
  paste0(
    "The script uses ", R.version.string,
    " with ", option2_packages_used,
    ". It reads the validated encounter-level output of `R/04_variable_derivation.R`; it does not reconstruct the cohort or ADD-RS."
  ),
  "",
  "## Modeled population",
  "",
  paste0("- Primary cohort: ", nrow(analysis_data), " encounters."),
  paste0("- ADD-RS 0-1: ", n_add_rs_0_1, " encounters."),
  paste0("- Known AAS outcome and included in the model: ", n_modeled, " encounters."),
  paste0("- AAS positive: ", n_aas_positive, "."),
  paste0("- AAS negative: ", n_aas_negative, "."),
  paste0("- Unknown AAS outcome: ", n_aas_unknown, "; retained as missing and excluded from model calculations."),
  paste0("- Observed modeled-population AAS prevalence: ", display_number(100 * aas_prevalence, 2L), "%."),
  "",
  "The modeled population is the full primary-cohort ADD-RS 0-1 subgroup with a known AAS outcome. It is not the retrospectively defined algorithm-discordant group.",
  "",
  "## External parameters",
  "",
  "The base case uses sensitivity 0.965 and specificity 0.562 from the published 2024 D-dimer meta-analysis estimates supplied in the Option 2 specification. One-way analyses use sensitivity 0.948 and 0.980 while holding specificity at 0.562, and specificity 0.483 and 0.639 while holding sensitivity at 0.965.",
  "",
  "The Ren et al. 2024 supplementary-data reconstruction uses sensitivity 0.974 and specificity 0.561. It is a secondary crude subgroup-consistency scenario, not independent validation, and this script performs no new pooled meta-analysis.",
  "",
  "The inspected repository did not contain a complete bibliographic identifier for the user-specified 2024 source. The parameter CSV therefore preserves the source attribution exactly as supplied rather than inventing citation details.",
  "",
  "## Parameterized engine",
  "",
  "All Option 2 parameter sets are evaluated by one function, `calculate_cost_consequence()`. For each parameter set:",
  "",
  "- `expected_tp = AAS-positive × sensitivity`",
  "- `expected_fn = AAS-positive × (1 − sensitivity)`",
  "- `expected_tn = AAS-negative × specificity`",
  "- `expected_fp = AAS-negative × (1 − specificity)`",
  "- `expected D-dimer positive = expected_tp + expected_fp`",
  "- `expected D-dimer negative = expected_tn + expected_fn`",
  "- `CTA-for-all CTAs = modeled N`",
  "- `D-dimer-first D-dimer tests = modeled N`",
  "- `expected CTA performed = expected D-dimer positive`",
  "- `expected CTA deferred = expected D-dimer negative`",
  "- `CTA deferred percentage = expected CTA deferred ÷ modeled N × 100`",
  "- `CTA-for-all expenditure = modeled N × CTA unit cost`",
  "- `D-dimer-first expenditure = modeled N × D-dimer unit cost + expected CTA performed × CTA unit cost`",
  "- `absolute expenditure reduction = CTA-for-all expenditure − D-dimer-first expenditure`",
  "- `percentage expenditure reduction = absolute expenditure reduction ÷ CTA-for-all expenditure × 100`",
  "",
  "Intermediate expected counts and expenditures are not rounded. Tables round only for display.",
  "",
  "## Primary Option 2 result",
  "",
  primary_result_markdown,
  "",
  paste0(
    "The primary run uses CTA unit cost ",
    display_currency(cost_inputs$cta_unit_cost),
    " and D-dimer unit cost ",
    display_currency(cost_inputs$d_dimer_unit_cost),
    ". All ", n_modeled,
    " encounters receive D-dimer under the D-dimer-first strategy; CTA is performed only after an expected positive D-dimer result."
  ),
  "",
  "Expected false-negative AAS is reported as a separate clinical consequence and has no monetary value in the engine.",
  "",
  "## Supplementary Option 2 parameter scenarios",
  "",
  scenario_markdown,
  "",
  "## Economic inputs",
  "",
  paste0(
    "The default current-run inputs are CTA unit cost ",
    display_currency(cost_inputs$cta_unit_cost),
    " and D-dimer unit cost ",
    display_currency(cost_inputs$d_dimer_unit_cost),
    ". They can be replaced only as a paired command-line override so a partial cost update cannot produce mixed-source results."
  ),
  "",
  "The reported monetary comparison is limited to standardized testing expenditure. It excludes all downstream clinical costs and does not monetize false-negative AAS.",
  "",
  "## Limitations",
  "",
  "- All modeled cell counts are expected values derived from external test-performance assumptions, not observed outcomes under an implemented D-dimer strategy.",
  "- Transporting published sensitivity and specificity to this retrospective cohort assumes that test thresholds, assays, spectrum, and application of ADD-RS are sufficiently comparable.",
  "- Unknown AAS outcomes are excluded rather than treated as negative.",
  "- No utilization effects beyond CTA performed/deferred are modeled.",
  "- No downstream management, adverse-event, quality-of-life, or false-negative monetary consequences are valued.",
  "- Option 1 remains a separate parameterization for later comparison.",
  "",
  "## Reproduction",
  "",
  paste0(
    "After generating `aas_analytic_encounters.rds` with `R/04_variable_derivation.R`, run `RESEARCH_DATA_ROOT=<research-data-root> Rscript --vanilla ",
    option2_script, "`."
  ),
  "",
  "The primary inputs are built into the default run. To evaluate a different paired cost set, supply both overrides:",
  "",
  paste0(
    "`RESEARCH_DATA_ROOT=<research-data-root> Rscript --vanilla ", option2_script,
    " --cta-unit-cost=<value> --d-dimer-unit-cost=<value>`"
  )
)

writeLines(audit_lines, audit_output_file, useBytes = TRUE)


# ---- 9. Aggregate console summary ------------------------------------------

message("\nOption 2 modeled cohort:")
print(cohort_inputs, row.names = FALSE)

message("\nPrimary Option 2 cost-consequence result (unrounded values):")
print(
  primary_option2_result[c(
    "sensitivity",
    "specificity",
    "cta_for_all_cta_count",
    "d_dimer_first_d_dimer_count",
    "expected_cta_performed",
    "expected_cta_deferred",
    "cta_deferred_percent",
    "expected_false_negative_aas",
    "cta_for_all_expenditure",
    "d_dimer_first_expenditure",
    "testing_expenditure_reduction",
    "testing_expenditure_reduction_percent",
    "false_negative_monetary_cost_included"
  )],
  row.names = FALSE
)

message("\nOption 2 outputs written:")
message(parameter_output_file)
message(results_output_file)
message(audit_output_file)
message("Option 2 completed with all reconciliation checks passed.")
