# Why this script is here
# I examine how the economic results behave under a new-setting sensitivity analysis.
# The results depend on the assumptions used to transport test accuracy.

# =============================================================================
# 16_economic_option1_transportability.R
# AAS CTA R Reanalysis
#
# Option 1 weak-prior new-setting transportability sensitivity analysis
# =============================================================================

source("R/00_setup.R")
source("R/economic_cost_consequence_engine.R")


# ---- 1. Fixed analytic provenance ------------------------------------------

transport_script <- "R/16_economic_option1_transportability.R"
engine_script <- "R/economic_cost_consequence_engine.R"
option1_model_script <- file.path(
  "external_dimer_evidence",
  "option1_conditional_bivariate",
  "R",
  "run_option1_conditional_bivariate.R"
)
option1_stan_file <- file.path(
  "external_dimer_evidence",
  "option1_conditional_bivariate",
  "stan",
  "bivariate_dta.stan"
)
posterior_file <- file.path(
  "external_dimer_evidence",
  "option1_conditional_bivariate",
  "output",
  "option1_posterior_draws_weakly_informative.csv.gz"
)
model_summary_file <- file.path(
  "external_dimer_evidence",
  "option1_conditional_bivariate",
  "output",
  "option1_model_summary.csv"
)
pooled_comparison_file <- file.path(
  "results",
  "economic_option1_option2_comparison.csv"
)

analytic_branch <- "codex/r-rebuild-first-pass"
analytic_base_commit <- "5fab156c666815315f5c51591c8388ac42be0ac5"
transport_run_id <- "option1_transportability_2026-09-19_v1"
option1_run_id <- "option1_2026-09-19_v1"

expected_hashes <- c(
  shared_engine =
    "84316d925eec14ed953bf41e2d78cbde7deac20f3f437fb4db95e2ba590b01ee",
  option1_stan_model =
    "ccdaf5cf2f85af8455d9a86e9217fe8899b35875e2bf81b4768ceb0ecf85adc1",
  option1_posterior_draws =
    "065419d8d85246cc40e86c9a12b2ae317f7c29ba74e0862600af3601553e042f",
  option1_model_summary =
    "3ee7c2e02bbf1fbec632e62b3de06ac6bbb749bbabeb151076a8ce189ec42f7e",
  pooled_economic_comparison =
    "8483081e138ebc2deccfde98a47e7de841f4ba720b800605d0b33e3be1123cd4"
)

current_branch <- trimws(
  system2("git", c("branch", "--show-current"), stdout = TRUE)
)
if (!identical(current_branch, analytic_branch)) {
  stop(
    "Transportability analysis must run on branch ", analytic_branch,
    "; current branch is ", current_branch, ".",
    call. = FALSE
  )
}

# Verify the frozen inputs before using them in this analysis.
sha256_file <- function(path) {
  if (!file.exists(path)) {
    stop("Cannot hash missing file: ", path, call. = FALSE)
  }

  shasum_path <- Sys.which("shasum")
  sha256sum_path <- Sys.which("sha256sum")

  if (nzchar(shasum_path)) {
    output <- system2(
      shasum_path,
      c("-a", "256", path),
      stdout = TRUE,
      stderr = TRUE
    )
  } else if (nzchar(sha256sum_path)) {
    output <- system2(
      sha256sum_path,
      path,
      stdout = TRUE,
      stderr = TRUE
    )
  } else {
    stop("No SHA-256 command is available.", call. = FALSE)
  }

  if (length(output) != 1L || !grepl("^[0-9a-f]{64}", output)) {
    stop("SHA-256 calculation failed for: ", path, call. = FALSE)
  }

  substr(output, 1L, 64L)
}

observed_hashes <- c(
  shared_engine = sha256_file(engine_script),
  option1_stan_model = sha256_file(option1_stan_file),
  option1_posterior_draws = sha256_file(posterior_file),
  option1_model_summary = sha256_file(model_summary_file),
  pooled_economic_comparison = sha256_file(pooled_comparison_file)
)

if (!identical(observed_hashes, expected_hashes)) {
  failed_hashes <- names(expected_hashes)[observed_hashes != expected_hashes]
  stop(
    "Frozen input hash mismatch for: ",
    paste(failed_hashes, collapse = ", "),
    call. = FALSE
  )
}


# ---- 2. Fixed modeled population and costs ---------------------------------

analytic_data_file <- file.path(
  aas_derived_dir,
  "aas_analytic_encounters.rds"
)
if (!file.exists(analytic_data_file)) {
  stop(
    "Validated analytic dataset not found: ", analytic_data_file,
    call. = FALSE
  )
}

# Confirm the modeled cohort and its known AAS outcomes.
analysis_data <- readRDS(analytic_data_file)
required_variables <- c("encounter", "add_rs", "aas_positive")
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

low_add_rs <- !is.na(analysis_data$add_rs) & analysis_data$add_rs %in% c(0L, 1L)
known_aas <- low_add_rs & !is.na(analysis_data$aas_positive)

n_add_rs_0_1 <- as.integer(sum(low_add_rs))
n_modeled <- as.integer(sum(known_aas))
n_aas_positive <- as.integer(sum(analysis_data$aas_positive[known_aas] == 1L))
n_aas_negative <- as.integer(sum(analysis_data$aas_positive[known_aas] == 0L))
n_aas_unknown <- as.integer(sum(low_add_rs & is.na(analysis_data$aas_positive)))

if (!identical(
  c(n_add_rs_0_1, n_modeled, n_aas_positive, n_aas_negative, n_aas_unknown),
  c(923L, 921L, 43L, 878L, 2L)
)) {
  stop(
    "Modeled population no longer reconciles to 923 low-ADD-RS, 921 known ",
    "outcomes, 43 AAS-positive, 878 AAS-negative, and 2 unknown outcomes.",
    call. = FALSE
  )
}

cta_unit_cost <- 634.96
d_dimer_unit_cost <- 10.18


# ---- 3. Load frozen weak-prior posterior-predictive draws ------------------

# Load the saved paired predictive draws and check their structure.
posterior_connection <- gzfile(posterior_file, open = "rt")
posterior_draws <- utils::read.csv(
  posterior_connection,
  check.names = FALSE
)
close(posterior_connection)

required_draw_columns <- c(
  "chain",
  "iteration",
  "draw_id",
  "pooled_sensitivity",
  "pooled_specificity",
  "tau_sensitivity",
  "tau_specificity",
  "rho",
  "new_study_sensitivity",
  "new_study_specificity",
  "evidence_strategy",
  "prior_strategy",
  "run_id",
  "model_sha256",
  "source_data_sha256",
  "seed",
  "cmdstan_version",
  "cmdstanr_version"
)

if (!identical(names(posterior_draws), required_draw_columns)) {
  stop("Weak-prior posterior-draw schema changed.", call. = FALSE)
}

if (
  nrow(posterior_draws) != 16000L ||
  anyDuplicated(posterior_draws$draw_id) ||
  !identical(sort(unique(posterior_draws$chain)), 1:4) ||
  any(tabulate(posterior_draws$chain, nbins = 4L) != 4000L)
) {
  stop("Weak-prior posterior-draw count or chain structure changed.", call. = FALSE)
}

probability_columns <- c(
  "pooled_sensitivity",
  "pooled_specificity",
  "new_study_sensitivity",
  "new_study_specificity"
)
if (
  any(!is.finite(as.matrix(posterior_draws[probability_columns]))) ||
  any(as.matrix(posterior_draws[probability_columns]) < 0) ||
  any(as.matrix(posterior_draws[probability_columns]) > 1)
) {
  stop("Weak-prior probability draws are invalid.", call. = FALSE)
}

if (
  !identical(unique(posterior_draws$prior_strategy), "weakly_informative") ||
  !identical(unique(posterior_draws$run_id), option1_run_id) ||
  !identical(
    unique(posterior_draws$model_sha256),
    expected_hashes[["option1_stan_model"]]
  )
) {
  stop("Weak-prior posterior provenance changed.", call. = FALSE)
}

# The Stan model generated each pair jointly within its generated-quantities
# block using the draw-specific mu, tau, and L_Omega. Reusing those saved pairs
# avoids a second Monte Carlo realization and exactly preserves the established
# Option 1 prediction distribution.
accuracy_draws <- data.frame(
  chain = posterior_draws$chain,
  iteration = posterior_draws$iteration,
  draw_id = posterior_draws$draw_id,
  pooled_sensitivity = posterior_draws$pooled_sensitivity,
  pooled_specificity = posterior_draws$pooled_specificity,
  tau_sensitivity = posterior_draws$tau_sensitivity,
  tau_specificity = posterior_draws$tau_specificity,
  rho = posterior_draws$rho,
  sensitivity_new_setting = posterior_draws$new_study_sensitivity,
  specificity_new_setting = posterior_draws$new_study_specificity,
  evidence_strategy = posterior_draws$evidence_strategy,
  prior_strategy = posterior_draws$prior_strategy,
  option1_run_id = posterior_draws$run_id,
  transport_run_id = transport_run_id,
  generation_method = paste(
    "Existing paired Stan generated quantities:",
    "eta_new ~ multivariate normal(mu, diag(tau) Omega diag(tau));",
    "inverse-logit transformation"
  ),
  model_sha256 = posterior_draws$model_sha256,
  source_data_sha256 = posterior_draws$source_data_sha256,
  posterior_file_sha256 = observed_hashes[["option1_posterior_draws"]],
  seed = posterior_draws$seed,
  cmdstan_version = posterior_draws$cmdstan_version,
  cmdstanr_version = posterior_draws$cmdstanr_version,
  stringsAsFactors = FALSE
)


# ---- 4. Pass every paired draw through the unchanged shared engine ----------

# Run each paired draw through the shared cost-consequence calculation.
modeled <- calculate_cost_consequence(
  aas_positive = n_aas_positive,
  aas_negative = n_aas_negative,
  sensitivity = accuracy_draws$sensitivity_new_setting,
  specificity = accuracy_draws$specificity_new_setting,
  cta_unit_cost = cta_unit_cost,
  d_dimer_unit_cost = d_dimer_unit_cost
)

economic_draws <- data.frame(
  chain = accuracy_draws$chain,
  iteration = accuracy_draws$iteration,
  draw_id = accuracy_draws$draw_id,
  sensitivity_new_setting = accuracy_draws$sensitivity_new_setting,
  specificity_new_setting = accuracy_draws$specificity_new_setting,
  expected_true_positive_aas = modeled$expected_tp,
  expected_false_negative_aas = modeled$expected_fn,
  expected_true_negative_non_aas = modeled$expected_tn,
  expected_false_positive_non_aas = modeled$expected_fp,
  expected_cta_performed = modeled$expected_cta_performed,
  expected_cta_deferred = modeled$expected_cta_deferred,
  cta_deferred_percent = modeled$cta_deferred_percent,
  cta_for_all_expenditure = modeled$cta_for_all_expenditure,
  d_dimer_first_expenditure = modeled$d_dimer_first_expenditure,
  testing_expenditure_reduction = modeled$testing_expenditure_reduction,
  testing_expenditure_reduction_percent =
    modeled$testing_expenditure_reduction_percent,
  false_negative_monetary_cost_included =
    modeled$false_negative_monetary_cost_included,
  n_modeled_known_aas = n_modeled,
  n_aas_positive = n_aas_positive,
  n_aas_negative = n_aas_negative,
  cta_unit_cost = cta_unit_cost,
  d_dimer_unit_cost = d_dimer_unit_cost,
  prior_strategy = "weakly_informative",
  interval_target = "latent accuracy and expected outcomes in a new setting",
  transport_run_id = transport_run_id,
  posterior_file_sha256 = observed_hashes[["option1_posterior_draws"]],
  shared_engine_sha256 = observed_hashes[["shared_engine"]],
  stringsAsFactors = FALSE
)


# ---- 5. Posterior-predictive summaries -------------------------------------

# Summarize the distribution of each modeled consequence.
summary_definitions <- data.frame(
  metric_order = 1:10,
  metric = c(
    "sensitivity_new_setting",
    "specificity_new_setting",
    "expected_false_negative_aas",
    "expected_true_negative_non_aas",
    "expected_cta_performed",
    "expected_cta_deferred",
    "cta_deferred_percent",
    "d_dimer_first_expenditure",
    "testing_expenditure_reduction",
    "testing_expenditure_reduction_percent"
  ),
  label = c(
    "D-dimer sensitivity in a new setting",
    "D-dimer specificity in a new setting",
    "Expected false-negative AAS",
    "Expected true-negative non-AAS encounters",
    "Expected CTAs performed",
    "Expected CTAs deferred",
    "CTA deferral percentage",
    "D-dimer-first standardized testing expenditure",
    "Absolute standardized expenditure reduction versus CTA-for-all",
    "Percentage standardized expenditure reduction"
  ),
  unit = c(
    "proportion",
    "proportion",
    "expected encounters",
    "expected encounters",
    "expected CTAs",
    "expected CTAs",
    "percentage points",
    "currency units",
    "currency units",
    "percentage points"
  ),
  stringsAsFactors = FALSE
)

summary_source <- data.frame(
  sensitivity_new_setting = accuracy_draws$sensitivity_new_setting,
  specificity_new_setting = accuracy_draws$specificity_new_setting,
  expected_false_negative_aas = economic_draws$expected_false_negative_aas,
  expected_true_negative_non_aas =
    economic_draws$expected_true_negative_non_aas,
  expected_cta_performed = economic_draws$expected_cta_performed,
  expected_cta_deferred = economic_draws$expected_cta_deferred,
  cta_deferred_percent = economic_draws$cta_deferred_percent,
  d_dimer_first_expenditure = economic_draws$d_dimer_first_expenditure,
  testing_expenditure_reduction =
    economic_draws$testing_expenditure_reduction,
  testing_expenditure_reduction_percent =
    economic_draws$testing_expenditure_reduction_percent,
  stringsAsFactors = FALSE
)

summary_triplet <- function(x) {
  c(
    median = stats::median(x),
    lower_95 = unname(stats::quantile(x, 0.025)),
    upper_95 = unname(stats::quantile(x, 0.975))
  )
}

transport_summary <- do.call(
  rbind,
  lapply(seq_len(nrow(summary_definitions)), function(i) {
    definition <- summary_definitions[i, , drop = FALSE]
    values <- summary_source[[definition$metric]]
    result <- summary_triplet(values)
    data.frame(
      metric_order = definition$metric_order,
      metric = definition$metric,
      label = definition$label,
      posterior_predictive_median = result[["median"]],
      lower_95 = result[["lower_95"]],
      upper_95 = result[["upper_95"]],
      interval_type = "95% posterior predictive interval for a new setting",
      unit = definition$unit,
      posterior_predictive_draws = length(values),
      prior_strategy = "weakly_informative",
      source_object = "accuracy_draws / economic_draws",
      generating_script = transport_script,
      transport_run_id = transport_run_id,
      stringsAsFactors = FALSE
    )
  })
)


# ---- 6. Comparison with Option 2 and pooled-mean Option 1 ------------------

# Place the predictive results alongside the existing comparison results.
pooled_comparison <- utils::read.csv(
  pooled_comparison_file,
  check.names = FALSE
)

comparison_metrics <- c(
  "sensitivity",
  "specificity",
  "expected_cta_performed",
  "expected_cta_deferred",
  "cta_deferred_percent",
  "expected_false_negative_aas",
  "d_dimer_first_expenditure",
  "testing_expenditure_reduction",
  "testing_expenditure_reduction_percent"
)

make_existing_comparison_row <- function(
  source_id,
  analysis_order,
  analysis_id,
  analysis_label,
  analysis_type
) {
  source_row <- pooled_comparison[
    pooled_comparison$analysis_id == source_id,
    ,
    drop = FALSE
  ]
  if (nrow(source_row) != 1L) {
    stop("Existing economic comparison row did not resolve uniquely.", call. = FALSE)
  }

  result <- data.frame(
    analysis_order = analysis_order,
    analysis_id = analysis_id,
    analysis_label = analysis_label,
    analysis_type = analysis_type,
    point_estimate_type = source_row$point_estimate_type,
    interval_type = source_row$interval_type,
    posterior_draw_count = source_row$posterior_draw_count,
    n_modeled_known_aas = n_modeled,
    n_aas_positive = n_aas_positive,
    n_aas_negative = n_aas_negative,
    cta_unit_cost = cta_unit_cost,
    d_dimer_unit_cost = d_dimer_unit_cost,
    cta_for_all_expenditure = source_row$cta_for_all_expenditure,
    false_negative_monetary_cost_included =
      source_row$false_negative_monetary_cost_included,
    source_file = pooled_comparison_file,
    source_file_sha256 = observed_hashes[["pooled_economic_comparison"]],
    stringsAsFactors = FALSE
  )

  for (metric in comparison_metrics) {
    for (suffix in c("estimate", "lower_95", "upper_95")) {
      column <- paste0(metric, "_", suffix)
      result[[column]] <- source_row[[column]]
    }
  }

  result
}

option2_row <- make_existing_comparison_row(
  source_id = "option2_published_meta_base_case",
  analysis_order = 1L,
  analysis_id = "option2_published_meta_base_case",
  analysis_label = "Option 2 published-meta base case",
  analysis_type = "fixed base case"
)

pooled_row <- make_existing_comparison_row(
  source_id = "option1_weakly_informative",
  analysis_order = 2L,
  analysis_id = "option1_weak_pooled_mean",
  analysis_label = "Option 1 weak-prior pooled-mean analysis",
  analysis_type = "pooled-mean posterior uncertainty"
)

transport_row <- data.frame(
  analysis_order = 3L,
  analysis_id = "option1_weak_new_setting_transportability",
  analysis_label = "Option 1 weak-prior new-setting transportability analysis",
  analysis_type = "new-setting posterior predictive uncertainty",
  point_estimate_type = "posterior predictive median",
  interval_type = "95% posterior predictive interval for a new setting",
  posterior_draw_count = nrow(economic_draws),
  n_modeled_known_aas = n_modeled,
  n_aas_positive = n_aas_positive,
  n_aas_negative = n_aas_negative,
  cta_unit_cost = cta_unit_cost,
  d_dimer_unit_cost = d_dimer_unit_cost,
  cta_for_all_expenditure = unique(economic_draws$cta_for_all_expenditure),
  false_negative_monetary_cost_included = FALSE,
  source_file = posterior_file,
  source_file_sha256 = observed_hashes[["option1_posterior_draws"]],
  stringsAsFactors = FALSE
)

transport_metric_map <- c(
  sensitivity = "sensitivity_new_setting",
  specificity = "specificity_new_setting",
  expected_cta_performed = "expected_cta_performed",
  expected_cta_deferred = "expected_cta_deferred",
  cta_deferred_percent = "cta_deferred_percent",
  expected_false_negative_aas = "expected_false_negative_aas",
  d_dimer_first_expenditure = "d_dimer_first_expenditure",
  testing_expenditure_reduction = "testing_expenditure_reduction",
  testing_expenditure_reduction_percent =
    "testing_expenditure_reduction_percent"
)

for (metric in comparison_metrics) {
  summary_metric <- transport_metric_map[[metric]]
  summary_row <- transport_summary[
    transport_summary$metric == summary_metric,
    ,
    drop = FALSE
  ]
  if (nrow(summary_row) != 1L) {
    stop("Transportability summary row did not resolve uniquely.", call. = FALSE)
  }
  transport_row[[paste0(metric, "_estimate")]] <-
    summary_row$posterior_predictive_median
  transport_row[[paste0(metric, "_lower_95")]] <- summary_row$lower_95
  transport_row[[paste0(metric, "_upper_95")]] <- summary_row$upper_95
}

transport_comparison <- rbind(option2_row, pooled_row, transport_row)
transport_comparison$analytic_base_branch <- analytic_branch
transport_comparison$analytic_base_commit <- analytic_base_commit
transport_comparison$transport_run_id <- transport_run_id
transport_comparison$generating_script <- transport_script


# ---- 7. Validation and variation diagnostics -------------------------------

model_summary <- utils::read.csv(model_summary_file, check.names = FALSE)
expected_prediction_rows <- model_summary[
  model_summary$prior_strategy == "weakly_informative" &
    model_summary$parameter %in% c(
      "prediction_sensitivity_new_study",
      "prediction_specificity_new_study"
    ),
  ,
  drop = FALSE
]
if (nrow(expected_prediction_rows) != 2L) {
  stop("Existing weak-prior prediction summaries did not resolve.", call. = FALSE)
}

validation_rows <- list()

# Collect the source and arithmetic checks before exporting the results.
add_validation <- function(
  check_id,
  category,
  metric,
  statistic,
  expected_numeric = NA_real_,
  observed_numeric = NA_real_,
  tolerance = NA_real_,
  expected_text = NA_character_,
  observed_text = NA_character_,
  pass = NA,
  notes = ""
) {
  validation_rows[[length(validation_rows) + 1L]] <<- data.frame(
    check_id = check_id,
    category = category,
    metric = metric,
    statistic = statistic,
    expected_numeric = expected_numeric,
    observed_numeric = observed_numeric,
    absolute_difference = if (
      is.na(expected_numeric) || is.na(observed_numeric)
    ) {
      NA_real_
    } else {
      abs(observed_numeric - expected_numeric)
    },
    tolerance = tolerance,
    expected_text = expected_text,
    observed_text = observed_text,
    pass = pass,
    notes = notes,
    stringsAsFactors = FALSE
  )
}

for (hash_name in names(expected_hashes)) {
  add_validation(
    check_id = paste0("hash_", hash_name),
    category = "frozen_input_hash",
    metric = hash_name,
    statistic = "SHA-256",
    expected_text = expected_hashes[[hash_name]],
    observed_text = observed_hashes[[hash_name]],
    pass = identical(expected_hashes[[hash_name]], observed_hashes[[hash_name]]),
    notes = "Frozen input must remain byte-identical"
  )
}

add_validation(
  check_id = "posterior_draw_count",
  category = "posterior_structure",
  metric = "paired new-setting draws",
  statistic = "N",
  expected_numeric = 16000,
  observed_numeric = nrow(accuracy_draws),
  tolerance = 0,
  pass = nrow(accuracy_draws) == 16000L,
  notes = "Four chains with 4,000 retained draws each"
)

prediction_validation_map <- list(
  prediction_sensitivity_new_study = list(
    output_metric = "sensitivity_new_setting",
    values = accuracy_draws$sensitivity_new_setting
  ),
  prediction_specificity_new_study = list(
    output_metric = "specificity_new_setting",
    values = accuracy_draws$specificity_new_setting
  )
)

for (parameter in names(prediction_validation_map)) {
  expected_row <- expected_prediction_rows[
    expected_prediction_rows$parameter == parameter,
    ,
    drop = FALSE
  ]
  definition <- prediction_validation_map[[parameter]]
  observed <- summary_triplet(definition$values)
  expected <- c(
    median = expected_row$estimate,
    lower_95 = expected_row$lower_95,
    upper_95 = expected_row$upper_95
  )

  for (statistic in names(expected)) {
    difference <- abs(observed[[statistic]] - expected[[statistic]])
    add_validation(
      check_id = paste0(parameter, "_", statistic),
      category = "prediction_summary_reconciliation",
      metric = definition$output_metric,
      statistic = statistic,
      expected_numeric = expected[[statistic]],
      observed_numeric = observed[[statistic]],
      tolerance = 1e-12,
      pass = difference <= 1e-12,
      notes = paste(
        "Exact reconciliation is expected because the saved paired Stan",
        "generated quantities are reused"
      )
    )
  }
}

max_cta_reconciliation_error <- max(abs(
  economic_draws$expected_cta_performed +
    economic_draws$expected_cta_deferred - n_modeled
))
max_disease_reconciliation_error <- max(abs(
  economic_draws$expected_true_positive_aas +
    economic_draws$expected_false_negative_aas - n_aas_positive
))
max_nondisease_reconciliation_error <- max(abs(
  economic_draws$expected_true_negative_non_aas +
    economic_draws$expected_false_positive_non_aas - n_aas_negative
))

for (check in list(
  list(
    id = "cta_count_reconciliation",
    metric = "CTAs performed plus deferred",
    value = max_cta_reconciliation_error
  ),
  list(
    id = "disease_count_reconciliation",
    metric = "Expected TP plus FN",
    value = max_disease_reconciliation_error
  ),
  list(
    id = "nondisease_count_reconciliation",
    metric = "Expected TN plus FP",
    value = max_nondisease_reconciliation_error
  )
)) {
  add_validation(
    check_id = check$id,
    category = "draw_level_reconciliation",
    metric = check$metric,
    statistic = "maximum absolute error",
    expected_numeric = 0,
    observed_numeric = check$value,
    tolerance = 1e-10,
    pass = check$value <= 1e-10,
    notes = "Checked across all 16,000 posterior-predictive draws"
  )
}

cta_for_all_expected <- n_modeled * cta_unit_cost
cta_for_all_values <- unique(economic_draws$cta_for_all_expenditure)
add_validation(
  check_id = "cta_for_all_expenditure",
  category = "economic_reconciliation",
  metric = "CTA-for-all standardized testing expenditure",
  statistic = "fixed value",
  expected_numeric = cta_for_all_expected,
  observed_numeric = if (length(cta_for_all_values) == 1L) {
    cta_for_all_values
  } else {
    NA_real_
  },
  tolerance = 1e-10,
  pass = length(cta_for_all_values) == 1L &&
    abs(cta_for_all_values - cta_for_all_expected) <= 1e-10,
  notes = "Must match the validated Option 2 comparator"
)

add_validation(
  check_id = "false_negative_monetization",
  category = "economic_reconciliation",
  metric = "False-negative AAS monetary cost",
  statistic = "any included",
  expected_numeric = 0,
  observed_numeric = sum(
    economic_draws$false_negative_monetary_cost_included
  ),
  tolerance = 0,
  pass = !any(economic_draws$false_negative_monetary_cost_included),
  notes = "False-negative AAS remains a separate clinical consequence"
)

new_setting_correlation <- stats::cor(
  accuracy_draws$sensitivity_new_setting,
  accuracy_draws$specificity_new_setting
)
cta_sensitivity_component_sd <- n_aas_positive * stats::sd(
  accuracy_draws$sensitivity_new_setting
)
cta_specificity_component_sd <- n_aas_negative * stats::sd(
  accuracy_draws$specificity_new_setting
)
cta_total_sd <- stats::sd(economic_draws$expected_cta_performed)
cta_component_sd_ratio <-
  cta_specificity_component_sd / cta_sensitivity_component_sd
cta_correlation_sensitivity <- stats::cor(
  economic_draws$expected_cta_performed,
  accuracy_draws$sensitivity_new_setting
)
cta_correlation_specificity <- stats::cor(
  economic_draws$expected_cta_performed,
  accuracy_draws$specificity_new_setting
)
negative_reduction_draws <- sum(
  economic_draws$testing_expenditure_reduction < 0
)

diagnostics <- list(
  list(
    id = "new_setting_accuracy_correlation",
    metric = "Sensitivity-specificity correlation in paired new-setting draws",
    statistic = "Pearson correlation",
    value = new_setting_correlation,
    note = "Empirical correlation after inverse-logit transformation"
  ),
  list(
    id = "cta_sensitivity_component_sd",
    metric = "CTA-count sensitivity component",
    statistic = "marginal SD in expected CTAs",
    value = cta_sensitivity_component_sd,
    note = "43 multiplied by SD of new-setting sensitivity"
  ),
  list(
    id = "cta_specificity_component_sd",
    metric = "CTA-count specificity component",
    statistic = "marginal SD in expected CTAs",
    value = cta_specificity_component_sd,
    note = "878 multiplied by SD of new-setting specificity"
  ),
  list(
    id = "cta_total_sd",
    metric = "Expected CTAs performed",
    statistic = "posterior-predictive SD",
    value = cta_total_sd,
    note = "Includes paired sensitivity-specificity covariance"
  ),
  list(
    id = "cta_component_sd_ratio",
    metric = "Specificity-to-sensitivity CTA variation",
    statistic = "marginal component SD ratio",
    value = cta_component_sd_ratio,
    note = "Descriptive scale comparison; not a variance-share decomposition"
  ),
  list(
    id = "cta_correlation_sensitivity",
    metric = "Expected CTAs and new-setting sensitivity",
    statistic = "Pearson correlation",
    value = cta_correlation_sensitivity,
    note = "Descriptive dependence diagnostic"
  ),
  list(
    id = "cta_correlation_specificity",
    metric = "Expected CTAs and new-setting specificity",
    statistic = "Pearson correlation",
    value = cta_correlation_specificity,
    note = "Descriptive dependence diagnostic"
  ),
  list(
    id = "negative_reduction_draw_count",
    metric = "D-dimer-first expenditure exceeds CTA-for-all",
    statistic = "draw count",
    value = negative_reduction_draws,
    note = "Tail diagnostic; not a prespecified materiality threshold"
  )
)

for (diagnostic in diagnostics) {
  add_validation(
    check_id = diagnostic$id,
    category = "interpretation_diagnostic",
    metric = diagnostic$metric,
    statistic = diagnostic$statistic,
    observed_numeric = diagnostic$value,
    notes = diagnostic$note
  )
}

transport_validation <- do.call(rbind, validation_rows)
if (any(transport_validation$pass %in% FALSE, na.rm = TRUE)) {
  stop("At least one transportability validation check failed.", call. = FALSE)
}


# ---- 8. Write machine-readable outputs -------------------------------------

accuracy_draws_file <- file.path(
  "results",
  "economic_option1_transportability_accuracy_draws.csv.gz"
)
economic_draws_file <- file.path(
  "results",
  "economic_option1_transportability_economic_draws.csv.gz"
)
summary_file <- file.path(
  "results",
  "economic_option1_transportability_summary.csv"
)
comparison_file <- file.path(
  "results",
  "economic_option1_transportability_comparison.csv"
)
validation_file <- file.path(
  "results",
  "validation",
  "economic_option1_transportability_validation.csv"
)

dir.create(dirname(validation_file), recursive = TRUE, showWarnings = FALSE)

# Write the draw-level output in compressed form.
write_gzip_csv <- function(x, path) {
  connection <- gzfile(path, open = "wt", compression = 9)
  on.exit(close(connection), add = TRUE)
  utils::write.csv(x, connection, row.names = FALSE, na = "")
}

write_gzip_csv(accuracy_draws, accuracy_draws_file)
write_gzip_csv(economic_draws, economic_draws_file)
utils::write.csv(transport_summary, summary_file, row.names = FALSE, na = "")
utils::write.csv(transport_comparison, comparison_file, row.names = FALSE, na = "")
utils::write.csv(transport_validation, validation_file, row.names = FALSE, na = "")


# ---- 9. Concise audit note --------------------------------------------------

# Build an aggregate audit note from the checked summaries.
display_number <- function(x, digits = 1L) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

display_currency <- function(x) {
  paste0("$", display_number(x, 2L))
}

display_interval <- function(metric, type = "number", digits = 1L) {
  row <- transport_summary[
    transport_summary$metric == metric,
    ,
    drop = FALSE
  ]
  if (nrow(row) != 1L) {
    stop("Display metric did not resolve uniquely: ", metric, call. = FALSE)
  }

  if (type == "proportion") {
    paste0(
      display_number(100 * row$posterior_predictive_median, digits),
      "% (", display_number(100 * row$lower_95, digits), "–",
      display_number(100 * row$upper_95, digits), "%)"
    )
  } else if (type == "percentage") {
    paste0(
      display_number(row$posterior_predictive_median, digits),
      "% (", display_number(row$lower_95, digits), "–",
      display_number(row$upper_95, digits), "%)"
    )
  } else if (type == "currency") {
    paste0(
      display_currency(row$posterior_predictive_median),
      " (", display_currency(row$lower_95), "–",
      display_currency(row$upper_95), ")"
    )
  } else {
    paste0(
      display_number(row$posterior_predictive_median, digits),
      " (", display_number(row$lower_95, digits), "–",
      display_number(row$upper_95, digits), ")"
    )
  }
}

summary_markdown <- c(
  "| New-setting result | Posterior predictive median (95% PPI) |",
  "|---|---:|",
  paste0(
    "| Sensitivity | ",
    display_interval("sensitivity_new_setting", "proportion"), " |"
  ),
  paste0(
    "| Specificity | ",
    display_interval("specificity_new_setting", "proportion"), " |"
  ),
  paste0(
    "| Expected false-negative AAS | ",
    display_interval("expected_false_negative_aas", "number", 2L), " |"
  ),
  paste0(
    "| Expected true-negative non-AAS encounters | ",
    display_interval("expected_true_negative_non_aas"), " |"
  ),
  paste0(
    "| Expected CTAs performed | ",
    display_interval("expected_cta_performed"), " |"
  ),
  paste0(
    "| Expected CTAs deferred | ",
    display_interval("expected_cta_deferred"), " |"
  ),
  paste0(
    "| CTA deferral | ",
    display_interval("cta_deferred_percent", "percentage"), " |"
  ),
  paste0(
    "| D-dimer-first standardized testing expenditure | ",
    display_interval("d_dimer_first_expenditure", "currency"), " |"
  ),
  paste0(
    "| Absolute standardized expenditure reduction | ",
    display_interval("testing_expenditure_reduction", "currency"), " |"
  ),
  paste0(
    "| Percentage standardized expenditure reduction | ",
    display_interval(
      "testing_expenditure_reduction_percent",
      "percentage"
    ),
    " |"
  )
)

comparison_cell <- function(row, metric, type = "number", digits = 1L) {
  estimate <- row[[paste0(metric, "_estimate")]]
  lower <- row[[paste0(metric, "_lower_95")]]
  upper <- row[[paste0(metric, "_upper_95")]]

  if (type == "proportion") {
    point <- paste0(display_number(100 * estimate, digits), "%")
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(100 * lower, digits), "–",
      display_number(100 * upper, digits), "%)"
    )
  } else if (type == "percentage") {
    point <- paste0(display_number(estimate, digits), "%")
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(lower, digits), "–",
      display_number(upper, digits), "%)"
    )
  } else if (type == "currency") {
    point <- display_currency(estimate)
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_currency(lower), "–", display_currency(upper), ")"
    )
  } else {
    point <- display_number(estimate, digits)
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(lower, digits), "–",
      display_number(upper, digits), ")"
    )
  }

  paste0(point, interval)
}

comparison_markdown <- c(
  paste0(
    "| Analysis | Sensitivity | Specificity | CTAs performed | CTAs deferred | ",
    "CTA deferral | False-negative AAS | D-dimer-first expenditure | ",
    "Absolute reduction | Percentage reduction |"
  ),
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(transport_comparison))) {
  row <- transport_comparison[i, , drop = FALSE]
  comparison_markdown <- c(
    comparison_markdown,
    paste0(
      "| ", row$analysis_label,
      " | ", comparison_cell(row, "sensitivity", "proportion"),
      " | ", comparison_cell(row, "specificity", "proportion"),
      " | ", comparison_cell(row, "expected_cta_performed"),
      " | ", comparison_cell(row, "expected_cta_deferred"),
      " | ", comparison_cell(row, "cta_deferred_percent", "percentage"),
      " | ", comparison_cell(
        row,
        "expected_false_negative_aas",
        "number",
        2L
      ),
      " | ", comparison_cell(
        row,
        "d_dimer_first_expenditure",
        "currency"
      ),
      " | ", comparison_cell(
        row,
        "testing_expenditure_reduction",
        "currency"
      ),
      " | ", comparison_cell(
        row,
        "testing_expenditure_reduction_percent",
        "percentage"
      ),
      " |"
    )
  )
}

fn_range <- range(economic_draws$expected_false_negative_aas)
cta_range <- range(economic_draws$expected_cta_performed)
expenditure_range <- range(economic_draws$d_dimer_first_expenditure)
reduction_range <- range(economic_draws$testing_expenditure_reduction)

audit_lines <- c(
  "# Option 1 New-Setting Transportability Sensitivity Analysis",
  "",
  "## Scope",
  "",
  paste0(
    "This sensitivity analysis asks how the economic and expected clinical ",
    "consequences could vary if the local setting behaved like a new setting ",
    "drawn from the between-study distribution estimated by the weakly ",
    "informative Option 1 bivariate random-effects model. It does not replace ",
    "Option 2 as the current base case."
  ),
  "",
  paste0(
    "The analysis does not refit or modify the Option 1 model, alter Option 2 ",
    "parameters, change `", engine_script, "`, monetize false-negative AAS, ",
    "or edit manuscript text."
  ),
  "",
  "## New-setting draw construction",
  "",
  paste0(
    "The frozen weak-prior posterior file contains 16,000 paired generated ",
    "quantities. In every fitted-model draw, Stan sampled a latent new-setting ",
    "logit-sensitivity/logit-specificity pair from the draw-specific bivariate ",
    "normal distribution using the pooled logits, heterogeneity SDs, and ",
    "correlation Cholesky factor, then applied the inverse-logit transform."
  ),
  "",
  paste0(
    "This script reuses those existing paired generated quantities rather than ",
    "resampling them or combining marginal prediction intervals. Their empirical ",
    "sensitivity-specificity correlation is ",
    display_number(new_setting_correlation, 3L), "."
  ),
  "",
  paste0(
    "These are posterior predictions for latent setting-level accuracy and ",
    "expected outcomes in a cohort of fixed size. They exclude additional ",
    "binomial sampling variation in an observed future study."
  ),
  "",
  "## Fixed economic structure",
  "",
  paste0(
    "Every draw uses 921 ADD-RS 0–1 encounters with known AAS outcome ",
    "(43 AAS-positive and 878 AAS-negative), CTA unit cost ",
    display_currency(cta_unit_cost), ", and D-dimer standardized unit-cost ",
    "proxy ", display_currency(d_dimer_unit_cost), "."
  ),
  "",
  paste0(
    "CTA-for-all expenditure remains ",
    display_currency(cta_for_all_expected),
    ". The D-dimer-first strategy applies D-dimer to all 921 encounters and ",
    "performs CTA after a modeled positive result."
  ),
  "",
  "## Transportability results",
  "",
  summary_markdown,
  "",
  "All intervals in this section are 95% posterior predictive intervals for a new setting, not credible intervals around the pooled mean.",
  "",
  "## Comparison",
  "",
  comparison_markdown,
  "",
  paste0(
    "The Option 1 pooled row uses posterior medians and 95% credible intervals ",
    "for pooled-mean uncertainty. The new-setting row uses posterior predictive ",
    "medians and 95% posterior predictive intervals; the interval types answer ",
    "different questions and should not be conflated."
  ),
  "",
  "## Interpretation checks",
  "",
  paste0(
    "Expected false-negative AAS depends only on sensitivity in the shared ",
    "engine. Its new-setting 95% posterior predictive interval is ",
    display_number(
      transport_summary$lower_95[
        transport_summary$metric == "expected_false_negative_aas"
      ],
      2L
    ),
    " to ",
    display_number(
      transport_summary$upper_95[
        transport_summary$metric == "expected_false_negative_aas"
      ],
      2L
    ),
    " expected cases among 43 AAS-positive encounters."
  ),
  "",
  paste0(
    "CTA utilization and expenditure vary primarily with specificity because ",
    "878 non-AAS encounters contribute to the specificity term, compared with ",
    "43 AAS-positive encounters contributing to the sensitivity term. The ",
    "marginal CTA-count SD component is ",
    display_number(cta_specificity_component_sd, 1L),
    " CTAs for specificity versus ",
    display_number(cta_sensitivity_component_sd, 1L),
    " for sensitivity, a ",
    display_number(cta_component_sd_ratio, 1L),
    "-fold scale difference. Expected CTAs correlate ",
    display_number(cta_correlation_specificity, 4L),
    " with specificity and ",
    display_number(cta_correlation_sensitivity, 4L),
    " with sensitivity across paired draws."
  ),
  "",
  paste0(
    "The 95% posterior predictive ranges are wide: expected CTAs performed ",
    "range from ",
    display_number(
      transport_summary$lower_95[
        transport_summary$metric == "expected_cta_performed"
      ]
    ),
    " to ",
    display_number(
      transport_summary$upper_95[
        transport_summary$metric == "expected_cta_performed"
      ]
    ),
    ", and D-dimer-first expenditure ranges from ",
    display_currency(
      transport_summary$lower_95[
        transport_summary$metric == "d_dimer_first_expenditure"
      ]
    ),
    " to ",
    display_currency(
      transport_summary$upper_95[
        transport_summary$metric == "d_dimer_first_expenditure"
      ]
    ),
    "."
  ),
  "",
  paste0(
    "The full 16,000-draw Monte Carlo sample includes more extreme tail draws: ",
    "expected false-negative AAS ranges from ",
    display_number(fn_range[1], 3L), " to ",
    display_number(fn_range[2], 3L), ", expected CTAs performed from ",
    display_number(cta_range[1], 1L), " to ",
    display_number(cta_range[2], 1L), ", and D-dimer-first expenditure from ",
    display_currency(expenditure_range[1]), " to ",
    display_currency(expenditure_range[2]), ". One of 16,000 draws produces ",
    "expenditure above CTA-for-all, with a maximum negative reduction of ",
    display_currency(abs(min(reduction_range))),
    ". These sample extrema are unstable Monte Carlo tail diagnostics, not ",
    "interval limits or prespecified decision thresholds."
  ),
  "",
  paste0(
    "Therefore, central Option 2 and pooled Option 1 estimates should not be ",
    "interpreted as universally transportable. The predictive distribution ",
    "contains settings with substantially different expected false-negative ",
    "AAS, CTA use, and testing expenditure. This does not establish that any ",
    "individual CTA was unnecessary or safely avoidable."
  ),
  "",
  "## Validation",
  "",
  paste0(
    "The generated sensitivity distribution is ",
    display_interval("sensitivity_new_setting", "proportion"),
    "; the existing Option 1 report gives the same median and 95% prediction ",
    "limits."
  ),
  paste0(
    "The generated specificity distribution is ",
    display_interval("specificity_new_setting", "proportion"),
    "; the existing Option 1 report gives the same median and 95% prediction ",
    "limits."
  ),
  "All six marginal prediction-summary comparisons matched exactly to numeric tolerance because the original saved Stan generated quantities were reused.",
  "All 16,000 draw-level cohort and economic reconciliation checks passed.",
  paste0(
    "The shared engine remained byte-identical at SHA-256 `",
    observed_hashes[["shared_engine"]], "`."
  ),
  "",
  "## Limitations",
  "",
  "- Only six studies inform the Option 1 random-effects distribution; heterogeneity and correlation are uncertain.",
  "- Weakly informative heterogeneity priors permit very wide tails. The full-sample extrema should not be treated as stable probability bounds.",
  "- The analysis projects latent accuracy for a new setting and expected cohort consequences; it does not add observed-study binomial variation.",
  "- Transportability requires comparability of D-dimer assay, threshold, clinical spectrum, and ADD-RS ascertainment; those conditions are not established by this model.",
  "- Unit costs and cohort composition remain fixed and carry no uncertainty in this analysis.",
  "- No sensitivity analysis is promoted to the manuscript base case."
)

audit_file <- file.path(
  "docs",
  "economic_option1_transportability_audit.md"
)
writeLines(audit_lines, audit_file, useBytes = TRUE)


# ---- 10. Aggregate console summary -----------------------------------------

message("\nOption 1 new-setting transportability summary:")
print(
  transport_summary[c(
    "label",
    "posterior_predictive_median",
    "lower_95",
    "upper_95",
    "unit"
  )],
  row.names = FALSE
)

message("\nValidation status:")
print(
  table(
    ifelse(
      is.na(transport_validation$pass),
      "diagnostic",
      ifelse(transport_validation$pass, "pass", "fail")
    )
  )
)
