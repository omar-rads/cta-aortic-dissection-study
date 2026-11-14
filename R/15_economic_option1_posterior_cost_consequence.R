# Why this script is here
# I propagate posterior accuracy draws through the economic model and compare options.
# The draws and source checks come from the separate evidence workflow.

# =============================================================================
# 15_economic_option1_posterior_cost_consequence.R
# AAS CTA R Reanalysis
#
# Option 1 posterior cost-consequence analysis and Option 1/Option 2 comparison
# =============================================================================

source("R/00_setup.R")
source("R/economic_cost_consequence_engine.R")


# ---- 1. Fixed analytic provenance ------------------------------------------

economic_script <- "R/15_economic_option1_posterior_cost_consequence.R"
engine_script <- "R/economic_cost_consequence_engine.R"
option2_script <- "R/14_economic_option2_published_meta.R"
analytic_base_branch <- "codex/r-rebuild-first-pass"
analytic_base_commit <- "b172783206786ee0a23e84c76493eef7d7e1db24"
economic_run_id <- "option1_cost_consequence_2026-09-19_v1"

current_branch <- trimws(
  system2("git", c("branch", "--show-current"), stdout = TRUE)
)

if (!identical(current_branch, analytic_base_branch)) {
  stop(
    "Option 1 economics must run on branch ", analytic_base_branch,
    "; current branch is ", current_branch, ".",
    call. = FALSE
  )
}

# Check the frozen source files before using the posterior draws.
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


# ---- 2. Fixed modeled population and costs ---------------------------------

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

# Confirm the encounter dataset and define the modeled population.
analysis_data <- readRDS(analytic_data_file)

required_variables <- c(
  "encounter",
  "add_rs",
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
    "Modeled-population inputs no longer reconcile to 923 low-ADD-RS, ",
    "921 known-outcome, 43 positive, 878 negative, and 2 unknown encounters.",
    call. = FALSE
  )
}

cta_unit_cost <- 634.96
d_dimer_unit_cost <- 10.18
option2_sensitivity <- 0.965
option2_specificity <- 0.562


# ---- 3. Load and validate the frozen Option 1 posterior draws --------------

posterior_directory <- file.path(
  "external_dimer_evidence",
  "option1_conditional_bivariate",
  "output"
)

posterior_sources <- data.frame(
  analysis_order = c(2L, 3L),
  analysis_id = c(
    "option1_weakly_informative",
    "option1_ren_prior_sensitivity"
  ),
  analysis_label = c(
    "Option 1 weakly informative Bayesian analysis",
    "Option 1 Ren-prior sensitivity analysis"
  ),
  prior_strategy = c("weakly_informative", "ren_informative"),
  posterior_file = file.path(
    posterior_directory,
    c(
      "option1_posterior_draws_weakly_informative.csv.gz",
      "option1_posterior_draws_ren_informative.csv.gz"
    )
  ),
  expected_sha256 = c(
    "065419d8d85246cc40e86c9a12b2ae317f7c29ba74e0862600af3601553e042f",
    "fbf310b3135074d9f5fb782077e72acac9a834bc5d6449a605d8b645812a292d"
  ),
  stringsAsFactors = FALSE
)

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

expected_model_sha256 <-
  "ccdaf5cf2f85af8455d9a86e9217fe8899b35875e2bf81b4768ceb0ecf85adc1"
expected_source_sha256 <-
  "e7c5c520bebe51ba5b22ef54c6fb5a0672858be4054cef0fc2039c492a0969db"
expected_option1_run_id <- "option1_2026-09-19_v1"

# Read and validate the saved posterior draws for each source.
read_posterior_draws <- function(source_row) {
  path <- source_row$posterior_file
  observed_sha256 <- sha256_file(path)

  if (!identical(observed_sha256, source_row$expected_sha256)) {
    stop("Posterior-draw SHA-256 mismatch: ", path, call. = FALSE)
  }

  connection <- gzfile(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  draws <- utils::read.csv(connection, check.names = FALSE)

  if (!identical(names(draws), required_draw_columns)) {
    stop("Posterior-draw schema changed: ", path, call. = FALSE)
  }

  if (
    nrow(draws) != 16000L ||
    anyDuplicated(draws$draw_id) ||
    !identical(sort(unique(draws$chain)), 1:4) ||
    any(tabulate(draws$chain, nbins = 4L) != 4000L)
  ) {
    stop("Posterior-draw count or chain structure changed: ", path, call. = FALSE)
  }

  if (
    any(!is.finite(draws$pooled_sensitivity)) ||
    any(!is.finite(draws$pooled_specificity)) ||
    any(draws$pooled_sensitivity < 0 | draws$pooled_sensitivity > 1) ||
    any(draws$pooled_specificity < 0 | draws$pooled_specificity > 1)
  ) {
    stop("Posterior accuracy draws are invalid: ", path, call. = FALSE)
  }

  expected_values <- list(
    evidence_strategy = "option1_conditional_bivariate",
    prior_strategy = source_row$prior_strategy,
    run_id = expected_option1_run_id,
    model_sha256 = expected_model_sha256,
    source_data_sha256 = expected_source_sha256
  )

  for (variable in names(expected_values)) {
    if (!identical(unique(draws[[variable]]), expected_values[[variable]])) {
      stop(
        "Posterior provenance changed for ", variable, ": ", path,
        call. = FALSE
      )
    }
  }

  attr(draws, "observed_sha256") <- observed_sha256
  draws
}

posterior_draws <- lapply(
  seq_len(nrow(posterior_sources)),
  function(i) read_posterior_draws(posterior_sources[i, , drop = FALSE])
)
names(posterior_draws) <- posterior_sources$prior_strategy


# ---- 4. Run Option 2 and every Option 1 draw through the shared engine ------

option2_modeled <- calculate_cost_consequence(
  aas_positive = n_aas_positive,
  aas_negative = n_aas_negative,
  sensitivity = option2_sensitivity,
  specificity = option2_specificity,
  cta_unit_cost = cta_unit_cost,
  d_dimer_unit_cost = d_dimer_unit_cost
)

# Apply the same cost-consequence engine to each accuracy draw.
run_option1_draws <- function(draws) {
  modeled <- calculate_cost_consequence(
    aas_positive = n_aas_positive,
    aas_negative = n_aas_negative,
    sensitivity = draws$pooled_sensitivity,
    specificity = draws$pooled_specificity,
    cta_unit_cost = cta_unit_cost,
    d_dimer_unit_cost = d_dimer_unit_cost
  )

  data.frame(
    sensitivity = draws$pooled_sensitivity,
    specificity = draws$pooled_specificity,
    modeled,
    stringsAsFactors = FALSE
  )
}

option1_modeled_draws <- lapply(posterior_draws, run_option1_draws)


# ---- 5. Summarize posterior outcomes and differences from Option 2 ---------

metric_variables <- c(
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

# Summarize the modeled outcomes and compare them with Option 2.
summary_triplet <- function(x) {
  c(
    estimate = stats::median(x),
    lower_95 = unname(stats::quantile(x, 0.025)),
    upper_95 = unname(stats::quantile(x, 0.975))
  )
}

option2_values <- c(
  sensitivity = option2_sensitivity,
  specificity = option2_specificity,
  unlist(option2_modeled[setdiff(metric_variables, c("sensitivity", "specificity"))])
)

make_base_row <- function(
  analysis_order,
  analysis_id,
  analysis_label,
  evidence_option,
  prior_strategy,
  evidence_source,
  posterior_file = NA_character_,
  posterior_sha256 = NA_character_,
  posterior_draw_count = NA_integer_,
  point_estimate_type,
  interval_type
) {
  data.frame(
    analysis_order = analysis_order,
    analysis_id = analysis_id,
    analysis_label = analysis_label,
    evidence_option = evidence_option,
    prior_strategy = prior_strategy,
    evidence_source = evidence_source,
    posterior_file = posterior_file,
    posterior_sha256 = posterior_sha256,
    posterior_draw_count = posterior_draw_count,
    point_estimate_type = point_estimate_type,
    interval_type = interval_type,
    n_add_rs_0_1 = n_add_rs_0_1,
    n_modeled_known_aas = n_modeled,
    n_aas_positive = n_aas_positive,
    n_aas_negative = n_aas_negative,
    n_aas_unknown_excluded = n_aas_unknown,
    cta_unit_cost = cta_unit_cost,
    d_dimer_unit_cost = d_dimer_unit_cost,
    cta_for_all_expenditure = option2_modeled$cta_for_all_expenditure,
    false_negative_monetary_cost_included = FALSE,
    stringsAsFactors = FALSE
  )
}

option2_summary <- make_base_row(
  analysis_order = 1L,
  analysis_id = "option2_published_meta_base_case",
  analysis_label = "Option 2 published-meta base case",
  evidence_option = "Option 2",
  prior_strategy = "not applicable",
  evidence_source = paste(
    "Published 2024 D-dimer meta-analysis; fixed estimates supplied",
    "in the Option 2 specification"
  ),
  point_estimate_type = "fixed external input",
  interval_type = "none; deterministic Option 2 input"
)

for (metric in metric_variables) {
  option2_summary[[paste0(metric, "_estimate")]] <- option2_values[[metric]]
  option2_summary[[paste0(metric, "_lower_95")]] <- NA_real_
  option2_summary[[paste0(metric, "_upper_95")]] <- NA_real_
}

option1_summaries <- lapply(seq_len(nrow(posterior_sources)), function(i) {
  source_row <- posterior_sources[i, , drop = FALSE]
  modeled_draws <- option1_modeled_draws[[source_row$prior_strategy]]
  summary_row <- make_base_row(
    analysis_order = source_row$analysis_order,
    analysis_id = source_row$analysis_id,
    analysis_label = source_row$analysis_label,
    evidence_option = "Option 1",
    prior_strategy = source_row$prior_strategy,
    evidence_source = paste(
      "Existing paired posterior draws of pooled sensitivity and specificity",
      "from the conditional Bayesian bivariate random-effects model"
    ),
    posterior_file = source_row$posterior_file,
    posterior_sha256 = attr(
      posterior_draws[[source_row$prior_strategy]],
      "observed_sha256"
    ),
    posterior_draw_count = nrow(modeled_draws),
    point_estimate_type = "posterior median",
    interval_type = "equal-tail 95% credible interval"
  )

  for (metric in metric_variables) {
    metric_summary <- summary_triplet(modeled_draws[[metric]])
    summary_row[[paste0(metric, "_estimate")]] <- metric_summary[["estimate"]]
    summary_row[[paste0(metric, "_lower_95")]] <- metric_summary[["lower_95"]]
    summary_row[[paste0(metric, "_upper_95")]] <- metric_summary[["upper_95"]]
  }

  summary_row
})

comparison_table <- do.call(
  rbind,
  c(list(option2_summary), option1_summaries)
)
comparison_table <- comparison_table[order(comparison_table$analysis_order), ]
row.names(comparison_table) <- NULL

for (metric in metric_variables) {
  estimate_column <- paste0(metric, "_estimate")
  difference_estimate_column <- paste0(
    "difference_vs_option2_", metric, "_estimate"
  )
  difference_lower_column <- paste0(
    "difference_vs_option2_", metric, "_lower_95"
  )
  difference_upper_column <- paste0(
    "difference_vs_option2_", metric, "_upper_95"
  )
  absolute_difference_column <- paste0(
    "absolute_difference_vs_option2_", metric, "_estimate"
  )

  comparison_table[[difference_estimate_column]] <-
    comparison_table[[estimate_column]] - option2_values[[metric]]
  comparison_table[[difference_lower_column]] <- NA_real_
  comparison_table[[difference_upper_column]] <- NA_real_
  comparison_table[[absolute_difference_column]] <- abs(
    comparison_table[[difference_estimate_column]]
  )

  for (i in 2:3) {
    modeled_draws <- option1_modeled_draws[[comparison_table$prior_strategy[i]]]
    difference_summary <- summary_triplet(
      modeled_draws[[metric]] - option2_values[[metric]]
    )
    comparison_table[[difference_lower_column]][i] <-
      difference_summary[["lower_95"]]
    comparison_table[[difference_upper_column]][i] <-
      difference_summary[["upper_95"]]
  }
}

comparison_table$analytic_data_source <-
  "R/04_variable_derivation.R output: aas_analytic_encounters.rds"
comparison_table$shared_engine_script <- engine_script
comparison_table$shared_engine_sha256 <- sha256_file(engine_script)
comparison_table$option2_generating_script <- option2_script
comparison_table$analytic_base_branch <- analytic_base_branch
comparison_table$analytic_base_commit <- analytic_base_commit
comparison_table$economic_run_id <- economic_run_id
comparison_table$r_version <- R.version.string
comparison_table$generating_script <- economic_script
comparison_table$generating_object <- "comparison_table"


# ---- 6. Internal validation -------------------------------------------------

tolerance <- 1e-8

if (nrow(comparison_table) != 3L || anyDuplicated(comparison_table$analysis_id)) {
  stop("The comparison table must contain three unique analyses.", call. = FALSE)
}

primary_expected <- c(
  expected_cta_performed = 426.059,
  expected_cta_deferred = 494.941,
  expected_false_negative_aas = 1.505,
  d_dimer_first_expenditure = 279906.20264,
  testing_expenditure_reduction = 304891.95736,
  testing_expenditure_reduction_percent = 52.1362716599519
)

if (any(abs(
  unlist(option2_modeled[names(primary_expected)]) - primary_expected
) > tolerance)) {
  stop("Option 2 shared-engine regression check failed.", call. = FALSE)
}

option2_reference_file <- file.path(
  "results",
  "economic_option2_published_meta_results.csv"
)
option2_reference <- utils::read.csv(option2_reference_file, check.names = FALSE)
option2_reference <- option2_reference[
  option2_reference$scenario_id == "base_case_published_meta",
  ,
  drop = FALSE
]

if (
  nrow(option2_reference) != 1L ||
  any(abs(
    unlist(option2_reference[names(primary_expected)]) - primary_expected
  ) > tolerance)
) {
  stop("Validated Option 2 output no longer matches the shared engine.", call. = FALSE)
}

for (prior_strategy in names(option1_modeled_draws)) {
  modeled_draws <- option1_modeled_draws[[prior_strategy]]

  if (any(abs(
    modeled_draws$expected_cta_performed +
      modeled_draws$expected_cta_deferred - n_modeled
  ) > tolerance)) {
    stop("CTA counts failed to reconcile for ", prior_strategy, ".", call. = FALSE)
  }

  if (any(abs(
    modeled_draws$expected_tp + modeled_draws$expected_fn - n_aas_positive
  ) > tolerance)) {
    stop("Diseased counts failed to reconcile for ", prior_strategy, ".", call. = FALSE)
  }

  if (any(abs(
    modeled_draws$expected_tn + modeled_draws$expected_fp - n_aas_negative
  ) > tolerance)) {
    stop("Nondiseased counts failed to reconcile for ", prior_strategy, ".", call. = FALSE)
  }

  if (any(modeled_draws$false_negative_monetary_cost_included)) {
    stop("False-negative monetary costs must remain excluded.", call. = FALSE)
  }
}

# Check the computed results against the recorded reference values.
expected_option1_medians <- data.frame(
  prior_strategy = c("weakly_informative", "ren_informative"),
  sensitivity = c(0.967254275, 0.971987035),
  specificity = c(0.54657899, 0.544332155),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(expected_option1_medians))) {
  prior_strategy <- expected_option1_medians$prior_strategy[i]
  row <- comparison_table[
    comparison_table$prior_strategy == prior_strategy,
    ,
    drop = FALSE
  ]
  if (
    nrow(row) != 1L ||
    abs(row$sensitivity_estimate - expected_option1_medians$sensitivity[i]) >
      tolerance ||
    abs(row$specificity_estimate - expected_option1_medians$specificity[i]) >
      tolerance
  ) {
    stop("Option 1 posterior median regression check failed.", call. = FALSE)
  }
}


# ---- 7. Write the single machine-readable comparison table -----------------

comparison_output_file <- file.path(
  "results",
  "economic_option1_option2_comparison.csv"
)

utils::write.csv(
  comparison_table,
  comparison_output_file,
  row.names = FALSE,
  na = ""
)


# ---- 8. Write the aggregate methods/results audit note ----------------------

# Format the aggregate audit note after the checks pass.
display_number <- function(x, digits = 1L) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

display_signed <- function(x, digits = 1L, prefix = "") {
  paste0(
    ifelse(x > 0, "+", ifelse(x < 0, "-", "")),
    prefix,
    display_number(abs(x), digits)
  )
}

summary_cell <- function(row, metric, type) {
  estimate <- row[[paste0(metric, "_estimate")]]
  lower <- row[[paste0(metric, "_lower_95")]]
  upper <- row[[paste0(metric, "_upper_95")]]
  difference <- row[[paste0("difference_vs_option2_", metric, "_estimate")]]

  if (type == "proportion") {
    value <- paste0(display_number(100 * estimate, 1L), "%")
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(100 * lower, 1L), "–",
      display_number(100 * upper, 1L), ")"
    )
    delta <- if (row$analysis_order == 1L) "" else paste0(
      "; Δ ", display_signed(100 * difference, 1L), " pp"
    )
  } else if (type == "percentage") {
    value <- paste0(display_number(estimate, 1L), "%")
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(lower, 1L), "–",
      display_number(upper, 1L), ")"
    )
    delta <- if (row$analysis_order == 1L) "" else paste0(
      "; Δ ", display_signed(difference, 1L), " pp"
    )
  } else if (type == "currency") {
    value <- paste0("$", display_number(estimate, 2L))
    interval <- if (is.na(lower)) "" else paste0(
      " ($", display_number(lower, 2L), "–$",
      display_number(upper, 2L), ")"
    )
    delta <- if (row$analysis_order == 1L) "" else paste0(
      "; Δ ", display_signed(difference, 2L, prefix = "$")
    )
  } else {
    value <- display_number(estimate, 1L)
    interval <- if (is.na(lower)) "" else paste0(
      " (", display_number(lower, 1L), "–",
      display_number(upper, 1L), ")"
    )
    delta <- if (row$analysis_order == 1L) "" else paste0(
      "; Δ ", display_signed(difference, 1L)
    )
  }

  paste0(value, interval, delta)
}

comparison_markdown <- c(
  paste0(
    "| Evidence source | Sensitivity | Specificity | CTAs performed | ",
    "CTAs deferred | CTA deferral | False-negative AAS | ",
    "D-dimer-first expenditure | Absolute expenditure reduction | ",
    "Expenditure reduction |"
  ),
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(comparison_table))) {
  row <- comparison_table[i, , drop = FALSE]
  comparison_markdown <- c(
    comparison_markdown,
    paste0(
      "| ", row$analysis_label,
      " | ", summary_cell(row, "sensitivity", "proportion"),
      " | ", summary_cell(row, "specificity", "proportion"),
      " | ", summary_cell(row, "expected_cta_performed", "count"),
      " | ", summary_cell(row, "expected_cta_deferred", "count"),
      " | ", summary_cell(row, "cta_deferred_percent", "percentage"),
      " | ", summary_cell(row, "expected_false_negative_aas", "count"),
      " | ", summary_cell(row, "d_dimer_first_expenditure", "currency"),
      " | ", summary_cell(row, "testing_expenditure_reduction", "currency"),
      " | ", summary_cell(
        row,
        "testing_expenditure_reduction_percent",
        "percentage"
      ),
      " |"
    )
  )
}

weak_row <- comparison_table[
  comparison_table$analysis_id == "option1_weakly_informative",
  ,
  drop = FALSE
]
ren_row <- comparison_table[
  comparison_table$analysis_id == "option1_ren_prior_sensitivity",
  ,
  drop = FALSE
]

audit_lines <- c(
  "# Option 1 and Option 2 Cost-Consequence Comparison",
  "",
  "## Scope",
  "",
  paste0(
    "This analysis applies the existing Option 1 paired posterior draws to ",
    "the same parameterized cost-consequence engine used by Option 2. It ",
    "does not refit or alter either Bayesian evidence-synthesis model, alter ",
    "Option 2 parameters, select a manuscript base case, or edit manuscript text."
  ),
  "",
  paste0(
    "The shared engine is `", engine_script, "` (SHA-256 `",
    sha256_file(engine_script), "`). `", option2_script,
    "` and `", economic_script, "` both source that file."
  ),
  "",
  "## Fixed modeled population and economic inputs",
  "",
  paste0(
    "All analyses use 921 ADD-RS 0–1 encounters with known AAS outcome ",
    "(43 AAS-positive and 878 AAS-negative), CTA unit cost $",
    display_number(cta_unit_cost, 2L), ", and D-dimer unit cost $",
    display_number(d_dimer_unit_cost, 2L), "."
  ),
  "",
  paste0(
    "CTA-for-all standardized testing expenditure is $",
    display_number(option2_modeled$cta_for_all_expenditure, 2L),
    " for every analysis. Under D-dimer-first, all 921 encounters receive ",
    "D-dimer and only modeled positive results proceed to CTA. False-negative ",
    "AAS remains a separate expected clinical consequence and is not monetized."
  ),
  "",
  "## Posterior propagation",
  "",
  paste0(
    "For each of 16,000 saved draws per prior, the paired `pooled_sensitivity` ",
    "and `pooled_specificity` values enter the shared engine together. This ",
    "preserves their within-draw joint uncertainty. The analysis does not use ",
    "the separate latent new-study prediction draws."
  ),
  "",
  paste0(
    "Every Option 1 outcome is summarized by its posterior median and ",
    "equal-tail 95% credible interval, matching the established Option 1 ",
    "summary convention. Option 2 is a fixed input and therefore has no ",
    "posterior interval."
  ),
  "",
  "## Comparison",
  "",
  comparison_markdown,
  "",
  paste0(
    "Parentheses are 95% credible intervals for Option 1. Δ is the posterior ",
    "median Option 1 result minus the fixed Option 2 result in the displayed ",
    "unit. The machine-readable table also contains the equal-tail 95% interval ",
    "for every signed difference and the absolute median difference."
  ),
  "",
  "## Interpretation of evidence-source impact",
  "",
  paste0(
    "Relative to Option 2, the weak-prior posterior median changes expected ",
    "CTAs performed by ",
    display_signed(
      weak_row$difference_vs_option2_expected_cta_performed_estimate,
      1L
    ),
    ", expected false-negative AAS by ",
    display_signed(
      weak_row$difference_vs_option2_expected_false_negative_aas_estimate,
      2L
    ),
    ", and D-dimer-first expenditure by ",
    display_signed(
      weak_row$difference_vs_option2_d_dimer_first_expenditure_estimate,
      2L,
      prefix = "$"
    ),
    "."
  ),
  "",
  paste0(
    "The Ren-prior posterior median changes the same outcomes by ",
    display_signed(
      ren_row$difference_vs_option2_expected_cta_performed_estimate,
      1L
    ),
    ", ",
    display_signed(
      ren_row$difference_vs_option2_expected_false_negative_aas_estimate,
      2L
    ),
    ", and ",
    display_signed(
      ren_row$difference_vs_option2_d_dimer_first_expenditure_estimate,
      2L,
      prefix = "$"
    ),
    ", respectively."
  ),
  "",
  paste0(
    "No materiality threshold was prespecified, so the comparison does not ",
    "assign a formal material/nonmaterial label. The central Option 1 estimates ",
    "do not reverse the qualitative Option 2 result: D-dimer-first still ",
    "defers approximately half of CTAs and lowers standardized testing ",
    "expenditure, while expected false-negative AAS remains nonzero. The ",
    "evidence-source choice does affect uncertainty. The weak-prior intervals ",
    "are especially wide for specificity-dependent CTA use and expenditure; ",
    "the Ren-prior analysis narrows those intervals but is explicitly a ",
    "prior-sensitivity analysis."
  ),
  "",
  "## Provenance and validation",
  "",
  paste0(
    "- Weakly informative posterior draws: `",
    posterior_sources$posterior_file[1], "`; SHA-256 `",
    posterior_sources$expected_sha256[1], "`."
  ),
  paste0(
    "- Ren-prior posterior draws: `",
    posterior_sources$posterior_file[2], "`; SHA-256 `",
    posterior_sources$expected_sha256[2], "`."
  ),
  paste0(
    "- Both draw files identify Option 1 run `", expected_option1_run_id,
    "`, bivariate model SHA-256 `", expected_model_sha256,
    "`, and source-data SHA-256 `", expected_source_sha256, "`."
  ),
  paste0(
    "- The shared engine reproduces the validated Option 2 primary expected ",
    "counts and expenditures exactly within numeric tolerance and matches ",
    "`results/economic_option2_published_meta_results.csv`."
  ),
  paste0(
    "- Every posterior draw passed count reconciliation: CTAs performed plus ",
    "deferred equals 921, expected true-positive plus false-negative equals 43, ",
    "and expected true-negative plus false-positive equals 878."
  ),
  "- No patient identifier or patient-level field is written to either output.",
  "",
  "## Limitations",
  "",
  "- Option 1 is a secondary synthesis of six published study-level conditional tables; its transportability to this local CTA-selected cohort remains uncertain.",
  "- The weakly informative and Ren-style analyses use the same likelihood but different heterogeneity priors; the Ren-style result remains a prior-sensitivity analysis.",
  "- Posterior credible intervals represent uncertainty in pooled sensitivity and specificity propagated through this deterministic cost-consequence model. They do not include uncertainty in unit costs, cohort size, AAS prevalence, or downstream outcomes.",
  "- Standardized expenditure includes only CTA and D-dimer testing. It does not value false-negative AAS or any downstream care.",
  "- This comparison does not choose the final manuscript evidence source."
)

audit_output_file <- file.path(
  "docs",
  "economic_option1_option2_comparison.md"
)
writeLines(audit_lines, audit_output_file, useBytes = TRUE)


# ---- 9. Aggregate console summary ------------------------------------------

message("\nOption 1/Option 2 cost-consequence comparison:")
print(
  comparison_table[c(
    "analysis_label",
    "sensitivity_estimate",
    "sensitivity_lower_95",
    "sensitivity_upper_95",
    "specificity_estimate",
    "specificity_lower_95",
    "specificity_upper_95",
    "expected_cta_performed_estimate",
    "expected_cta_performed_lower_95",
    "expected_cta_performed_upper_95",
    "expected_cta_deferred_estimate",
    "expected_false_negative_aas_estimate",
    "d_dimer_first_expenditure_estimate",
    "testing_expenditure_reduction_estimate",
    "testing_expenditure_reduction_percent_estimate"
  )],
  row.names = FALSE
)
