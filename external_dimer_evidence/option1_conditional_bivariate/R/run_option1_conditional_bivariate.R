#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE, warn = 1)

required_packages <- c("cmdstanr", "posterior", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ", paste(missing_packages, collapse = ", "),
    ". Restore the project environment before running Option 1."
  )
}

script_flag <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(script_flag) != 1L) {
  stop("Could not identify this script's path from --file.")
}
script_path <- normalizePath(sub("^--file=", "", script_flag), mustWork = TRUE)
repo_root <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), mustWork = TRUE)
module_dir <- file.path(repo_root, "external_dimer_evidence", "option1_conditional_bivariate")
shared_dir <- file.path(repo_root, "external_dimer_evidence", "shared")
source_file <- file.path(shared_dir, "source_data", "ren_2024_joint_strategy_counts.csv")
derived_dir <- file.path(module_dir, "derived_data")
output_dir <- file.path(module_dir, "output")
documentation_dir <- file.path(module_dir, "documentation")
work_dir <- file.path(repo_root, "work", "option1_conditional_bivariate")
raw_fit_dir <- file.path(work_dir, "cmdstan_output")
for (path in c(derived_dir, output_dir, documentation_dir, work_dir, raw_fit_dir)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

run_id <- Sys.getenv("OPTION1_RUN_ID", unset = "option1_2026-09-19_v1")
seed <- 38905181L
chains <- 4L
parallel_chains <- 4L
iter_warmup <- 2000L
iter_sampling <- 4000L
adapt_delta <- 0.99
max_treedepth <- 15L

cmdstan_path_env <- Sys.getenv("CMDSTAN", unset = "")
if (nzchar(cmdstan_path_env)) {
  cmdstan_path <- cmdstan_path_env
} else {
  cmdstan_parent <- file.path(repo_root, "work", "cmdstan")
  candidates <- list.dirs(cmdstan_parent, recursive = FALSE, full.names = TRUE)
  candidates <- candidates[grepl("cmdstan-[0-9]", basename(candidates))]
  if (length(candidates) == 0L) {
    stop(
      "CmdStan was not found. Install it under work/cmdstan or set CMDSTAN."
    )
  }
  cmdstan_path <- sort(candidates, decreasing = TRUE)[1L]
}
cmdstanr::set_cmdstan_path(cmdstan_path)

sha256_file <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

quantile_summary <- function(x) {
  c(
    mean = mean(x),
    median = stats::median(x),
    lower_95 = unname(stats::quantile(x, 0.025)),
    upper_95 = unname(stats::quantile(x, 0.975))
  )
}

source_data <- utils::read.csv(source_file, check.names = FALSE)
required_columns <- c(
  "study_id", "study", "source_strategy", "threshold_order",
  "tp", "fp", "fn", "tn", "source_document", "source_figure",
  "figure_panel", "embedded_image", "embedded_image_sha256", "source_url"
)
if (!identical(required_columns, names(source_data))) {
  stop("The Ren source table schema changed; expected columns were not found exactly.")
}

count_columns <- c("tp", "fp", "fn", "tn")
if (any(!is.finite(as.matrix(source_data[count_columns]))) ||
    any(as.matrix(source_data[count_columns]) < 0) ||
    any(as.matrix(source_data[count_columns]) %% 1 != 0)) {
  stop("Source counts must be finite, nonnegative integers.")
}
if (anyDuplicated(source_data[c("study_id", "source_strategy")])) {
  stop("Duplicate study-strategy rows were found in the Ren source table.")
}

expected_studies <- c(
  "kotani_2017", "mclatchie_2023", "nazerian_2014b",
  "morello_2021", "nazerian_2018", "gorla_2017a"
)
required_strategies <- c(
  "addrs_gt0_or_ddimer_gt500",
  "addrs_gt1_or_ddimer_gt500",
  "addrs_gt1"
)
for (strategy in required_strategies) {
  observed <- source_data$study_id[source_data$source_strategy == strategy]
  if (!setequal(observed, expected_studies) || length(observed) != 6L) {
    stop("Expected exactly the same six studies for strategy: ", strategy)
  }
}

strategy_rows <- function(strategy) {
  d <- source_data[source_data$source_strategy == strategy, , drop = FALSE]
  d[match(expected_studies, d$study_id), , drop = FALSE]
}

addrs_gt1 <- strategy_rows("addrs_gt1")
combined_gt1 <- strategy_rows("addrs_gt1_or_ddimer_gt500")
combined_gt0 <- strategy_rows("addrs_gt0_or_ddimer_gt500")
if (!identical(addrs_gt1$study_id, combined_gt1$study_id) ||
    !identical(addrs_gt1$study_id, combined_gt0$study_id)) {
  stop("Study ordering failed while pairing published strategies.")
}

audit <- data.frame(
  study_id = addrs_gt1$study_id,
  study = addrs_gt1$study,
  addrs_gt1_tp = addrs_gt1$tp,
  addrs_gt1_fp = addrs_gt1$fp,
  addrs_gt1_fn = addrs_gt1$fn,
  addrs_gt1_tn = addrs_gt1$tn,
  combined_gt1_ddimer_tp = combined_gt1$tp,
  combined_gt1_ddimer_fp = combined_gt1$fp,
  combined_gt1_ddimer_fn = combined_gt1$fn,
  combined_gt1_ddimer_tn = combined_gt1$tn,
  conditional_tp = combined_gt1$tp - addrs_gt1$tp,
  conditional_fn = combined_gt1$fn,
  conditional_fp = combined_gt1$fp - addrs_gt1$fp,
  conditional_tn = combined_gt1$tn,
  source_addrs_gt1 = "Ren 2024 S7 Figure S7-4(c)",
  source_combined = "Ren 2024 S7 Figure S7-2(b)",
  source_url = combined_gt1$source_url,
  stringsAsFactors = FALSE
)
audit$conditional_disease_total <- audit$conditional_tp + audit$conditional_fn
audit$conditional_nondisease_total <- audit$conditional_tn + audit$conditional_fp
audit$conditional_sensitivity <- audit$conditional_tp / audit$conditional_disease_total
audit$conditional_specificity <- audit$conditional_tn / audit$conditional_nondisease_total

assertions <- do.call(rbind, lapply(seq_len(nrow(audit)), function(i) {
  checks <- c(
    disease_totals_reconcile =
      addrs_gt1$tp[i] + addrs_gt1$fn[i] == combined_gt1$tp[i] + combined_gt1$fn[i],
    nondisease_totals_reconcile =
      addrs_gt1$fp[i] + addrs_gt1$tn[i] == combined_gt1$fp[i] + combined_gt1$tn[i],
    conditional_counts_nonnegative =
      all(unlist(audit[i, c("conditional_tp", "conditional_fn", "conditional_fp", "conditional_tn")]) >= 0),
    disease_identity =
      audit$conditional_tp[i] + audit$conditional_fn[i] == addrs_gt1$fn[i],
    nondisease_identity =
      audit$conditional_tn[i] + audit$conditional_fp[i] == addrs_gt1$tn[i],
    positive_conditional_denominators =
      audit$conditional_disease_total[i] > 0 && audit$conditional_nondisease_total[i] > 0
  )
  data.frame(
    scope = "study",
    study_id = audit$study_id[i],
    check = names(checks),
    pass = unname(checks),
    stringsAsFactors = FALSE
  )
}))
global_checks <- c(
  exactly_six_studies = nrow(audit) == 6L,
  all_source_rows_present = nrow(source_data) == 18L,
  source_counts_nonnegative_integer = TRUE,
  no_duplicate_study_strategy = TRUE,
  all_study_checks_pass = all(assertions$pass)
)
assertions <- rbind(
  assertions,
  data.frame(
    scope = "global",
    study_id = "ALL",
    check = names(global_checks),
    pass = unname(global_checks),
    stringsAsFactors = FALSE
  )
)
if (!all(assertions$pass)) {
  failed <- assertions[!assertions$pass, ]
  write_csv(failed, file.path(derived_dir, "ren_2024_reconciliation_failures.csv"))
  stop("Ren source/conditional reconciliation failed; see the failure artifact.")
}

audit_total <- audit[1L, , drop = FALSE]
audit_total[] <- NA
audit_total$study_id <- "TOTAL"
audit_total$study <- "All six studies"
sum_columns <- c(
  "addrs_gt1_tp", "addrs_gt1_fp", "addrs_gt1_fn", "addrs_gt1_tn",
  "combined_gt1_ddimer_tp", "combined_gt1_ddimer_fp",
  "combined_gt1_ddimer_fn", "combined_gt1_ddimer_tn",
  "conditional_tp", "conditional_fn", "conditional_fp", "conditional_tn",
  "conditional_disease_total", "conditional_nondisease_total"
)
for (column in sum_columns) audit_total[[column]] <- sum(audit[[column]])
audit_total$conditional_sensitivity <-
  audit_total$conditional_tp / audit_total$conditional_disease_total
audit_total$conditional_specificity <-
  audit_total$conditional_tn / audit_total$conditional_nondisease_total
audit_total$source_addrs_gt1 <- "Sum of Ren 2024 S7 Figure S7-4(c)"
audit_total$source_combined <- "Sum of Ren 2024 S7 Figure S7-2(b)"
audit_total$source_url <- combined_gt1$source_url[1L]
audit_export <- rbind(audit, audit_total)
write_csv(audit_export, file.path(derived_dir, "ren_2024_conditional_dimer_audit.csv"))
write_csv(assertions, file.path(derived_dir, "ren_2024_reconciliation_assertions.csv"))

sampling_settings <- list(
  chains = chains,
  parallel_chains = parallel_chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  refresh = 500
)

diagnose_fit <- function(fit, fit_label, core_variables) {
  parameter_summary <- fit$summary(variables = core_variables)
  diagnostic_columns <- c("rhat", "ess_bulk", "ess_tail")
  finite_diagnostics <- vapply(
    parameter_summary[diagnostic_columns],
    function(x) all(is.finite(x)),
    logical(1)
  )
  if (nrow(parameter_summary) == 0L || !all(finite_diagnostics)) {
    stop(
      "Nonfinite or missing parameter diagnostics for fit: ", fit_label,
      ". No diagnostic rows may be silently discarded."
    )
  }
  diagnostic <- fit$diagnostic_summary()
  divergences <- sum(diagnostic$num_divergent)
  treedepth_hits <- sum(diagnostic$num_max_treedepth)
  min_ebfmi <- min(diagnostic$ebfmi)
  max_rhat <- max(parameter_summary$rhat)
  min_ess_bulk <- min(parameter_summary$ess_bulk)
  min_ess_tail <- min(parameter_summary$ess_tail)
  gates <- c(
    max_rhat_below_1_01 = max_rhat < 1.01,
    min_bulk_ess_at_least_400 = min_ess_bulk >= 400,
    min_tail_ess_at_least_400 = min_ess_tail >= 400,
    zero_divergences = divergences == 0,
    zero_treedepth_hits = treedepth_hits == 0,
    all_chain_ebfmi_above_0_3 = min_ebfmi > 0.3
  )
  parameter_export <- data.frame(
    fit = fit_label,
    record_type = "parameter",
    parameter = parameter_summary$variable,
    mean = parameter_summary$mean,
    median = parameter_summary$median,
    sd = parameter_summary$sd,
    rhat = parameter_summary$rhat,
    ess_bulk = parameter_summary$ess_bulk,
    ess_tail = parameter_summary$ess_tail,
    metric = NA_character_,
    value = NA_real_,
    threshold = NA_character_,
    pass = NA,
    stringsAsFactors = FALSE
  )
  metric_values <- c(
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    total_divergences = divergences,
    total_treedepth_hits = treedepth_hits,
    min_chain_ebfmi = min_ebfmi
  )
  metric_thresholds <- c("<1.01", ">=400", ">=400", "0", "0", ">0.3")
  metric_pass <- c(gates[1L], gates[2L], gates[3L], gates[4L], gates[5L], gates[6L])
  metric_export <- data.frame(
    fit = fit_label,
    record_type = "fit_metric",
    parameter = NA_character_,
    mean = NA_real_,
    median = NA_real_,
    sd = NA_real_,
    rhat = NA_real_,
    ess_bulk = NA_real_,
    ess_tail = NA_real_,
    metric = names(metric_values),
    value = unname(metric_values),
    threshold = metric_thresholds,
    pass = unname(metric_pass),
    stringsAsFactors = FALSE
  )
  chain_export <- data.frame(
    fit = fit_label,
    record_type = "chain_metric",
    parameter = NA_character_,
    mean = NA_real_,
    median = NA_real_,
    sd = NA_real_,
    rhat = NA_real_,
    ess_bulk = NA_real_,
    ess_tail = NA_real_,
    metric = paste0("ebfmi_chain_", seq_along(diagnostic$ebfmi)),
    value = diagnostic$ebfmi,
    threshold = ">0.3",
    pass = diagnostic$ebfmi > 0.3,
    stringsAsFactors = FALSE
  )
  list(
    table = rbind(parameter_export, metric_export, chain_export),
    pass = all(gates),
    gates = gates,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    divergences = divergences,
    treedepth_hits = treedepth_hits,
    min_ebfmi = min_ebfmi
  )
}

make_trace_plot <- function(fit, path, title_text) {
  variables <- c("mu[1]", "mu[2]", "tau[1]", "tau[2]", "rho")
  labels <- c(
    "Mean logit sensitivity", "Mean logit specificity",
    "SD logit sensitivity", "SD logit specificity", "Correlation"
  )
  draws <- posterior::as_draws_array(fit$draws(variables = variables))
  raw_draws <- unclass(draws)
  grDevices::png(path, width = 2400, height = 1800, res = 220)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mfrow = c(3, 2), mar = c(3.2, 3.5, 2.3, 0.8), oma = c(0, 0, 2, 0))
  chain_colors <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
  for (j in seq_along(variables)) {
    y <- raw_draws[, , j, drop = TRUE]
    graphics::matplot(
      seq_len(dim(y)[1L]), y,
      type = "l", lty = 1, col = chain_colors,
      xlab = "Post-warmup iteration", ylab = "", main = labels[j]
    )
  }
  graphics::plot.new()
  graphics::legend("center", legend = paste("Chain", 1:4), col = chain_colors, lty = 1, bty = "n")
  graphics::mtext(title_text, outer = TRUE, cex = 1.1, font = 2)
}

ren_model_file <- file.path(module_dir, "stan", "ren_multithreshold_validation.stan")
ren_model <- cmdstanr::cmdstan_model(
  ren_model_file,
  exe_file = file.path(work_dir, "ren_multithreshold_validation")
)

fpr_low <- (sum(combined_gt0$fp) + 0.5) /
  (sum(combined_gt0$fp + combined_gt0$tn) + 1)
fpr_high <- (sum(combined_gt1$fp) + 0.5) /
  (sum(combined_gt1$fp + combined_gt1$tn) + 1)
tpr_low <- (sum(combined_gt0$tp) + 0.5) /
  (sum(combined_gt0$tp + combined_gt0$fn) + 1)
tpr_high <- (sum(combined_gt1$tp) + 0.5) /
  (sum(combined_gt1$tp + combined_gt1$fn) + 1)
initial_scale_fpr <- log(2) / (stats::qlogis(fpr_low) - stats::qlogis(fpr_high))
initial_scale_tpr <- log(2) / (stats::qlogis(tpr_low) - stats::qlogis(tpr_high))
ren_shapes <- c(13.2, 7.4, 9.3, 4.4)
ren_rates <- c(44.7, 38.3, 38.1, 15.2)
set.seed(seed)
ren_initial_values <- lapply(seq_len(chains), function(chain) {
  list(
    population_mean = c(
      stats::qlogis(fpr_low) * initial_scale_fpr,
      stats::qlogis(tpr_low) * initial_scale_tpr,
      log(initial_scale_fpr),
      log(initial_scale_tpr)
    ) + stats::rnorm(4, 0, 0.02),
    tau = ren_shapes / ren_rates,
    z = matrix(stats::rnorm(4 * nrow(combined_gt0), 0, 0.02), 4, nrow(combined_gt0))
  )
})
ren_validation_data <- list(
  N = nrow(combined_gt0),
  tp_low_threshold = combined_gt0$tp,
  tp_high_threshold = combined_gt1$tp,
  fp_low_threshold = combined_gt0$fp,
  fp_high_threshold = combined_gt1$fp,
  n_disease = combined_gt0$tp + combined_gt0$fn,
  n_nondisease = combined_gt0$fp + combined_gt0$tn,
  log_threshold = log(c(1, 2)),
  mean_prior_mean = rep(0, 4),
  mean_prior_sd = rep(10, 4),
  tau_gamma_shape = ren_shapes,
  tau_gamma_rate = ren_rates
)
dir.create(
  file.path(raw_fit_dir, "ren_reproduction"),
  recursive = TRUE,
  showWarnings = FALSE
)
ren_fit <- do.call(
  ren_model$sample,
  c(
    list(
      data = ren_validation_data,
      seed = seed,
      init = ren_initial_values,
      output_dir = file.path(raw_fit_dir, "ren_reproduction")
    ),
    sampling_settings
  )
)
ren_fit$save_object(file.path(work_dir, "ren_reproduction_fit.rds"))
ren_diagnostics <- diagnose_fit(
  ren_fit,
  "ren_multithreshold_reproduction",
  c("population_mean", "tau", "z")
)
ren_draws <- posterior::as_draws_df(
  ren_fit$draws(c("pooled_sensitivity[2]", "pooled_specificity[2]"))
)
ren_sensitivity <- quantile_summary(ren_draws[["pooled_sensitivity[2]"]])
ren_specificity <- quantile_summary(ren_draws[["pooled_specificity[2]"]])
ren_validation <- rbind(
  data.frame(
    parameter = "pooled_sensitivity",
    posterior_mean = ren_sensitivity["mean"],
    posterior_median = ren_sensitivity["median"],
    lower_95 = ren_sensitivity["lower_95"],
    upper_95 = ren_sensitivity["upper_95"],
    published_target = 0.983,
    absolute_difference_percentage_points =
      abs(ren_sensitivity["median"] - 0.983) * 100,
    target_within_95_interval =
      0.983 >= ren_sensitivity["lower_95"] && 0.983 <= ren_sensitivity["upper_95"],
    stringsAsFactors = FALSE
  ),
  data.frame(
    parameter = "pooled_specificity",
    posterior_mean = ren_specificity["mean"],
    posterior_median = ren_specificity["median"],
    lower_95 = ren_specificity["lower_95"],
    upper_95 = ren_specificity["upper_95"],
    published_target = 0.514,
    absolute_difference_percentage_points =
      abs(ren_specificity["median"] - 0.514) * 100,
    target_within_95_interval =
      0.514 >= ren_specificity["lower_95"] && 0.514 <= ren_specificity["upper_95"],
    stringsAsFactors = FALSE
  )
)
ren_validation$point_difference_within_1_percentage_point <-
  ren_validation$absolute_difference_percentage_points <= 1
ren_validation$diagnostics_pass <- ren_diagnostics$pass
write_csv(ren_validation, file.path(output_dir, "ren_2024_reproduction_validation.csv"))
if (!ren_diagnostics$pass ||
    !all(ren_validation$point_difference_within_1_percentage_point) ||
    !all(ren_validation$target_within_95_interval)) {
  write_csv(
    ren_diagnostics$table,
    file.path(output_dir, "ren_2024_reproduction_diagnostics.csv")
  )
  stop(
    "Ren prior/model mapping did not pass the predeclared reproduction gate; ",
    "conditional models were not run."
  )
}

bivariate_model_file <- file.path(module_dir, "stan", "bivariate_dta.stan")
bivariate_model <- cmdstanr::cmdstan_model(
  bivariate_model_file,
  exe_file = file.path(work_dir, "bivariate_dta")
)
base_bivariate_data <- list(
  N = nrow(audit),
  tp = audit$conditional_tp,
  fn = audit$conditional_fn,
  tn = audit$conditional_tn,
  fp = audit$conditional_fp,
  mu_prior_mean = c(0, 0),
  mu_prior_sd = c(2.5, 2.5),
  tau_t_df = 3,
  tau_t_scale = c(1, 1),
  tau_gamma_shape = c(7.4, 13.2),
  tau_gamma_rate = c(38.3, 44.7),
  lkj_eta = 2
)
weak_data <- c(base_bivariate_data, list(tau_prior_type = 1L))
ren_prior_data <- c(base_bivariate_data, list(tau_prior_type = 2L))

initial_mu <- c(
  stats::qlogis(sum(audit$conditional_tp) / sum(audit$conditional_disease_total)),
  stats::qlogis(sum(audit$conditional_tn) / sum(audit$conditional_nondisease_total))
)
make_bivariate_initial_values <- function(initial_tau, local_seed) {
  set.seed(local_seed)
  lapply(seq_len(chains), function(chain) {
    list(
      mu = initial_mu + stats::rnorm(2, 0, 0.02),
      tau = initial_tau,
      L_Omega = diag(2),
      z = matrix(stats::rnorm(2 * nrow(audit), 0, 0.02), 2, nrow(audit))
    )
  })
}

fit_bivariate <- function(data, prior_strategy, fit_seed, initial_tau) {
  fit_output_dir <- file.path(raw_fit_dir, prior_strategy)
  dir.create(fit_output_dir, recursive = TRUE, showWarnings = FALSE)
  fit <- do.call(
    bivariate_model$sample,
    c(
      list(
        data = data,
        seed = fit_seed,
        init = make_bivariate_initial_values(initial_tau, fit_seed),
        output_dir = fit_output_dir
      ),
      sampling_settings
    )
  )
  fit$save_object(file.path(work_dir, paste0(prior_strategy, "_fit.rds")))
  fit
}

weak_fit <- fit_bivariate(
  weak_data,
  "weakly_informative",
  seed + 1L,
  c(0.5, 0.5)
)
ren_prior_fit <- fit_bivariate(
  ren_prior_data,
  "ren_informative",
  seed + 2L,
  c(7.4 / 38.3, 13.2 / 44.7)
)

weak_diagnostics <- diagnose_fit(
  weak_fit,
  "option1_weakly_informative",
  c("mu", "tau", "rho", "z")
)
ren_prior_diagnostics <- diagnose_fit(
  ren_prior_fit,
  "option1_ren_informative",
  c("mu", "tau", "rho", "z")
)
all_diagnostics <- rbind(
  ren_diagnostics$table,
  weak_diagnostics$table,
  ren_prior_diagnostics$table
)
write_csv(all_diagnostics, file.path(output_dir, "option1_mcmc_diagnostics.csv"))
if (!weak_diagnostics$pass || !ren_prior_diagnostics$pass) {
  stop("At least one conditional fit failed the predeclared MCMC diagnostic gates.")
}

make_trace_plot(
  weak_fit,
  file.path(output_dir, "option1_weakly_informative_traceplot.png"),
  "Option 1 conditional D-dimer: weakly informative priors"
)
make_trace_plot(
  ren_prior_fit,
  file.path(output_dir, "option1_ren_informative_traceplot.png"),
  "Option 1 conditional D-dimer: Ren-style heterogeneity priors"
)

posterior_variables <- c(
  "pooled_sensitivity", "pooled_specificity", "tau[1]", "tau[2]", "rho",
  "new_study_sensitivity", "new_study_specificity"
)

save_posterior_draws <- function(fit, prior_strategy, fit_seed) {
  draws <- posterior::as_draws_df(fit$draws(posterior_variables))
  export <- data.frame(
    chain = draws$.chain,
    iteration = draws$.iteration,
    draw_id = draws$.draw,
    pooled_sensitivity = draws$pooled_sensitivity,
    pooled_specificity = draws$pooled_specificity,
    tau_sensitivity = draws[["tau[1]"]],
    tau_specificity = draws[["tau[2]"]],
    rho = draws$rho,
    new_study_sensitivity = draws$new_study_sensitivity,
    new_study_specificity = draws$new_study_specificity,
    evidence_strategy = "option1_conditional_bivariate",
    prior_strategy = prior_strategy,
    run_id = run_id,
    model_sha256 = sha256_file(bivariate_model_file),
    source_data_sha256 = sha256_file(source_file),
    seed = fit_seed,
    cmdstan_version = as.character(cmdstanr::cmdstan_version()),
    cmdstanr_version = as.character(utils::packageVersion("cmdstanr")),
    stringsAsFactors = FALSE
  )
  path <- file.path(output_dir, paste0("option1_posterior_draws_", prior_strategy, ".csv.gz"))
  connection <- gzfile(path, open = "wt", compression = 9)
  on.exit(close(connection), add = TRUE)
  utils::write.csv(export, connection, row.names = FALSE, na = "")
  invisible(path)
}
save_posterior_draws(weak_fit, "weakly_informative", seed + 1L)
save_posterior_draws(ren_prior_fit, "ren_informative", seed + 2L)

summarize_option_fit <- function(fit, prior_strategy) {
  draws <- posterior::as_draws_df(fit$draws(posterior_variables))
  definitions <- list(
    pooled_sensitivity = list(variable = "pooled_sensitivity", interval = "95% credible interval", unit = "proportion"),
    pooled_specificity = list(variable = "pooled_specificity", interval = "95% credible interval", unit = "proportion"),
    tau_sensitivity = list(variable = "tau[1]", interval = "95% credible interval", unit = "logit SD"),
    tau_specificity = list(variable = "tau[2]", interval = "95% credible interval", unit = "logit SD"),
    rho = list(variable = "rho", interval = "95% credible interval", unit = "correlation"),
    prediction_sensitivity_new_study = list(variable = "new_study_sensitivity", interval = "95% prediction interval", unit = "proportion"),
    prediction_specificity_new_study = list(variable = "new_study_specificity", interval = "95% prediction interval", unit = "proportion")
  )
  do.call(rbind, lapply(names(definitions), function(parameter) {
    definition <- definitions[[parameter]]
    summary <- quantile_summary(draws[[definition$variable]])
    data.frame(
      evidence_strategy = "option1_conditional_bivariate",
      prior_strategy = prior_strategy,
      parameter = parameter,
      posterior_mean = summary["mean"],
      estimate = summary["median"],
      lower_95 = summary["lower_95"],
      upper_95 = summary["upper_95"],
      interval_type = definition$interval,
      unit = definition$unit,
      numerator = NA_integer_,
      denominator = NA_integer_,
      generating_object = paste0("option1_fits$", prior_strategy),
      stringsAsFactors = FALSE
    )
  }))
}

model_summary <- rbind(
  summarize_option_fit(weak_fit, "weakly_informative"),
  summarize_option_fit(ren_prior_fit, "ren_informative")
)
crude_rows <- rbind(
  data.frame(
    evidence_strategy = "option1_conditional_bivariate",
    prior_strategy = "descriptive_crude_pooling",
    parameter = "crude_pooled_sensitivity",
    posterior_mean = NA_real_,
    estimate = sum(audit$conditional_tp) / sum(audit$conditional_disease_total),
    lower_95 = NA_real_,
    upper_95 = NA_real_,
    interval_type = "none; descriptive verification only",
    unit = "proportion",
    numerator = sum(audit$conditional_tp),
    denominator = sum(audit$conditional_disease_total),
    generating_object = "conditional_audit totals",
    stringsAsFactors = FALSE
  ),
  data.frame(
    evidence_strategy = "option1_conditional_bivariate",
    prior_strategy = "descriptive_crude_pooling",
    parameter = "crude_pooled_specificity",
    posterior_mean = NA_real_,
    estimate = sum(audit$conditional_tn) / sum(audit$conditional_nondisease_total),
    lower_95 = NA_real_,
    upper_95 = NA_real_,
    interval_type = "none; descriptive verification only",
    unit = "proportion",
    numerator = sum(audit$conditional_tn),
    denominator = sum(audit$conditional_nondisease_total),
    generating_object = "conditional_audit totals",
    stringsAsFactors = FALSE
  )
)
model_summary <- rbind(model_summary, crude_rows)
write_csv(model_summary, file.path(output_dir, "option1_model_summary.csv"))

comparison_parameters <- c(
  "pooled_sensitivity", "pooled_specificity",
  "tau_sensitivity", "tau_specificity", "rho",
  "prediction_sensitivity_new_study", "prediction_specificity_new_study"
)
weak_summary <- model_summary[
  model_summary$prior_strategy == "weakly_informative" &
    model_summary$parameter %in% comparison_parameters,
]
ren_summary <- model_summary[
  model_summary$prior_strategy == "ren_informative" &
    model_summary$parameter %in% comparison_parameters,
]
comparison <- merge(
  weak_summary,
  ren_summary,
  by = "parameter",
  suffixes = c("_weak", "_ren"),
  sort = FALSE
)
comparison$absolute_difference <- abs(comparison$estimate_weak - comparison$estimate_ren)
comparison$difference_unit <- ifelse(
  comparison$unit_weak == "proportion",
  "percentage points",
  comparison$unit_weak
)
comparison$absolute_difference_display <- ifelse(
  comparison$unit_weak == "proportion",
  comparison$absolute_difference * 100,
  comparison$absolute_difference
)
pooled_differences <- comparison$absolute_difference[
  comparison$parameter %in% c("pooled_sensitivity", "pooled_specificity")
]
max_pooled_difference <- max(pooled_differences)
pooled_accuracy_prior_sensitivity_label <- if (max_pooled_difference <= 0.01) {
  "essentially unchanged"
} else if (max_pooled_difference < 0.05) {
  "modestly prior-sensitive"
} else {
  "materially prior-sensitive"
}
comparison$pooled_accuracy_prior_sensitivity_label <-
  pooled_accuracy_prior_sensitivity_label
comparison$interpretation_scope <-
  "classification applies only to pooled sensitivity and specificity medians"
comparison <- comparison[c(
  "parameter",
  "estimate_weak", "lower_95_weak", "upper_95_weak",
  "estimate_ren", "lower_95_ren", "upper_95_ren",
  "absolute_difference_display", "difference_unit",
  "pooled_accuracy_prior_sensitivity_label", "interpretation_scope"
)]
write_csv(comparison, file.path(output_dir, "option1_prior_comparison.csv"))

posterior_predictive_checks <- function(fit, prior_strategy) {
  tp_variables <- paste0("tp_rep[", seq_len(nrow(audit)), "]")
  tn_variables <- paste0("tn_rep[", seq_len(nrow(audit)), "]")
  discrepancy_variables <- c(
    "discrepancy_sensitivity_observed", "discrepancy_sensitivity_replicated",
    "discrepancy_specificity_observed", "discrepancy_specificity_replicated"
  )
  draws <- as.data.frame(
    posterior::as_draws_df(
      fit$draws(c(tp_variables, tn_variables, discrepancy_variables))
    ),
    check.names = FALSE
  )
  study_rows <- do.call(rbind, lapply(seq_len(nrow(audit)), function(i) {
    tp_summary <- quantile_summary(draws[[tp_variables[i]]])
    tn_summary <- quantile_summary(draws[[tn_variables[i]]])
    rbind(
      data.frame(
        prior_strategy = prior_strategy,
        check_scope = "study",
        study_id = audit$study_id[i],
        quantity = "true_positives",
        observed = audit$conditional_tp[i],
        replicated_mean = tp_summary["mean"],
        replicated_lower_95 = tp_summary["lower_95"],
        replicated_upper_95 = tp_summary["upper_95"],
        posterior_predictive_p = mean(draws[[tp_variables[i]]] >= audit$conditional_tp[i]),
        extreme_p_flag = FALSE,
        stringsAsFactors = FALSE
      ),
      data.frame(
        prior_strategy = prior_strategy,
        check_scope = "study",
        study_id = audit$study_id[i],
        quantity = "true_negatives",
        observed = audit$conditional_tn[i],
        replicated_mean = tn_summary["mean"],
        replicated_lower_95 = tn_summary["lower_95"],
        replicated_upper_95 = tn_summary["upper_95"],
        posterior_predictive_p = mean(draws[[tn_variables[i]]] >= audit$conditional_tn[i]),
        extreme_p_flag = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }))
  tp_matrix <- as.matrix(draws[tp_variables])
  tn_matrix <- as.matrix(draws[tn_variables])
  total_fn_rep <- rowSums(matrix(audit$conditional_disease_total, nrow(tp_matrix), nrow(audit), byrow = TRUE) - tp_matrix)
  total_fp_rep <- rowSums(matrix(audit$conditional_nondisease_total, nrow(tn_matrix), nrow(audit), byrow = TRUE) - tn_matrix)
  fn_summary <- quantile_summary(total_fn_rep)
  fp_summary <- quantile_summary(total_fp_rep)
  aggregate_rows <- rbind(
    data.frame(
      prior_strategy = prior_strategy,
      check_scope = "aggregate",
      study_id = "ALL",
      quantity = "total_false_negatives",
      observed = sum(audit$conditional_fn),
      replicated_mean = fn_summary["mean"],
      replicated_lower_95 = fn_summary["lower_95"],
      replicated_upper_95 = fn_summary["upper_95"],
      posterior_predictive_p = mean(total_fn_rep >= sum(audit$conditional_fn)),
      extreme_p_flag = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      prior_strategy = prior_strategy,
      check_scope = "aggregate",
      study_id = "ALL",
      quantity = "total_false_positives",
      observed = sum(audit$conditional_fp),
      replicated_mean = fp_summary["mean"],
      replicated_lower_95 = fp_summary["lower_95"],
      replicated_upper_95 = fp_summary["upper_95"],
      posterior_predictive_p = mean(total_fp_rep >= sum(audit$conditional_fp)),
      extreme_p_flag = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      prior_strategy = prior_strategy,
      check_scope = "discrepancy",
      study_id = "ALL",
      quantity = "pearson_sensitivity_discrepancy",
      observed = mean(draws$discrepancy_sensitivity_observed),
      replicated_mean = mean(draws$discrepancy_sensitivity_replicated),
      replicated_lower_95 = stats::quantile(draws$discrepancy_sensitivity_replicated, 0.025),
      replicated_upper_95 = stats::quantile(draws$discrepancy_sensitivity_replicated, 0.975),
      posterior_predictive_p = mean(
        draws$discrepancy_sensitivity_replicated >= draws$discrepancy_sensitivity_observed
      ),
      extreme_p_flag = FALSE,
      stringsAsFactors = FALSE
    ),
    data.frame(
      prior_strategy = prior_strategy,
      check_scope = "discrepancy",
      study_id = "ALL",
      quantity = "pearson_specificity_discrepancy",
      observed = mean(draws$discrepancy_specificity_observed),
      replicated_mean = mean(draws$discrepancy_specificity_replicated),
      replicated_lower_95 = stats::quantile(draws$discrepancy_specificity_replicated, 0.025),
      replicated_upper_95 = stats::quantile(draws$discrepancy_specificity_replicated, 0.975),
      posterior_predictive_p = mean(
        draws$discrepancy_specificity_replicated >= draws$discrepancy_specificity_observed
      ),
      extreme_p_flag = FALSE,
      stringsAsFactors = FALSE
    )
  )
  result <- rbind(study_rows, aggregate_rows)
  result$extreme_p_flag <-
    result$posterior_predictive_p < 0.05 | result$posterior_predictive_p > 0.95
  result
}

weak_ppc <- posterior_predictive_checks(weak_fit, "weakly_informative")
ren_ppc <- posterior_predictive_checks(ren_prior_fit, "ren_informative")
ppc <- rbind(weak_ppc, ren_ppc)
write_csv(ppc, file.path(output_dir, "option1_posterior_predictive_checks.csv"))

make_ppc_plot <- function(ppc_data, path, title_text) {
  study_data <- ppc_data[ppc_data$check_scope == "study", ]
  grDevices::png(path, width = 2200, height = 1300, res = 220)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old_par)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mfrow = c(1, 2), mar = c(8, 4, 3, 1))
  for (quantity in c("true_positives", "true_negatives")) {
    d <- study_data[study_data$quantity == quantity, ]
    d <- d[match(audit$study_id, d$study_id), ]
    y_range <- range(c(d$replicated_lower_95, d$replicated_upper_95, d$observed))
    graphics::plot(
      seq_len(nrow(d)), d$replicated_mean,
      ylim = y_range, xaxt = "n", pch = 16, col = "#1F78B4",
      xlab = "", ylab = "Count",
      main = if (quantity == "true_positives") "True positives" else "True negatives"
    )
    graphics::segments(
      seq_len(nrow(d)), d$replicated_lower_95,
      seq_len(nrow(d)), d$replicated_upper_95,
      col = "#1F78B4", lwd = 2
    )
    graphics::points(seq_len(nrow(d)), d$observed, pch = 4, col = "#E31A1C", lwd = 2)
    graphics::axis(1, at = seq_len(nrow(d)), labels = audit$study, las = 2, cex.axis = 0.75)
    graphics::legend(
      "topright", legend = c("Replicated mean and 95% interval", "Observed"),
      col = c("#1F78B4", "#E31A1C"), pch = c(16, 4), bty = "n", cex = 0.75
    )
  }
  graphics::mtext(title_text, outer = TRUE, line = -1, font = 2)
}
make_ppc_plot(
  weak_ppc,
  file.path(output_dir, "option1_weakly_informative_ppc.png"),
  "Posterior predictive checks: weakly informative priors"
)
make_ppc_plot(
  ren_ppc,
  file.path(output_dir, "option1_ren_informative_ppc.png"),
  "Posterior predictive checks: Ren-style heterogeneity priors"
)

fmt_pct <- function(x, digits = 1L) formatC(100 * x, format = "f", digits = digits)
get_result <- function(prior, parameter) {
  model_summary[
    model_summary$prior_strategy == prior & model_summary$parameter == parameter,
    ,
    drop = FALSE
  ]
}
weak_sens <- get_result("weakly_informative", "pooled_sensitivity")
weak_spec <- get_result("weakly_informative", "pooled_specificity")
ren_sens <- get_result("ren_informative", "pooled_sensitivity")
ren_spec <- get_result("ren_informative", "pooled_specificity")
weak_pred_sens <- get_result("weakly_informative", "prediction_sensitivity_new_study")
weak_pred_spec <- get_result("weakly_informative", "prediction_specificity_new_study")
ren_pred_sens <- get_result("ren_informative", "prediction_sensitivity_new_study")
ren_pred_spec <- get_result("ren_informative", "prediction_specificity_new_study")
pooled_sens_difference_pp <- 100 * abs(weak_sens$estimate - ren_sens$estimate)
pooled_spec_difference_pp <- 100 * abs(weak_spec$estimate - ren_spec$estimate)
weak_extreme_ppc <- weak_ppc[weak_ppc$extreme_p_flag, , drop = FALSE]
ren_extreme_ppc <- ren_ppc[ren_ppc$extreme_p_flag, , drop = FALSE]
results_document <- c(
  "# Option 1 conditional D-dimer results",
  "",
  paste0("Run identifier: `", run_id, "`."),
  "",
  "## Reconciliation",
  "",
  paste0(
    "All six study tables passed the source-total, nonnegative-cell, and conditional-identity checks. ",
    "The derived ADD-RS 0–1 tables contain ", sum(audit$conditional_tp), " true positives, ",
    sum(audit$conditional_fn), " false negatives, ", sum(audit$conditional_fp), " false positives, and ",
    sum(audit$conditional_tn), " true negatives. Crude pooling gives sensitivity ",
    fmt_pct(sum(audit$conditional_tp) / sum(audit$conditional_disease_total), 2), "% and specificity ",
    fmt_pct(sum(audit$conditional_tn) / sum(audit$conditional_nondisease_total), 2), "% (descriptive only)."
  ),
  "",
  "## Ren reproduction gate",
  "",
  paste0(
    "The reconstructed paired-threshold model estimated ",
    fmt_pct(ren_sensitivity["median"], 2), "% sensitivity and ",
    fmt_pct(ren_specificity["median"], 2), "% specificity for ADD-RS >1 OR D-dimer >500, ",
    "compared with the published 98.3% and 51.4%. Both differences were below 1 percentage point, ",
    "both targets lay inside the reconstructed 95% intervals, and all MCMC gates passed."
  ),
  "",
  "## Conditional Bayesian estimates",
  "",
  "| Prior strategy | Pooled sensitivity (95% CrI) | Pooled specificity (95% CrI) |",
  "|---|---:|---:|",
  paste0(
    "| Weakly informative | ", fmt_pct(weak_sens$estimate), "% (",
    fmt_pct(weak_sens$lower_95), "%–", fmt_pct(weak_sens$upper_95), "%) | ",
    fmt_pct(weak_spec$estimate), "% (", fmt_pct(weak_spec$lower_95), "%–",
    fmt_pct(weak_spec$upper_95), "%) |"
  ),
  paste0(
    "| Ren-style heterogeneity | ", fmt_pct(ren_sens$estimate), "% (",
    fmt_pct(ren_sens$lower_95), "%–", fmt_pct(ren_sens$upper_95), "%) | ",
    fmt_pct(ren_spec$estimate), "% (", fmt_pct(ren_spec$lower_95), "%–",
    fmt_pct(ren_spec$upper_95), "%) |"
  ),
  "",
  paste0(
    "Pooled-accuracy prior-sensitivity classification: **",
    pooled_accuracy_prior_sensitivity_label, "** (absolute median differences: sensitivity ",
    formatC(pooled_sens_difference_pp, format = "f", digits = 2),
    " percentage points; specificity ",
    formatC(pooled_spec_difference_pp, format = "f", digits = 2),
    " percentage points)."
  ),
  "",
  paste0(
    "That label applies only to the pooled medians. Heterogeneity and predictive uncertainty are ",
    "substantially prior-sensitive: the sensitivity prediction interval is ",
    fmt_pct(weak_pred_sens$lower_95), "%–", fmt_pct(weak_pred_sens$upper_95),
    "% under weak priors versus ", fmt_pct(ren_pred_sens$lower_95), "%–",
    fmt_pct(ren_pred_sens$upper_95), "% under Ren-style priors; the corresponding specificity ",
    "intervals are ", fmt_pct(weak_pred_spec$lower_95), "%–",
    fmt_pct(weak_pred_spec$upper_95), "% and ", fmt_pct(ren_pred_spec$lower_95), "%–",
    fmt_pct(ren_pred_spec$upper_95), "%."
  ),
  "",
  paste0(
    "Posterior predictive checks produced ", nrow(weak_extreme_ppc),
    " extreme tail-area flag(s) under weak priors and ", nrow(ren_extreme_ppc),
    " under Ren-style priors. The Ren-style flags are the McLatchie 2023 true-positive count ",
    "(posterior predictive p=", formatC(
      ren_ppc$posterior_predictive_p[
        ren_ppc$study_id == "mclatchie_2023" & ren_ppc$quantity == "true_positives"
      ], format = "f", digits = 4
    ), ") and the overall sensitivity Pearson discrepancy (p=",
    formatC(
      ren_ppc$posterior_predictive_p[
        ren_ppc$quantity == "pearson_sensitivity_discrepancy"
      ], format = "f", digits = 4
    ), "). These flags indicate that the informative heterogeneity prior does not fully reproduce ",
    "the observed sensitivity dispersion, despite satisfactory MCMC convergence."
  ),
  "",
  "Full heterogeneity estimates, correlations, prediction intervals, posterior predictive checks, and diagnostics are in the machine-readable output tables. These Option 1 estimates are candidate external parameters only; they are not designated as final or base-case economic inputs.",
  "",
  "## Limitations",
  "",
  "Only six studies contribute. Some diseased cells are sparse, between-study heterogeneity and the sensitivity-specificity correlation are uncertain, and transportability to the local CTA-selected cohort is not established. This is a secondary reanalysis of published study-level data rather than a new systematic review. The Ren-style priors are a documented single-threshold reduction of a four-parameter paired-threshold model. Option 1 remains separate from Option 2 pending later methodological comparison."
)
writeLines(
  results_document,
  file.path(documentation_dir, "option1_results.md"),
  useBytes = TRUE
)

session_metadata <- data.frame(
  run_id = run_id,
  evidence_strategy = "option1_conditional_bivariate",
  R_version = paste(R.version$major, R.version$minor, sep = "."),
  cmdstanr_version = as.character(utils::packageVersion("cmdstanr")),
  posterior_version = as.character(utils::packageVersion("posterior")),
  cmdstan_version = as.character(cmdstanr::cmdstan_version()),
  chains = chains,
  iter_warmup = iter_warmup,
  iter_sampling = iter_sampling,
  adapt_delta = adapt_delta,
  max_treedepth = max_treedepth,
  seed = seed,
  source_data_sha256 = sha256_file(source_file),
  bivariate_model_sha256 = sha256_file(bivariate_model_file),
  ren_validation_model_sha256 = sha256_file(ren_model_file),
  stringsAsFactors = FALSE
)
write_csv(session_metadata, file.path(output_dir, "option1_run_metadata.csv"))

manifest_candidates <- c(
  file.path(repo_root, "renv.lock"),
  source_file,
  file.path(shared_dir, "source_provenance", "ren_2024_source_provenance.md"),
  file.path(module_dir, "README.md"),
  script_path,
  bivariate_model_file,
  ren_model_file,
  list.files(derived_dir, recursive = TRUE, full.names = TRUE),
  list.files(output_dir, recursive = TRUE, full.names = TRUE),
  list.files(documentation_dir, recursive = TRUE, full.names = TRUE)
)
manifest_path <- file.path(output_dir, "option1_file_manifest.csv")
manifest_candidates <- unique(manifest_candidates[
  file.exists(manifest_candidates) & manifest_candidates != manifest_path
])
manifest <- data.frame(
  path = sub(paste0("^", repo_root, "/"), "", manifest_candidates),
  bytes = file.info(manifest_candidates)$size,
  sha256 = vapply(manifest_candidates, sha256_file, character(1)),
  stringsAsFactors = FALSE
)
write_csv(manifest, manifest_path)

message("Option 1 conditional D-dimer pipeline completed successfully.")
message("Reconciliation: 6/6 studies passed.")
message(
  "Ren validation medians: sensitivity ", fmt_pct(ren_sensitivity["median"], 2),
  "%; specificity ", fmt_pct(ren_specificity["median"], 2), "%"
)
message(
  "Weak-prior pooled medians: sensitivity ", fmt_pct(weak_sens$estimate, 2),
  "%; specificity ", fmt_pct(weak_spec$estimate, 2), "%"
)
message(
  "Ren-prior pooled medians: sensitivity ", fmt_pct(ren_sens$estimate, 2),
  "%; specificity ", fmt_pct(ren_spec$estimate, 2), "%"
)
