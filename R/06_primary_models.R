# Why this script is here
# I fit the algorithm-discordance regression and examine clustering.
# The resulting estimates describe associations, not causal effects.

# 06_primary_models.R
# AAS CTA reanalysis, stage 7: algorithm-discordance regression and clustering checks.

source("R/05_descriptive_analysis.R")

# ---- Complete-case regression dataset -----------------------------------------
# I restrict the model to complete cases for the specified variables and check reference levels.

model_data <- analysis_data[
  analysis_data$algorithm_class %in% c("Concordant", "Discordant") &
    analysis_data$provider_group %in% c("Physician", "Non-physician"), , drop = FALSE
]

model_data$age_per_10_years <- model_data$age / 10
model_data$sex_model <- stats::relevel(factor(model_data$sex), ref = "Male")
model_data$esi_model <- stats::relevel(factor(model_data$esi_group), ref = "ESI 1-2")
model_data$provider_model <- stats::relevel(factor(model_data$provider_group), ref = "Physician")
model_data$priority_model <- stats::relevel(factor(model_data$order_priority), ref = "STAT")
model_data$site_model <- stats::relevel(factor(model_data$site), ref = "Hospital D")

model_variables <- c(
  "algorithm_discordant",
  "age_per_10_years",
  "sex_model",
  "esi_model",
  "provider_model",
  "priority_model",
  "site_model",
  "patient_empi",
  "primary_provider_id"
)

model_data <- model_data[stats::complete.cases(model_data[model_variables]), , drop = FALSE]

if (!all(levels(model_data$sex_model) == c("Male", "Female"))) {
  stop("Sex model reference coding is not Male then Female.", call. = FALSE)
}

if (levels(model_data$site_model)[1L] != "Hospital D") {
  stop("Site model reference is not Hospital D.", call. = FALSE)
}

# ---- Algorithm-discordance logistic regression --------------------------------
# I fit the specified association model and express estimates as odds ratios with Wald intervals.

primary_model <- stats::glm(
  algorithm_discordant ~
    age_per_10_years + sex_model + esi_model + provider_model + priority_model + site_model,
  family = stats::binomial(),
  data = model_data
)

# Odds ratios with Wald 95% CIs and two-sided z tests under the given covariance.
tidy_logistic_model <- function(model, covariance = stats::vcov(model)) {
  estimates <- stats::coef(model)
  standard_errors <- sqrt(diag(covariance))
  z_values <- estimates / standard_errors

  data.frame(
    term = names(estimates),
    coefficient = unname(estimates),
    standard_error = unname(standard_errors),
    odds_ratio = exp(unname(estimates)),
    ci_low = exp(unname(estimates) - 1.96 * unname(standard_errors)),
    ci_high = exp(unname(estimates) + 1.96 * unname(standard_errors)),
    p_value = 2 * stats::pnorm(-abs(unname(z_values))),
    stringsAsFactors = FALSE
  )
}

primary_model_estimates <- tidy_logistic_model(primary_model)

term_labels <- c(
  "(Intercept)" = "Intercept",
  "age_per_10_years" = "Age, per 10-year increase",
  "sex_modelFemale" = "Female vs male",
  "esi_modelESI 3-4" = "ESI 3-4 vs ESI 1-2",
  "provider_modelNon-physician" = "Non-physician vs physician",
  "priority_modelNon-STAT" = "Non-STAT vs STAT",
  "site_modelHospital A" = "Hospital A vs Hospital D",
  "site_modelHospital B" = "Hospital B vs Hospital D",
  "site_modelHospital C" = "Hospital C vs Hospital D"
)

primary_model_estimates$comparison <- unname(term_labels[primary_model_estimates$term])

if (anyNA(primary_model_estimates$comparison)) {
  stop("At least one primary-model term lacks a display label.", call. = FALSE)
}

table4_source <- primary_model_estimates[
  primary_model_estimates$term != "(Intercept)",
  c(
    "comparison",
    "term",
    "coefficient",
    "standard_error",
    "odds_ratio",
    "ci_low",
    "ci_high",
    "p_value"
  )
]

table4_source$analytic_n <- nrow(model_data)
table4_source$discordant_events <- sum(model_data$algorithm_discordant)
table4_source$concordant_events <- sum(!model_data$algorithm_discordant)
table4_source$covariance <- "Model-based"
table4_source$generating_object <- "table4_source"

# ---- Model diagnostics --------------------------------------------------------
# I summarize model fit and discrimination for the fitted data.

# C statistic from the rank-sum (Mann-Whitney) identity.
auc_rank <- function(outcome, probability) {
  n_positive <- sum(outcome == 1L)
  n_negative <- sum(outcome == 0L)
  ranks <- rank(probability, ties.method = "average")

  (sum(ranks[outcome == 1L]) - n_positive * (n_positive + 1) / 2) / (n_positive * n_negative)
}

primary_model_diagnostics <- data.frame(
  analytic_n = nrow(model_data),
  discordant_events = sum(model_data$algorithm_discordant),
  concordant_events = sum(!model_data$algorithm_discordant),
  aic = stats::AIC(primary_model),
  null_deviance = primary_model$null.deviance,
  residual_deviance = primary_model$deviance,
  degrees_freedom_residual = primary_model$df.residual,
  c_statistic = auc_rank(model_data$algorithm_discordant, stats::fitted(primary_model)),
  stringsAsFactors = FALSE
)

# ---- One-way cluster-robust covariance checks ---------------------------------
# I check uncertainty with patient and provider clustering separately, using the same fitted model.

# CR1 sandwich covariance with the usual G/(G-1) * (N-1)/(N-P) correction.
cluster_robust_vcov <- function(model, cluster) {
  design <- stats::model.matrix(model)
  residual_score <- stats::model.response(stats::model.frame(model)) - stats::fitted(model)
  score_rows <- design * residual_score

  cluster <- as.character(cluster)
  cluster_scores <- rowsum(score_rows, cluster, reorder = FALSE)

  bread <- stats::vcov(model)
  meat <- crossprod(cluster_scores)

  n <- nrow(design)
  p <- ncol(design)
  g <- nrow(cluster_scores)

  correction <- (g / (g - 1)) * ((n - 1) / (n - p))

  correction * bread %*% meat %*% bread
}

patient_cluster_vcov <- cluster_robust_vcov(primary_model, model_data$patient_empi)
provider_cluster_vcov <- cluster_robust_vcov(primary_model, model_data$primary_provider_id)

patient_cluster_estimates <- tidy_logistic_model(primary_model, patient_cluster_vcov)
patient_cluster_estimates$covariance <- "Patient-clustered CR1"

provider_cluster_estimates <- tidy_logistic_model(primary_model, provider_cluster_vcov)
provider_cluster_estimates$covariance <- "Provider-clustered CR1"

clustered_model_estimates <- rbind(patient_cluster_estimates, provider_cluster_estimates)
clustered_model_estimates$comparison <- unname(term_labels[clustered_model_estimates$term])

cluster_summary <- data.frame(
  cluster_type = c("Patient", "Provider", "Site"),
  n_clusters = c(
    length(unique(model_data$patient_empi)),
    length(unique(model_data$primary_provider_id)),
    length(unique(model_data$site))
  ),
  inference = c(
    "One-way CR1 covariance",
    "One-way CR1 covariance",
    "Fixed effects in primary model; four clusters are insufficient for reliable site-clustered SEs"
  ),
  stringsAsFactors = FALSE
)

# ---- Patient/provider random-intercept sensitivity model (needs lme4) ---------
# I attempt a mixed model when the required package is available and retain its run status.

mixed_model_status <- "Not run"
mixed_model_estimates <- data.frame()

if (requireNamespace("lme4", quietly = TRUE)) {
  mixed_model <- suppressWarnings(
    lme4::glmer(
      algorithm_discordant ~
        age_per_10_years + sex_model + esi_model + provider_model + priority_model + site_model +
        (1 | patient_empi) + (1 | primary_provider_id),
      family = stats::binomial(),
      data = model_data,
      control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 200000))
    )
  )

  mixed_coefficients <- summary(mixed_model)$coefficients

  mixed_model_estimates <- data.frame(
    term = rownames(mixed_coefficients),
    coefficient = mixed_coefficients[, "Estimate"],
    standard_error = mixed_coefficients[, "Std. Error"],
    odds_ratio = exp(mixed_coefficients[, "Estimate"]),
    ci_low = exp(mixed_coefficients[, "Estimate"] - 1.96 * mixed_coefficients[, "Std. Error"]),
    ci_high = exp(mixed_coefficients[, "Estimate"] + 1.96 * mixed_coefficients[, "Std. Error"]),
    p_value = mixed_coefficients[, "Pr(>|z|)"],
    comparison = unname(term_labels[rownames(mixed_coefficients)]),
    covariance = "Patient/provider random intercepts",
    stringsAsFactors = FALSE
  )

  random_effect_variance <- as.data.frame(lme4::VarCorr(mixed_model))
  patient_variance <- random_effect_variance$vcov[
    random_effect_variance$grp == "patient_empi"
  ][1L]

  mixed_model_status <- if (lme4::isSingular(mixed_model, tol = 1e-4)) {
    "Singular fit"
  } else if (!is.na(patient_variance) && patient_variance > 100) {
    paste0(
      "Numerically converged but unstable: patient random-effect variance ",
      "is extreme because most patients contribute one encounter"
    )
  } else {
    "Converged, nonsingular"
  }
}

# ---- Validate and save --------------------------------------------------------
# I check the model table and save the estimates, diagnostics, and sensitivity outputs.

if (nrow(table4_source) != 8L) {
  stop("Table 4 does not contain the eight prespecified comparisons.", call. = FALSE)
}

if (
  sum(model_data$algorithm_discordant) +
    sum(!model_data$algorithm_discordant) != nrow(model_data)
) {
  stop("Primary-model outcome totals do not reconcile.", call. = FALSE)
}

primary_model_results_file <- file.path(aas_derived_dir, "aas_primary_model_results.rds")

saveRDS(
  list(
    table4_source = table4_source,
    primary_model_diagnostics = primary_model_diagnostics,
    clustered_model_estimates = clustered_model_estimates,
    cluster_summary = cluster_summary,
    mixed_model_estimates = mixed_model_estimates,
    mixed_model_status = mixed_model_status,
    random_effect_variance = if (exists("random_effect_variance")) {
      random_effect_variance
    } else {
      data.frame()
    }
  ),
  primary_model_results_file
)

message("\nPrimary logistic-regression estimates:")
print(table4_source, row.names = FALSE)

message("\nPrimary model diagnostics:")
print(primary_model_diagnostics, row.names = FALSE)

message("\nCluster counts and inference approach:")
print(cluster_summary, row.names = FALSE)

message("\nMixed-model status: ", mixed_model_status)

if (exists("random_effect_variance")) {
  message("Random-intercept variance estimates:")
  print(random_effect_variance[, c("grp", "vcov", "sdcor")], row.names = FALSE)
}

message("\nPrimary model result objects written outside Git:")
message(primary_model_results_file)
