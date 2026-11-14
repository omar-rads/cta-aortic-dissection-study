# Why this script is here
# I calculate modeled imaging, costs, and clinical consequences from supplied inputs.
# False negatives are reported separately and do not establish the safety of CTA deferral.

# =============================================================================
# economic_cost_consequence_engine.R
# Shared parameterized cost-consequence calculations for AAS CTA strategies
# =============================================================================

calculate_cost_consequence <- function(
  aas_positive,
  aas_negative,
  sensitivity,
  specificity,
  cta_unit_cost,
  d_dimer_unit_cost
) {

  # Check the cohort counts and costs before calculating either strategy.
  scalar_inputs <- c(
    aas_positive = aas_positive,
    aas_negative = aas_negative,
    cta_unit_cost = cta_unit_cost,
    d_dimer_unit_cost = d_dimer_unit_cost
  )

  if (
    length(aas_positive) != 1L ||
    length(aas_negative) != 1L ||
    length(cta_unit_cost) != 1L ||
    length(d_dimer_unit_cost) != 1L ||
    any(!is.finite(scalar_inputs)) ||
    any(scalar_inputs < 0)
  ) {
    stop(
      "AAS counts and unit costs must be nonnegative finite scalars.",
      call. = FALSE
    )
  }

  # Keep sensitivity and specificity paired and within the probability range.
  if (
    length(sensitivity) == 0L ||
    length(specificity) == 0L ||
    length(sensitivity) != length(specificity) ||
    any(!is.finite(sensitivity)) ||
    any(!is.finite(specificity)) ||
    any(sensitivity < 0 | sensitivity > 1) ||
    any(specificity < 0 | specificity > 1)
  ) {
    stop(
      "Sensitivity and specificity must be equal-length finite vectors in [0, 1].",
      call. = FALSE
    )
  }

  n <- aas_positive + aas_negative
  if (n <= 0) {
    stop("The modeled population must be positive.", call. = FALSE)
  }

  # Translate the supplied accuracy into expected test outcomes.
  expected_tp <- aas_positive * sensitivity
  expected_fn <- aas_positive * (1 - sensitivity)
  expected_tn <- aas_negative * specificity
  expected_fp <- aas_negative * (1 - specificity)

  expected_d_dimer_positive <- expected_tp + expected_fp
  expected_d_dimer_negative <- expected_tn + expected_fn

  # Compare testing expenditures under CTA for all and D-dimer first.
  cta_for_all_expenditure <- n * cta_unit_cost
  d_dimer_first_expenditure <-
    n * d_dimer_unit_cost + expected_d_dimer_positive * cta_unit_cost
  testing_expenditure_reduction <-
    cta_for_all_expenditure - d_dimer_first_expenditure
  testing_expenditure_reduction_percent <- if (cta_for_all_expenditure == 0) {
    rep(NA_real_, length(sensitivity))
  } else {
    100 * testing_expenditure_reduction / cta_for_all_expenditure
  }

  # Return false negatives separately from the testing expenditure comparison.
  data.frame(
    expected_tp = expected_tp,
    expected_fn = expected_fn,
    expected_tn = expected_tn,
    expected_fp = expected_fp,
    expected_d_dimer_positive = expected_d_dimer_positive,
    expected_d_dimer_negative = expected_d_dimer_negative,
    cta_for_all_cta_count = rep(n, length(sensitivity)),
    d_dimer_first_d_dimer_count = rep(n, length(sensitivity)),
    expected_cta_performed = expected_d_dimer_positive,
    expected_cta_deferred = expected_d_dimer_negative,
    expected_false_negative_aas = expected_fn,
    cta_deferred_percent = 100 * expected_d_dimer_negative / n,
    cta_unit_cost = rep(cta_unit_cost, length(sensitivity)),
    d_dimer_unit_cost = rep(d_dimer_unit_cost, length(sensitivity)),
    cta_for_all_expenditure = rep(
      cta_for_all_expenditure,
      length(sensitivity)
    ),
    d_dimer_first_expenditure = d_dimer_first_expenditure,
    testing_expenditure_reduction = testing_expenditure_reduction,
    testing_expenditure_reduction_percent =
      testing_expenditure_reduction_percent,
    false_negative_monetary_cost_included = rep(
      FALSE,
      length(sensitivity)
    ),
    stringsAsFactors = FALSE
  )
}
