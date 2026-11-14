# Synthetic checks of the shared economic engine. No study data are read.
source("R/economic_cost_consequence_engine.R")

calculate <- function(sensitivity, specificity, aas_positive = 2,
                      aas_negative = 8, cta_unit_cost = 100,
                      d_dimer_unit_cost = 10) {
  calculate_cost_consequence(
    aas_positive, aas_negative, sensitivity, specificity,
    cta_unit_cost, d_dimer_unit_cost
  )
}

expect_error <- function(expr) {
  failed <- tryCatch({ force(expr); FALSE }, error = function(e) TRUE)
  stopifnot(failed)
}

# In ten hypothetical patients, the first strategy produces 5.5 positive
# D-dimers and 4.5 deferred CTAs. Testing costs $650 versus $1,000 for CTA
# in everyone. The second paired estimate represents perfect accuracy.
result <- calculate(c(0.75, 1), c(0.5, 1))
expected <- list(
  expected_tp = c(1.5, 2), expected_fn = c(0.5, 0),
  expected_tn = c(4, 8), expected_fp = c(4, 0),
  expected_d_dimer_positive = c(5.5, 2),
  expected_d_dimer_negative = c(4.5, 8),
  expected_cta_performed = c(5.5, 2), expected_cta_deferred = c(4.5, 8),
  expected_false_negative_aas = c(0.5, 0),
  cta_for_all_expenditure = c(1000, 1000),
  d_dimer_first_expenditure = c(650, 300),
  testing_expenditure_reduction = c(350, 700),
  testing_expenditure_reduction_percent = c(35, 70)
)
stopifnot(nrow(result) == 2L)
for (field in names(expected)) {
  stopifnot(isTRUE(all.equal(result[[field]], expected[[field]])))
}
stopifnot(all(!result$false_negative_monetary_cost_included))

# Zero and one are valid accuracy boundaries. A zero CTA cost makes the
# percentage reduction undefined, while the dollar calculation remains valid.
boundary <- calculate(0, 0, cta_unit_cost = 0)
stopifnot(boundary$expected_false_negative_aas == 2,
          boundary$expected_cta_deferred == 2,
          boundary$testing_expenditure_reduction == -100,
          is.na(boundary$testing_expenditure_reduction_percent))

expect_error(calculate(-0.01, 0.5))
expect_error(calculate(0.5, 1.01))
expect_error(calculate(c(0.5, 0.75), 0.5))
expect_error(calculate(numeric(), numeric()))
expect_error(calculate(0.5, 0.5, aas_positive = 0, aas_negative = 0))
expect_error(calculate(0.5, 0.5, cta_unit_cost = -1))

cat("Synthetic economic engine tests passed.\n")
