# Why this script is here
# I enforce the clinician-review gate for the exploratory report analysis.
# Final prevalence and historical comparisons wait for the required review.

# =============================================================================
# 12_report_alt_dx_results.R
# Clinician-review gate for the exploratory report-based analysis.
#
# This stage intentionally refuses to calculate final prevalence or compare
# with the historical regex method until the clinician-review workbook has been
# completed and any systematic error mechanism has been adjudicated.
# =============================================================================

source("R/00_setup.R")

args <- commandArgs(trailingOnly = TRUE)
run_id <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("REPORT_ALT_DX_RUN_ID")
if (!nzchar(run_id)) {
  pointer <- file.path(aas_derived_dir, "report_alt_dx", "current_run_path.txt")
  if (!file.exists(pointer)) stop("No report-analysis run was specified.", call. = FALSE)
  run_dir <- trimws(readLines(pointer, warn = FALSE)[1L])
} else {
  run_dir <- file.path(aas_derived_dir, "report_alt_dx", run_id)
}

# Check for the completed workbook before allowing this stage to proceed.
review_file <- file.path(run_dir, "clinician_audit", "clinician_audit_completed.xlsx")
if (!file.exists(review_file)) {
  stop(
    paste0(
      "Clinician review is required before final report-based prevalence, historical comparison, ",
      "or manuscript language can be generated. Complete the audit workbook and save a copy as: ",
      review_file
    ),
    call. = FALSE
  )
}

stop(
  paste0(
    "A completed clinician workbook is present, but it must first be audited for systematic error ",
    "mechanisms and a recalibration decision. Do not patch individual records or calculate final ",
    "cohort results until that decision is documented."
  ),
  call. = FALSE
)
