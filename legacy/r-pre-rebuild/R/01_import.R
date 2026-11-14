# =============================================================================
# 01_import.R
# AAS CTA R Reanalysis
#
# Stage 3a: Source data import
#
# Objective:
#   Faithfully import and characterize the authoritative Excel source workbook
#   without substantive recoding, filtering, deduplication, cohort construction,
#   or variable derivation.
#
# Source:
#   ASER_new.xlsx
#
# Expected location:
#   RESEARCH_DATA_ROOT/radiology/cta-aortic-dissection/raw/
#
# Historical SAS results will be used only as reconciliation landmarks.
# =============================================================================


# ---- 1. Project setup ---------------------------------------------------------

source("R/00_setup.R")


# ---- 2. Define source workbook ------------------------------------------------

source_file <- file.path(
  aas_raw_dir,
  "ASER_new.xlsx"
)

source_sheet <- "Sheet1"


# ---- 3. Verify source workbook exists ----------------------------------------

if (!file.exists(source_file)) {
  stop(
    paste0(
      "Source workbook not found:\n",
      source_file
    ),
    call. = FALSE
  )
}

message("Source workbook found:")
message(source_file)

message("Worksheet selected: ", source_sheet)

# ---- 4. Check required package -----------------------------------------------

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop(
    paste0(
      "Package 'readxl' is required but is not installed.\n",
      "Install it with:\n",
      "  renv::install(\"readxl\")\n",
      "Then run:\n",
      "  renv::snapshot()"
    ),
    call. = FALSE
  )
}


# ---- 5. Import source workbook ------------------------------------------------

source_raw <- readxl::read_excel(
  path = source_file,
  sheet = source_sheet,
  col_types = "text",
  .name_repair = "minimal"
)

message("Source workbook imported successfully.")

# ---- 6. Validate imported dimensions -----------------------------------------

n_source_rows <- nrow(source_raw)
n_source_columns <- ncol(source_raw)

message("Imported rows: ", n_source_rows)
message("Imported columns: ", n_source_columns)

# ---- 6A. Verify expected source dimensions -----------------------------------

expected_source_rows <- 2900L
expected_source_columns <- 45L

if (
  n_source_rows != expected_source_rows ||
  n_source_columns != expected_source_columns
) {
  stop(
    paste0(
      "Unexpected source workbook dimensions.\n",
      "Expected: ",
      expected_source_rows,
      " rows x ",
      expected_source_columns,
      " columns.\n",
      "Imported: ",
      n_source_rows,
      " rows x ",
      n_source_columns,
      " columns.\n",
      "Do not proceed until the source-file discrepancy is resolved."
    ),
    call. = FALSE
  )
}

message("Source dimensions match the verified workbook.")

# ---- 7. Inspect source column names ------------------------------------------

source_column_names <- names(source_raw)

message("\nSource column names:")
print(source_column_names)

# ---- 8. Check for duplicated column names ------------------------------------

duplicated_column_names <- unique(
  source_column_names[duplicated(source_column_names)]
)

if (length(duplicated_column_names) == 0L) {
  message("\nNo duplicated column names detected.")
} else {
  warning(
    paste0(
      "\nDuplicated column names detected: ",
      paste(duplicated_column_names, collapse = ", ")
    ),
    call. = FALSE
  )
}

# ---- 9. Summarize source missingness -----------------------------------------

missing_summary <- data.frame(
  variable = names(source_raw),
  
  n_missing = vapply(
    source_raw,
    function(x) sum(is.na(x)),
    integer(1)
  ),
  
  pct_missing = round(
    100 * vapply(
      source_raw,
      function(x) mean(is.na(x)),
      numeric(1)
    ),
    2
  )
)

message("\nMissingness summary:")
print(missing_summary, row.names = FALSE)

# ---- 10. Assess exact duplicate rows -----------------------------------------

n_exact_duplicate_rows <- sum(duplicated(source_raw))

message(
  "\nExact duplicated rows in source workbook: ",
  n_exact_duplicate_rows
)

# ---- 11. Import checkpoint ----------------------------------------------------

message(
  paste0(
    "\n01_import.R completed successfully.\n",
    "No substantive recoding, filtering, deduplication, cohort construction, ",
    "or variable derivation has been performed."
  )
)