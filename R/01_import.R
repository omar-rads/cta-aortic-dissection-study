# Why this script is here
# I read the source workbook and check its structure and missingness.
# I keep all source rows here and leave cohort decisions to later scripts.

# 01_import.R
# AAS CTA reanalysis, stage 3a: import the source workbook.
#
# Reads ASER_new.xlsx as text and checks its structure and missingness. Every
# source row is kept. Recoding, cohort construction and derivation come later.
# Historical SAS results are reconciliation landmarks only.

source("R/00_setup.R")

# I locate the workbook and check the reader before importing all fields as text.
source_file <- file.path(aas_raw_dir, "ASER_new.xlsx")
source_sheet <- "Sheet1"

if (!file.exists(source_file)) {
  stop(paste0("Source workbook not found:\n", source_file), call. = FALSE)
}

message("Source workbook found:")
message(source_file)
message("Worksheet selected: ", source_sheet)

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

source_raw <- readxl::read_excel(
  path = source_file,
  sheet = source_sheet,
  col_types = "text",
  .name_repair = "minimal"
)

# ---- Dimensions ---------------------------------------------------------------
# I compare the workbook dimensions with the expected snapshot so a changed source is visible.

n_source_rows <- nrow(source_raw)
n_source_columns <- ncol(source_raw)

message("Imported rows: ", n_source_rows)
message("Imported columns: ", n_source_columns)

expected_source_rows <- 2900L
expected_source_columns <- 45L

if (n_source_rows != expected_source_rows || n_source_columns != expected_source_columns) {
  stop(
    paste0(
      "Unexpected source workbook dimensions.\n",
      "Expected: ", expected_source_rows, " rows x ", expected_source_columns, " columns.\n",
      "Imported: ", n_source_rows, " rows x ", n_source_columns, " columns.\n",
      "Do not proceed until the source-file discrepancy is resolved."
    ),
    call. = FALSE
  )
}

# ---- Column names, missingness and duplicate rows -----------------------------
# I check the fields and source-row quality before applying cohort rules.

source_column_names <- names(source_raw)

message("\nSource column names:")
print(source_column_names)

duplicated_column_names <- unique(source_column_names[duplicated(source_column_names)])

if (length(duplicated_column_names) > 0L) {
  warning(
    paste0("\nDuplicated column names detected: ", paste(duplicated_column_names, collapse = ", ")),
    call. = FALSE
  )
}

missing_summary <- data.frame(
  variable = names(source_raw),
  n_missing = vapply(source_raw, function(x) sum(is.na(x)), integer(1)),
  pct_missing = round(100 * vapply(source_raw, function(x) mean(is.na(x)), numeric(1)), 2)
)

message("\nMissingness summary:")
print(missing_summary, row.names = FALSE)

n_exact_duplicate_rows <- sum(duplicated(source_raw))

message("\nExact duplicated rows in source workbook: ", n_exact_duplicate_rows)
