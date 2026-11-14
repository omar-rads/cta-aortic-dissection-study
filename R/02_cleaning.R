# Why this script is here
# I clean the imported fields and examine the relationships among identifiers.
# I do not pair or collapse CTA components in this step.

# 02_cleaning.R
# AAS CTA reanalysis, stage 3b: source-data cleaning and identifier assessment.
#
# Characterizes the imported records at their source grain and establishes how
# patient, encounter and accession identifiers relate before cohort construction.
# No CTA pairing, exclusion or collapsing happens here. See R/03_cohort_construction.R.

source("R/01_import.R")

study_tz <- "America/New_York"

parse_timestamp <- function(x, format = "%m/%d/%Y %H:%M:%S") {
  as.POSIXct(x, format = format, tz = study_tz)
}

source_clean <- source_raw

message(
  "Cleaning working copy created: ",
  nrow(source_clean), " rows x ", ncol(source_clean), " columns."
)

# ---- Identifier completeness and uniqueness -----------------------------------
# I check how encounters, patients, and accessions relate before grouping rows.

identifier_columns <- c("Patient EMPI Nbr", "Encounter", "EEMR Accession Number")

identifier_summary <- data.frame(
  identifier = identifier_columns,
  n_missing = vapply(
    identifier_columns, function(v) sum(is.na(source_clean[[v]])), integer(1),
    USE.NAMES = FALSE
  ),
  n_unique = vapply(
    identifier_columns, function(v) length(unique(source_clean[[v]])), integer(1),
    USE.NAMES = FALSE
  )
)

message("\nKey identifier summary:")
print(identifier_summary, row.names = FALSE)

rows_per_encounter <- table(source_clean[["Encounter"]])
encounter_row_distribution <- table(as.integer(rows_per_encounter))

message("\nDistribution of source rows per encounter:")
print(encounter_row_distribution)
message("\nMaximum source rows in a single encounter: ", max(rows_per_encounter))

encounters_per_patient <- tapply(
  source_clean[["Encounter"]],
  source_clean[["Patient EMPI Nbr"]],
  function(x) length(unique(x))
)
patient_encounter_distribution <- table(as.integer(encounters_per_patient))

message("\nDistribution of unique encounters per patient:")
print(patient_encounter_distribution)
message("\nPatients with more than one encounter: ", sum(encounters_per_patient > 1))
message("Maximum encounters contributed by one patient: ", max(encounters_per_patient))

ordered_item_counts <- sort(table(source_clean[["Ordered Item"]]), decreasing = TRUE)

message("\nOrdered Item categories:")
print(ordered_item_counts)

patients_per_encounter <- tapply(
  source_clean[["Patient EMPI Nbr"]],
  source_clean[["Encounter"]],
  function(x) length(unique(x))
)

message("\nNumber of unique patients represented within each encounter:")
print(table(as.integer(patients_per_encounter)))

if (any(patients_per_encounter != 1L)) {
  stop(
    "At least one encounter maps to more than one patient EMPI. Resolve before cohort construction.",
    call. = FALSE
  )
}

# ---- Order timestamps ---------------------------------------------------------
# I inspect the source date format and parsing failures before using order times.

order_timestamp_raw <- source_clean[["order timestamp"]]
order_timestamp_shapes <- gsub("[0-9]", "D", order_timestamp_raw)

message("\nOrder timestamp format patterns:")
print(sort(table(order_timestamp_shapes), decreasing = TRUE))

# Month precedes day when the first component stays within 1-12.
first_date_component <- as.integer(substr(order_timestamp_raw, 1, 2))
second_date_component <- as.integer(substr(order_timestamp_raw, 4, 5))

message(
  "\nFirst date component range: ",
  min(first_date_component), " to ", max(first_date_component)
)
message(
  "Second date component range: ",
  min(second_date_component), " to ", max(second_date_component)
)

source_clean$order_timestamp <- parse_timestamp(order_timestamp_raw)
n_order_timestamp_parse_failures <- sum(is.na(source_clean$order_timestamp))

message("\nOrder timestamps failing datetime parsing: ", n_order_timestamp_parse_failures)

if (n_order_timestamp_parse_failures > 0L) {
  stop(
    "At least one order timestamp failed datetime parsing. Resolve before cohort construction.",
    call. = FALSE
  )
}

order_year <- as.integer(format(source_clean$order_timestamp, "%Y", tz = study_tz))

message("\nSource records by order year:")
print(table(order_year))

# ---- Checkpoint ---------------------------------------------------------------
# This confirms that cleaning kept the source row count intact.

if (nrow(source_clean) != nrow(source_raw)) {
  stop(
    paste0(
      "Cleaning changed the source row count from ", nrow(source_raw),
      " to ", nrow(source_clean), ". Resolve before cohort construction."
    ),
    call. = FALSE
  )
}

message(
  "\n02_cleaning.R completed successfully with ",
  nrow(source_clean), " source records preserved."
)
