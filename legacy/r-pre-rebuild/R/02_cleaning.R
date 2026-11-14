# =============================================================================
# 02_cleaning.R
# AAS CTA R Reanalysis
#
# Stage 3b: Source-data cleaning and identifier assessment
#
# Objective:
#   Characterize and normalize the imported source records while preserving
#   the source-data grain. Establish the relationships among patient,
#   encounter, accession/order, provider, and site identifiers before cohort
#   construction.
#
# Input:
#   source_raw from R/01_import.R
#
# Important:
#   This script does not perform CTA pairing, cohort exclusions, or collapse
#   records into composite studies. Those operations can be found in
#   R/03_cohort_construction.R.
# =============================================================================


# ---- 1. Load validated source import -----------------------------------------

source("R/01_import.R")


# ---- 2. Create cleaning working copy -----------------------------------------

source_clean <- source_raw

message(
  "Cleaning working copy created: ",
  nrow(source_clean),
  " rows x ",
  ncol(source_clean),
  " columns."
)

# ---- 3. Assess key identifier completeness and uniqueness --------------------

identifier_summary <- data.frame(
  identifier = c(
    "Patient EMPI Nbr",
    "Encounter",
    "EEMR Accession Number"
  ),
  
  n_missing = c(
    sum(is.na(source_clean[["Patient EMPI Nbr"]])),
    sum(is.na(source_clean[["Encounter"]])),
    sum(is.na(source_clean[["EEMR Accession Number"]]))
  ),
  
  n_unique = c(
    length(unique(source_clean[["Patient EMPI Nbr"]])),
    length(unique(source_clean[["Encounter"]])),
    length(unique(source_clean[["EEMR Accession Number"]]))
  )
)

message("\nKey identifier summary:")
print(identifier_summary, row.names = FALSE)

# ---- 4. Assess number of source rows per encounter ---------------------------

rows_per_encounter <- table(
  source_clean[["Encounter"]]
)

encounter_row_distribution <- table(
  as.integer(rows_per_encounter)
)

message("\nDistribution of source rows per encounter:")
print(encounter_row_distribution)

message(
  "\nMaximum source rows in a single encounter: ",
  max(rows_per_encounter)
)


# ---- 5. Assess number of encounters per patient ------------------------------

encounters_per_patient <- tapply(
  source_clean[["Encounter"]],
  source_clean[["Patient EMPI Nbr"]],
  function(x) length(unique(x))
)

patient_encounter_distribution <- table(
  as.integer(encounters_per_patient)
)

message("\nDistribution of unique encounters per patient:")
print(patient_encounter_distribution)

message(
  "\nPatients with more than one encounter: ",
  sum(encounters_per_patient > 1)
)

message(
  "Maximum encounters contributed by one patient: ",
  max(encounters_per_patient)
)

# ---- 6. Inspect source imaging-order categories ------------------------------

ordered_item_counts <- sort(
  table(source_clean[["Ordered Item"]]),
  decreasing = TRUE
)

message("\nOrdered Item categories:")
print(ordered_item_counts)

# ---- 7. Verify encounter-to-patient consistency ------------------------------

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

message("Each encounter maps to exactly one patient EMPI.")

# ---- 8. Inspect order-timestamp formatting -----------------------------------

order_timestamp_raw <- source_clean[["order timestamp"]]

order_timestamp_shapes <- gsub(
  "[0-9]",
  "D",
  order_timestamp_raw
)

message("\nOrder timestamp format patterns:")
print(
  sort(
    table(order_timestamp_shapes),
    decreasing = TRUE
  )
)

# ---- 9. Verify order-timestamp date component ordering -----------------------

order_timestamp_raw <- source_clean[["order timestamp"]]

first_date_component <- as.integer(
  substr(order_timestamp_raw, 1, 2)
)

second_date_component <- as.integer(
  substr(order_timestamp_raw, 4, 5)
)

message(
  "\nFirst date component range: ",
  min(first_date_component),
  " to ",
  max(first_date_component)
)

message(
  "Second date component range: ",
  min(second_date_component),
  " to ",
  max(second_date_component)
)

# ---- 10. Parse order timestamp ------------------------------------------------

source_clean$order_timestamp <- as.POSIXct(
  source_clean[["order timestamp"]],
  format = "%m/%d/%Y %H:%M:%S",
  tz = "America/New_York"
)

n_order_timestamp_parse_failures <- sum(
  is.na(source_clean$order_timestamp)
)

message(
  "\nOrder timestamps failing datetime parsing: ",
  n_order_timestamp_parse_failures
)

if (n_order_timestamp_parse_failures > 0L) {
  stop(
    "At least one order timestamp failed datetime parsing. Resolve before cohort construction.",
    call. = FALSE
  )
}

message("All order timestamps parsed successfully.")


order_year <- as.integer(
  format(
    source_clean$order_timestamp,
    "%Y",
    tz = "America/New_York"
  )
)

message("\nSource records by order year:")
print(table(order_year))

# ---- 11. Cleaning checkpoint -------------------------------------------------

if (nrow(source_clean) != nrow(source_raw)) {
  stop(
    paste0(
      "Cleaning changed the source row count from ",
      nrow(source_raw),
      " to ",
      nrow(source_clean),
      ". Resolve before cohort construction."
    ),
    call. = FALSE
  )
}

message(
  "\n02_cleaning.R completed successfully with ",
  nrow(source_clean),
  " source records preserved."
)

message(
  "No cohort exclusions, CTA pairing, or composite-study construction ",
  "have been performed."
)