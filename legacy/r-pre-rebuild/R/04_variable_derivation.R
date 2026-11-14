# =============================================================================
# 04_variable_derivation.R
# AAS CTA R Reanalysis
#
# Stage 5: Encounter-level variable derivation
#
# Objective:
#   Construct one scientifically explicit analytic record per included CTA
#   encounter and derive the variables required for descriptive analyses,
#   algorithm classification, outcomes, and modeling.
#
# Important:
#   Variables will not be inherited arbitrarily from one component accession.
#   Agreement between component rows will be evaluated before encounter-level
#   values are assigned.
#
# Input:
#   final_cohort_encounters
#   final_cohort_source_rows
#   from R/03_cohort_construction.R
# =============================================================================


# ---- 1. Load locked cohort ----------------------------------------------------

source("R/03_cohort_construction.R")

analytic_data <- final_cohort_encounters

message(
  "\nVariable-derivation cohort loaded: ",
  nrow(analytic_data),
  " encounters."
)

message(
  "Accession-level component records available: ",
  nrow(final_cohort_source_rows)
)

# ---- 2. Define encounter-agreement helper ------------------------------------

count_distinct_nonmissing <- function(x) {
  
  x <- x[
    !is.na(x) &
      trimws(as.character(x)) != ""
  ]
  
  length(unique(x))
}

# ---- 3. Audit expected encounter-invariant variables -------------------------

invariant_variables <- c(
  "Patient EMPI Nbr",
  "Age",
  "Gender",
  "Race",
  "Healthcare Entity",
  "ED Disposition",
  "Discharge Disposition DESC",
  "ED Arrival Timestamp",
  "Discharge Timestamp",
  "Reason for Visit",
  "ED Chief Complaint",
  "esi",
  "d-dimer"
)

encounter_ids <- analytic_data$encounter

invariant_audit <- data.frame(
  variable = invariant_variables,
  n_encounters_with_multiple_values = NA_integer_,
  stringsAsFactors = FALSE
)

for (j in seq_along(invariant_variables)) {
  
  variable_name <- invariant_variables[j]
  
  distinct_counts <- tapply(
    final_cohort_source_rows[[variable_name]],
    final_cohort_source_rows[["Encounter"]],
    count_distinct_nonmissing
  )
  
  relevant_counts <-
    distinct_counts[
      names(distinct_counts) %in%
        encounter_ids
    ]
  
  invariant_audit$n_encounters_with_multiple_values[j] <-
    sum(
      relevant_counts > 1L,
      na.rm = TRUE
    )
}

message("\nEncounter-level agreement audit:")

print(
  invariant_audit,
  row.names = FALSE
)

# ---- 4. Audit accession-specific variables within encounters -----------------

accession_specific_variables <- c(
  "Ordering Personnel",
  "Is the ordering personnel a physician? (Y/N)",
  "ordering provider type",
  "Order Description",
  "order timestamp",
  "exam start timestamp",
  "exam completion timestamp",
  "dictation date",
  "Postive? (Y/N)",
  "Dictation Clinical Indication listed?",
  "Incidental findings? (e.g. pulmonary embolism, etc)"
)

accession_specific_audit <- data.frame(
  variable = accession_specific_variables,
  n_encounters_with_multiple_values = NA_integer_,
  stringsAsFactors = FALSE
)

for (j in seq_along(accession_specific_variables)) {
  
  variable_name <- accession_specific_variables[j]
  
  distinct_counts <- tapply(
    final_cohort_source_rows[[variable_name]],
    final_cohort_source_rows[["Encounter"]],
    count_distinct_nonmissing
  )
  
  relevant_counts <-
    distinct_counts[
      names(distinct_counts) %in%
        encounter_ids
    ]
  
  accession_specific_audit$n_encounters_with_multiple_values[j] <-
    sum(
      relevant_counts > 1L,
      na.rm = TRUE
    )
}

message("\nAccession-specific disagreement audit:")

print(
  accession_specific_audit,
  row.names = FALSE
)

# ---- 5. Characterize provider disagreement within composite encounters -------

provider_type_counts_per_encounter <- tapply(
  final_cohort_source_rows[["ordering provider type"]],
  final_cohort_source_rows[["Encounter"]],
  count_distinct_nonmissing
)

provider_name_counts_per_encounter <- tapply(
  final_cohort_source_rows[["Ordering Personnel"]],
  final_cohort_source_rows[["Encounter"]],
  count_distinct_nonmissing
)

physician_flag_counts_per_encounter <- tapply(
  final_cohort_source_rows[["Is the ordering personnel a physician? (Y/N)"]],
  final_cohort_source_rows[["Encounter"]],
  count_distinct_nonmissing
)