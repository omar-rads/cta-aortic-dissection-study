# Why this script is here
# I assemble adult encounters with both required CTA components.
# The primary unit is the encounter, and the order-time interval is a sensitivity flag.

# 03_cohort_construction.R
# AAS CTA reanalysis, stage 4: cohort construction and composite CTA study definition.
#
# Primary cohort: adult encounters with both a chest and an abdomen/pelvis CTA
# component in the same encounter. Repeat eligible encounters from one patient
# are kept. The adult-age restriction is applied after both-component encounters
# are assembled. The 60-minute order-time criterion is kept as a flag for
# R/07_sensitivity_analyses.R and does not define the primary cohort.
#
# The historical SAS cohort size of 1,194 is a reconciliation target, not a
# required result. Membership is determined from the source data alone.

source("R/02_cleaning.R")

cohort_source <- source_clean

message("Cohort-construction source loaded: ", nrow(cohort_source), " accession-level records.")

# ---- CTA component region -----------------------------------------------------
# I map ordered items to the two required regions and stop if a region is unmapped.

cohort_source$scan_region <- NA_character_
cohort_source$scan_region[
  cohort_source[["Ordered Item"]] %in%
    c("CTA Chest w/ + w/o Contrast", "CTA Chest w/ + w/o Contrast (Aorta)")
] <- "Chest"
cohort_source$scan_region[
  cohort_source[["Ordered Item"]] == "CTA Abdomen + Pelvis w/Contrast"
] <- "AbdomenPelvis"

n_unmapped_scan_regions <- sum(is.na(cohort_source$scan_region))

message("\nUnmapped CTA component records: ", n_unmapped_scan_regions)
message("\nCTA component distribution:")
print(table(cohort_source$scan_region, useNA = "ifany"))

if (n_unmapped_scan_regions > 0L) {
  stop("At least one Ordered Item could not be mapped to a CTA component region.", call. = FALSE)
}

# ---- Components per encounter -------------------------------------------------
# I group source rows by encounter and count the component types before applying eligibility.

encounter_groups <- split(seq_len(nrow(cohort_source)), cohort_source[["Encounter"]])

count_region <- function(region) {
  vapply(encounter_groups, function(i) sum(cohort_source$scan_region[i] == region), integer(1))
}

encounter_component_summary <- data.frame(
  encounter = names(encounter_groups),
  patient_empi = vapply(
    encounter_groups,
    function(i) unique(cohort_source[["Patient EMPI Nbr"]][i]),
    character(1)
  ),
  n_accessions = lengths(encounter_groups),
  n_chest = count_region("Chest"),
  n_abdomen_pelvis = count_region("AbdomenPelvis"),
  stringsAsFactors = FALSE
)

encounter_component_summary$has_chest <- encounter_component_summary$n_chest >= 1L
encounter_component_summary$has_abdomen_pelvis <-
  encounter_component_summary$n_abdomen_pelvis >= 1L
encounter_component_summary$has_both_components <-
  encounter_component_summary$has_chest & encounter_component_summary$has_abdomen_pelvis

message("\nEncounter-level summary rows: ", nrow(encounter_component_summary))
message(
  "Total accessions represented in encounter summary: ",
  sum(encounter_component_summary$n_accessions)
)
message("\nChest x abdomen/pelvis component-count pattern:")
print(table(
  Chest = encounter_component_summary$n_chest,
  AbdomenPelvis = encounter_component_summary$n_abdomen_pelvis
))
message(
  "\nEncounters containing both CTA components: ",
  sum(encounter_component_summary$has_both_components)
)
message(
  "Encounters missing at least one CTA component: ",
  sum(!encounter_component_summary$has_both_components)
)

# ---- Order-time gap between components ----------------------------------------
# I use the closest cross-region order pair for the timing flag. The primary cohort does not require the 60-minute limit.

cohort_source$order_datetime_cohort <- parse_timestamp(cohort_source[["order timestamp"]])
n_failed_order_times <- sum(is.na(cohort_source$order_datetime_cohort))

message("\nOrder timestamps failing cohort-stage parsing: ", n_failed_order_times)

if (n_failed_order_times > 0L) {
  stop("At least one order timestamp failed cohort-stage datetime parsing.", call. = FALSE)
}

# Chest-to-abdomen/pelvis order-time gaps in minutes for the rows `i` of one
# encounter: one matrix row per chest order, one column per abdomen/pelvis order.
cross_region_gap_minutes <- function(rows, i) {
  order_seconds <- as.numeric(rows$order_datetime_cohort[i])
  abs(outer(
    order_seconds[rows$scan_region[i] == "Chest"],
    order_seconds[rows$scan_region[i] == "AbdomenPelvis"],
    "-"
  )) / 60
}

# Source rows and gap of the chest/abdomen-pelvis pair ordered closest together.
# R/04_variable_derivation.R reuses this for the provider audit.
closest_cross_region_pair <- function(rows, i) {
  gap_minutes <- cross_region_gap_minutes(rows, i)
  pair <- which(gap_minutes == min(gap_minutes), arr.ind = TRUE)[1L, ]
  list(
    chest_row = i[rows$scan_region[i] == "Chest"][pair[1L]],
    abdomen_pelvis_row = i[rows$scan_region[i] == "AbdomenPelvis"][pair[2L]],
    gap_minutes = gap_minutes[pair[1L], pair[2L]]
  )
}

encounter_component_summary$min_cross_region_order_gap_min <- vapply(
  encounter_groups,
  function(i) {
    gap_minutes <- cross_region_gap_minutes(cohort_source, i)
    if (length(gap_minutes) == 0L) NA_real_ else min(gap_minutes)
  },
  numeric(1)
)

# Validation flag only. The timing-restricted cohort is a sensitivity analysis.
encounter_component_summary$within_60_minutes <-
  encounter_component_summary$has_both_components &
  !is.na(encounter_component_summary$min_cross_region_order_gap_min) &
  encounter_component_summary$min_cross_region_order_gap_min <= 60

primary_cohort_encounters <- encounter_component_summary[
  encounter_component_summary$has_both_components, , drop = FALSE
]

gap_breaks <- c(-Inf, 0, 15, 30, 60, 120, Inf)
gap_labels <- c("0 minutes", ">0 to 15", ">15 to 30", ">30 to 60", ">60 to 120", ">120")

message("\nBoth-component encounters: ", nrow(primary_cohort_encounters))
message(
  "Both-component encounters with missing pair timing: ",
  sum(is.na(primary_cohort_encounters$min_cross_region_order_gap_min))
)
message(
  "Both-component encounters within 60 minutes: ",
  sum(primary_cohort_encounters$within_60_minutes)
)
message(
  "Both-component encounters exceeding 60 minutes: ",
  sum(!primary_cohort_encounters$within_60_minutes)
)
message("\nDistribution of minimum cross-region order-time gap (minutes):")
print(summary(primary_cohort_encounters$min_cross_region_order_gap_min))
message("\nTiming categories:")
print(table(
  cut(
    primary_cohort_encounters$min_cross_region_order_gap_min,
    breaks = gap_breaks, labels = gap_labels, right = TRUE
  ),
  useNA = "ifany"
))

# ---- Encounters whose components were ordered more than 60 minutes apart ------
# I retain the closest pairs here for a closer timing and provider audit.

timing_discrepant_encounters <- encounter_component_summary[
  encounter_component_summary$has_both_components &
    encounter_component_summary$min_cross_region_order_gap_min > 60, , drop = FALSE
]

message("\nComponent patterns among >60-minute encounters:")
print(table(
  Chest = timing_discrepant_encounters$n_chest,
  AbdomenPelvis = timing_discrepant_encounters$n_abdomen_pelvis
))
message("\nSorted minimum cross-region gaps >60 minutes:")
print(round(sort(timing_discrepant_encounters$min_cross_region_order_gap_min), 1))

timing_discrepant_pairs <- lapply(
  encounter_groups[timing_discrepant_encounters$encounter],
  function(i) closest_cross_region_pair(cohort_source, i)
)
chest_pair_row <- vapply(timing_discrepant_pairs, `[[`, integer(1), "chest_row")
abdomen_pelvis_pair_row <- vapply(timing_discrepant_pairs, `[[`, integer(1), "abdomen_pelvis_row")

timing_discrepant_pair_audit <- data.frame(
  encounter = cohort_source[["Encounter"]][chest_pair_row],
  patient_empi = cohort_source[["Patient EMPI Nbr"]][chest_pair_row],
  chest_accession = cohort_source[["EEMR Accession Number"]][chest_pair_row],
  abdomen_pelvis_accession = cohort_source[["EEMR Accession Number"]][abdomen_pelvis_pair_row],
  chest_order_time = cohort_source$order_datetime_cohort[chest_pair_row],
  abdomen_pelvis_order_time = cohort_source$order_datetime_cohort[abdomen_pelvis_pair_row],
  gap_minutes = vapply(timing_discrepant_pairs, `[[`, numeric(1), "gap_minutes"),
  n_chest = timing_discrepant_encounters$n_chest,
  n_abdomen_pelvis = timing_discrepant_encounters$n_abdomen_pelvis,
  stringsAsFactors = FALSE
)

message("\nTiming-discrepant pair-audit rows created: ", nrow(timing_discrepant_pair_audit))
message("\nProvisional primary cohort encounters: ", nrow(primary_cohort_encounters))

# ---- Exam-start timestamps (audit only, not an inclusion criterion) -----------
# I compare acquisition timing for discrepant pairs as an audit, not as an inclusion rule.

exam_start_raw <- cohort_source[["exam start timestamp"]]
exam_start_shapes <- gsub("[0-9]", "D", exam_start_raw)

message("\nExam-start timestamp format patterns:")
print(sort(table(exam_start_shapes, useNA = "ifany"), decreasing = TRUE))

timing_discrepant_pair_audit$same_order_date <-
  as.Date(timing_discrepant_pair_audit$chest_order_time, tz = study_tz) ==
  as.Date(timing_discrepant_pair_audit$abdomen_pelvis_order_time, tz = study_tz)

message("\nOrder-date relationship among >60-minute pairs:")
print(table(timing_discrepant_pair_audit$same_order_date, useNA = "ifany"))

exam_start_first_component <- as.integer(substr(exam_start_raw, 1, 2))
exam_start_second_component <- as.integer(substr(exam_start_raw, 4, 5))

message(
  "\nExam-start first date component range: ",
  min(exam_start_first_component), " to ", max(exam_start_first_component)
)
message(
  "Exam-start second date component range: ",
  min(exam_start_second_component), " to ", max(exam_start_second_component)
)

cohort_source$exam_start_datetime <- parse_timestamp(exam_start_raw, format = "%m/%d/%Y %H:%M")
exam_start_parse_failed <- is.na(cohort_source$exam_start_datetime)
n_exam_start_parse_failures <- sum(exam_start_parse_failed)

message("\nExam-start timestamps failing datetime parsing: ", n_exam_start_parse_failures)
message(
  "Exam-start records with day component 00: ",
  sum(exam_start_second_component == 0L, na.rm = TRUE)
)
message(
  "Parse failures with day component 00: ",
  sum(exam_start_parse_failed & exam_start_second_component == 0L, na.rm = TRUE)
)

if (n_exam_start_parse_failures > 0L) {
  warning(
    paste0(
      n_exam_start_parse_failures,
      " exam-start timestamps could not be parsed. ",
      "Raw values are preserved; exam-start time is not being used ",
      "as a primary cohort inclusion criterion."
    ),
    call. = FALSE
  )
}

# Acquisition timing for the >60-minute pairs.
chest_exam_row <- match(
  timing_discrepant_pair_audit$chest_accession,
  cohort_source[["EEMR Accession Number"]]
)
abdomen_exam_row <- match(
  timing_discrepant_pair_audit$abdomen_pelvis_accession,
  cohort_source[["EEMR Accession Number"]]
)

if (anyNA(chest_exam_row) || anyNA(abdomen_exam_row)) {
  stop(
    "At least one timing-discrepant accession could not be matched back to the source data.",
    call. = FALSE
  )
}

timing_discrepant_pair_audit$chest_exam_start <- cohort_source$exam_start_datetime[chest_exam_row]
timing_discrepant_pair_audit$abdomen_pelvis_exam_start <-
  cohort_source$exam_start_datetime[abdomen_exam_row]
timing_discrepant_pair_audit$exam_start_pair_complete <-
  !is.na(timing_discrepant_pair_audit$chest_exam_start) &
  !is.na(timing_discrepant_pair_audit$abdomen_pelvis_exam_start)

message(
  "\n>60-minute encounters with both exam-start timestamps parseable: ",
  sum(timing_discrepant_pair_audit$exam_start_pair_complete)
)
message(
  ">60-minute encounters with at least one unparseable exam-start timestamp: ",
  sum(!timing_discrepant_pair_audit$exam_start_pair_complete)
)

timing_discrepant_pair_audit$exam_start_gap_minutes <- abs(as.numeric(difftime(
  timing_discrepant_pair_audit$chest_exam_start,
  timing_discrepant_pair_audit$abdomen_pelvis_exam_start,
  units = "mins"
)))

message("\nExam-start gap summary among the 20 >60-minute order pairs:")
print(summary(timing_discrepant_pair_audit$exam_start_gap_minutes))
message("\nExam-start gap categories among the 20 >60-minute order pairs:")
print(table(
  cut(
    timing_discrepant_pair_audit$exam_start_gap_minutes,
    breaks = gap_breaks, labels = gap_labels, right = TRUE
  ),
  useNA = "ifany"
))

timing_discrepant_pair_audit$same_exam_start_date <-
  as.Date(timing_discrepant_pair_audit$chest_exam_start, tz = study_tz) ==
  as.Date(timing_discrepant_pair_audit$abdomen_pelvis_exam_start, tz = study_tz)

message("\nExam-start date relationship among >60-minute order pairs:")
print(table(timing_discrepant_pair_audit$same_exam_start_date, useNA = "ifany"))

acquisition_review_cases <- timing_discrepant_pair_audit[
  is.na(timing_discrepant_pair_audit$exam_start_gap_minutes) |
    timing_discrepant_pair_audit$exam_start_gap_minutes > 30, , drop = FALSE
]

message(
  "\nTiming-discrepant encounters requiring targeted acquisition review: ",
  nrow(acquisition_review_cases)
)

# ---- Adult-age eligibility ----------------------------------------------------
# I apply the age restriction after identifying both-component encounters and check agreement across rows.

primary_cohort_source_rows <- cohort_source[
  cohort_source[["Encounter"]] %in% primary_cohort_encounters$encounter, , drop = FALSE
]
primary_cohort_source_rows$age_numeric <- suppressWarnings(
  as.numeric(primary_cohort_source_rows[["Age"]])
)

message(
  "\nPrimary-cohort accession rows with unparseable age: ",
  sum(is.na(primary_cohort_source_rows$age_numeric))
)
message(
  "Primary-cohort accession-level age range: ",
  min(primary_cohort_source_rows$age_numeric, na.rm = TRUE),
  " to ",
  max(primary_cohort_source_rows$age_numeric, na.rm = TRUE)
)

ages_per_primary_encounter <- tapply(
  primary_cohort_source_rows$age_numeric,
  primary_cohort_source_rows[["Encounter"]],
  function(x) length(unique(x[!is.na(x)]))
)

message(
  "\nPrimary-cohort encounters with more than one recorded age: ",
  sum(ages_per_primary_encounter > 1L)
)

# One age per encounter, or NA when the component rows disagree.
primary_age_by_encounter <- tapply(
  primary_cohort_source_rows$age_numeric,
  primary_cohort_source_rows[["Encounter"]],
  function(x) {
    values <- unique(x[!is.na(x)])
    if (length(values) == 1L) values else NA_real_
  }
)

message(
  "Primary-cohort encounters with unresolved encounter-level age: ",
  sum(is.na(primary_age_by_encounter))
)
message(
  "Primary-cohort encounters with age <18: ",
  sum(primary_age_by_encounter < 18, na.rm = TRUE)
)
message(
  "Primary-cohort encounters with age = 18: ",
  sum(primary_age_by_encounter == 18, na.rm = TRUE)
)
message(
  "Primary-cohort encounter-level age range: ",
  min(primary_age_by_encounter, na.rm = TRUE),
  " to ",
  max(primary_age_by_encounter, na.rm = TRUE)
)

primary_cohort_encounters$age <- unname(
  primary_age_by_encounter[primary_cohort_encounters$encounter]
)

if (any(is.na(primary_cohort_encounters$age))) {
  stop("At least one provisional primary-cohort encounter has unresolved age.", call. = FALSE)
}

underage_primary_encounters <- primary_cohort_encounters[
  primary_cohort_encounters$age < 18, , drop = FALSE
]

message("\nUnder-18 encounters excluded from primary cohort: ", nrow(underage_primary_encounters))
message(
  "Under-18 encounters satisfying the <=60-minute timing flag: ",
  sum(underage_primary_encounters$within_60_minutes, na.rm = TRUE)
)

final_cohort_encounters <- primary_cohort_encounters[
  primary_cohort_encounters$age >= 18, , drop = FALSE
]

message("Final adult primary cohort encounters: ", nrow(final_cohort_encounters))

# ---- Final cohort: accession rows and patient contribution --------------------
# I retain source rows for eligible encounters and check each patient’s contribution.

final_cohort_source_rows <- cohort_source[
  cohort_source[["Encounter"]] %in% final_cohort_encounters$encounter, , drop = FALSE
]

message("\nAccession rows belonging to final adult cohort: ", nrow(final_cohort_source_rows))
message(
  "Unique encounters represented in final accession dataset: ",
  length(unique(final_cohort_source_rows[["Encounter"]]))
)

if (length(unique(final_cohort_source_rows[["Encounter"]])) != nrow(final_cohort_encounters)) {
  stop(
    "Final encounter index and accession-level cohort do not contain the same encounters.",
    call. = FALSE
  )
}

included_encounters_per_patient <- table(final_cohort_encounters$patient_empi)
n_final_unique_patients <- length(included_encounters_per_patient)
n_repeat_patients <- sum(included_encounters_per_patient > 1L)
max_encounters_per_patient <- max(included_encounters_per_patient)

message("\nUnique patients in final adult primary cohort: ", n_final_unique_patients)
message("Patients contributing more than one included encounter: ", n_repeat_patients)
message("Maximum included encounters contributed by one patient: ", max_encounters_per_patient)
message("\nDistribution of included encounters per patient:")
print(table(as.integer(included_encounters_per_patient)))

ed_arrival_raw <- final_cohort_source_rows[["ED Arrival Timestamp"]]
ed_arrival_shapes <- gsub("[0-9]", "D", ed_arrival_raw)

message("\nED-arrival timestamp format patterns:")
print(sort(table(ed_arrival_shapes, useNA = "ifany"), decreasing = TRUE))

final_cohort_source_rows$ed_arrival_datetime <- parse_timestamp(ed_arrival_raw)
n_ed_arrival_parse_failures <- sum(is.na(final_cohort_source_rows$ed_arrival_datetime))

message("\nED-arrival timestamps failing parsing: ", n_ed_arrival_parse_failures)

n_timing_sensitivity_encounters <- sum(final_cohort_encounters$within_60_minutes, na.rm = TRUE)

message(
  "\nAdult encounters satisfying <=60-minute order-time criterion: ",
  n_timing_sensitivity_encounters
)

cohort_flow <- data.frame(
  stage = c(
    "Source accession records",
    "Unique source encounters",
    "Single-region encounters excluded",
    "Both-component encounters",
    "Under-18 encounters excluded",
    "Final adult primary cohort"
  ),
  n = c(
    nrow(cohort_source),
    nrow(encounter_component_summary),
    sum(!encounter_component_summary$has_both_components),
    nrow(primary_cohort_encounters),
    nrow(underage_primary_encounters),
    nrow(final_cohort_encounters)
  ),
  stringsAsFactors = FALSE
)

message("\nFinal cohort-flow summary:")
print(cohort_flow, row.names = FALSE)

# ---- Integrity checks and save ------------------------------------------------
# I check the encounter grain and eligibility before saving the cohort objects.

if (any(final_cohort_encounters$age < 18)) {
  stop("Final cohort contains an encounter younger than 18 years.", call. = FALSE)
}

if (!all(final_cohort_encounters$has_both_components)) {
  stop("Final cohort contains an encounter missing a required CTA component.", call. = FALSE)
}

if (anyDuplicated(final_cohort_encounters$encounter) > 0L) {
  stop("Final encounter-level cohort contains duplicated Encounter identifiers.", call. = FALSE)
}

final_encounter_file <- file.path(aas_derived_dir, "aas_final_cohort_encounters.rds")
final_accession_file <- file.path(aas_derived_dir, "aas_final_cohort_accessions.rds")

saveRDS(final_cohort_encounters, final_encounter_file)
saveRDS(final_cohort_source_rows, final_accession_file)

message("\nDerived cohort files written:")
message(final_encounter_file)
message(final_accession_file)

message("Final primary cohort encounters: ", nrow(final_cohort_encounters))
message("Unique patients represented: ", n_final_unique_patients)
