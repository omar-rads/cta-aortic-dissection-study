# =============================================================================
# 03_cohort_construction.R
# AAS CTA R Reanalysis
#
# Stage 4: Cohort construction and composite CTA study definition
#
# Objective:
#   Independently construct the analytic CTA cohort from cleaned accession-level
#   source records.
#
# Cohort definition to evaluate:
#   A qualifying composite AAS CTA study requires both:
#     1. a chest CTA component, and
#     2. an abdomen/pelvis CTA component,
#   within the same encounter and ordered within 60 minutes.
#
# Input:
#   source_clean from R/02_cleaning.R
#
# Important:
#   The historical SAS cohort size of 1,194 is a reconciliation target, not a
#   required result. Cohort membership will be determined independently from
#   the source data and study definition.
# =============================================================================


# ---- 1. Load cleaned source records ------------------------------------------

source("R/02_cleaning.R")

cohort_source <- source_clean

message(
  "Cohort-construction source loaded: ",
  nrow(cohort_source),
  " accession-level records."
)

# ---- 2. Classify CTA component region ----------------------------------------

cohort_source$scan_region <- NA_character_

cohort_source$scan_region[
  cohort_source[["Ordered Item"]] %in% c(
    "CTA Chest w/ + w/o Contrast",
    "CTA Chest w/ + w/o Contrast (Aorta)"
  )
] <- "Chest"

cohort_source$scan_region[
  cohort_source[["Ordered Item"]] ==
    "CTA Abdomen + Pelvis w/Contrast"
] <- "AbdomenPelvis"

# ---- 3. Validate CTA component classification --------------------------------

n_unmapped_scan_regions <- sum(
  is.na(cohort_source$scan_region)
)

message(
  "\nUnmapped CTA component records: ",
  n_unmapped_scan_regions
)

message("\nCTA component distribution:")
print(
  table(
    cohort_source$scan_region,
    useNA = "ifany"
  )
)

if (n_unmapped_scan_regions > 0L) {
  stop(
    "At least one Ordered Item could not be mapped to a CTA component region.",
    call. = FALSE
  )
}

# ---- 4. Group accession-level records by encounter ---------------------------

encounter_groups <- split(
  seq_len(nrow(cohort_source)),
  cohort_source[["Encounter"]]
)

# ---- 5. Summarize CTA components within each encounter -----------------------

encounter_component_summary <- data.frame(
  
  encounter = names(encounter_groups),
  
  patient_empi = vapply(
    encounter_groups,
    function(i) {
      unique(cohort_source[["Patient EMPI Nbr"]][i])
    },
    character(1)
  ),
  
  n_accessions = vapply(
    encounter_groups,
    length,
    integer(1)
  ),
  
  n_chest = vapply(
    encounter_groups,
    function(i) {
      sum(cohort_source$scan_region[i] == "Chest")
    },
    integer(1)
  ),
  
  n_abdomen_pelvis = vapply(
    encounter_groups,
    function(i) {
      sum(cohort_source$scan_region[i] == "AbdomenPelvis")
    },
    integer(1)
  ),
  
  stringsAsFactors = FALSE
)

# ---- 6. Identify encounters containing each protocol component ---------------

encounter_component_summary$has_chest <-
  encounter_component_summary$n_chest >= 1L

encounter_component_summary$has_abdomen_pelvis <-
  encounter_component_summary$n_abdomen_pelvis >= 1L

encounter_component_summary$has_both_components <-
  encounter_component_summary$has_chest &
  encounter_component_summary$has_abdomen_pelvis

# ---- 7. Validate encounter-level component summary ---------------------------

message(
  "\nEncounter-level summary rows: ",
  nrow(encounter_component_summary)
)

message(
  "Total accessions represented in encounter summary: ",
  sum(encounter_component_summary$n_accessions)
)

message("\nChest x abdomen/pelvis component-count pattern:")
print(
  table(
    Chest = encounter_component_summary$n_chest,
    AbdomenPelvis = encounter_component_summary$n_abdomen_pelvis
  )
)

message(
  "\nEncounters containing both CTA components: ",
  sum(encounter_component_summary$has_both_components)
)

message(
  "Encounters missing at least one CTA component: ",
  sum(!encounter_component_summary$has_both_components)
)

# ---- 8. Parse order timestamps for cohort-pairing analysis -------------------

cohort_source$order_datetime_cohort <- as.POSIXct(
  cohort_source[["order timestamp"]],
  format = "%m/%d/%Y %H:%M:%S",
  tz = "America/New_York"
)

n_failed_order_times <- sum(
  is.na(cohort_source$order_datetime_cohort)
)

message(
  "\nOrder timestamps failing cohort-stage parsing: ",
  n_failed_order_times
)

if (n_failed_order_times > 0L) {
  stop(
    "At least one order timestamp failed cohort-stage datetime parsing.",
    call. = FALSE
  )
}

# ---- 9. Calculate minimum chest-to-abdomen/pelvis order-time gap -------------

encounter_component_summary$min_cross_region_order_gap_min <- vapply(
  encounter_groups,
  function(i) {
    
    regions <- cohort_source$scan_region[i]
    
    if (
      !any(regions == "Chest") ||
      !any(regions == "AbdomenPelvis")
    ) {
      return(NA_real_)
    }
    
    chest_times <- cohort_source$order_datetime_cohort[i][
      regions == "Chest"
    ]
    
    abdomen_pelvis_times <- cohort_source$order_datetime_cohort[i][
      regions == "AbdomenPelvis"
    ]
    
    pairwise_gap_seconds <- abs(
      outer(
        as.numeric(chest_times),
        as.numeric(abdomen_pelvis_times),
        "-"
      )
    )
    
    min(pairwise_gap_seconds) / 60
  },
  numeric(1)
)

# ---- 10. Evaluate the 60-minute composite-study criterion --------------------

encounter_component_summary$within_60_minutes <-
  encounter_component_summary$has_both_components &
  !is.na(
    encounter_component_summary$min_cross_region_order_gap_min
  ) &
  encounter_component_summary$min_cross_region_order_gap_min <= 60

# NOTE:
# The <=60-minute order-time criterion is retained here as a validation flag.
# It is not used to define the primary cohort in 03_cohort_construction.R.
# A timing-restricted cohort will be evaluated later in
# R/07_sensitivity_analyses.R.
# ---- 11. Audit timing-based cohort eligibility -------------------------------

both_component_encounters <-
  encounter_component_summary[
    encounter_component_summary$has_both_components,
    ,
    drop = FALSE
  ]

message(
  "\nBoth-component encounters: ",
  nrow(both_component_encounters)
)

message(
  "Both-component encounters with missing pair timing: ",
  sum(
    is.na(
      both_component_encounters$min_cross_region_order_gap_min
    )
  )
)

message(
  "Both-component encounters within 60 minutes: ",
  sum(
    both_component_encounters$within_60_minutes
  )
)

message(
  "Both-component encounters exceeding 60 minutes: ",
  sum(
    !both_component_encounters$within_60_minutes
  )
)

message("\nDistribution of minimum cross-region order-time gap (minutes):")
print(
  summary(
    both_component_encounters$min_cross_region_order_gap_min
  )
)

message("\nTiming categories:")
print(
  table(
    cut(
      both_component_encounters$min_cross_region_order_gap_min,
      breaks = c(
        -Inf,
        0,
        15,
        30,
        60,
        120,
        Inf
      ),
      labels = c(
        "0 minutes",
        ">0 to 15",
        ">15 to 30",
        ">30 to 60",
        ">60 to 120",
        ">120"
      ),
      right = TRUE
    ),
    useNA = "ifany"
  )
)

# ---- 12. Characterize encounters exceeding the 60-minute criterion ----------

timing_discrepant_encounters <-
  encounter_component_summary[
    encounter_component_summary$has_both_components &
      encounter_component_summary$min_cross_region_order_gap_min > 60,
    ,
    drop = FALSE
  ]

message("\nComponent patterns among >60-minute encounters:")
print(
  table(
    Chest = timing_discrepant_encounters$n_chest,
    AbdomenPelvis = timing_discrepant_encounters$n_abdomen_pelvis
  )
)

message("\nSorted minimum cross-region gaps >60 minutes:")
print(
  round(
    sort(
      timing_discrepant_encounters$min_cross_region_order_gap_min
    ),
    1
  )
)

# ---- 13. Build closest cross-region pair audit -------------------------------

closest_cross_region_pair <- function(i) {
  
  regions <- cohort_source$scan_region[i]
  
  chest_idx <- i[regions == "Chest"]
  abdomen_idx <- i[regions == "AbdomenPelvis"]
  
  if (length(chest_idx) == 0L || length(abdomen_idx) == 0L) {
    return(NULL)
  }
  
  chest_times <-
    cohort_source$order_datetime_cohort[chest_idx]
  
  abdomen_times <-
    cohort_source$order_datetime_cohort[abdomen_idx]
  
  gap_minutes <- abs(
    outer(
      as.numeric(chest_times),
      as.numeric(abdomen_times),
      "-"
    )
  ) / 60
  
  closest_pair <- which(
    gap_minutes == min(gap_minutes),
    arr.ind = TRUE
  )[1, ]
  
  chest_row <- chest_idx[closest_pair[1]]
  abdomen_row <- abdomen_idx[closest_pair[2]]
  
  data.frame(
    encounter =
      cohort_source[["Encounter"]][chest_row],
    
    patient_empi =
      cohort_source[["Patient EMPI Nbr"]][chest_row],
    
    chest_accession =
      cohort_source[["EEMR Accession Number"]][chest_row],
    
    abdomen_pelvis_accession =
      cohort_source[["EEMR Accession Number"]][abdomen_row],
    
    chest_order_time =
      cohort_source$order_datetime_cohort[chest_row],
    
    abdomen_pelvis_order_time =
      cohort_source$order_datetime_cohort[abdomen_row],
    
    gap_minutes =
      gap_minutes[closest_pair[1], closest_pair[2]],
    
    n_chest =
      sum(regions == "Chest"),
    
    n_abdomen_pelvis =
      sum(regions == "AbdomenPelvis"),
    
    stringsAsFactors = FALSE
  )
}

closest_pair_audit <- do.call(
  rbind,
  lapply(
    encounter_groups,
    closest_cross_region_pair
  )
)

timing_discrepant_pair_audit <-
  closest_pair_audit[
    closest_pair_audit$gap_minutes > 60,
    ,
    drop = FALSE
  ]

message(
  "\nTiming-discrepant pair-audit rows created: ",
  nrow(timing_discrepant_pair_audit)
)

# ---- 14. Create provisional primary both-component cohort --------------------

primary_cohort_encounters <-
  encounter_component_summary[
    encounter_component_summary$has_both_components,
    ,
    drop = FALSE
  ]

message(
  "\nProvisional primary cohort encounters: ",
  nrow(primary_cohort_encounters)
)

message(
  "Timing restriction is not applied to the primary cohort."
)

# ---- 15. Inspect exam-start timestamp formatting -----------------------------

exam_start_raw <- cohort_source[["exam start timestamp"]]

exam_start_shapes <- gsub(
  "[0-9]",
  "D",
  exam_start_raw
)

message("\nExam-start timestamp format patterns:")
print(
  sort(
    table(
      exam_start_shapes,
      useNA = "ifany"
    ),
    decreasing = TRUE
  )
)

# ---- 16. Check order-date agreement in timing-discrepant encounters ----------

timing_discrepant_pair_audit$same_order_date <-
  as.Date(
    timing_discrepant_pair_audit$chest_order_time,
    tz = "America/New_York"
  ) ==
  as.Date(
    timing_discrepant_pair_audit$abdomen_pelvis_order_time,
    tz = "America/New_York"
  )

message("\nOrder-date relationship among >60-minute pairs:")
print(
  table(
    timing_discrepant_pair_audit$same_order_date,
    useNA = "ifany"
  )
)

# ---- 17. Verify exam-start date component ordering ---------------------------

exam_start_first_component <- as.integer(
  substr(exam_start_raw, 1, 2)
)

exam_start_second_component <- as.integer(
  substr(exam_start_raw, 4, 5)
)

message(
  "\nExam-start first date component range: ",
  min(exam_start_first_component),
  " to ",
  max(exam_start_first_component)
)

message(
  "Exam-start second date component range: ",
  min(exam_start_second_component),
  " to ",
  max(exam_start_second_component)
)

# ---- 18. Parse exam-start timestamps -----------------------------------------

cohort_source$exam_start_datetime <- as.POSIXct(
  cohort_source[["exam start timestamp"]],
  format = "%m/%d/%Y %H:%M",
  tz = "America/New_York"
)

exam_start_parse_failed <- is.na(
  cohort_source$exam_start_datetime
)

n_exam_start_parse_failures <- sum(
  exam_start_parse_failed
)

message(
  "\nExam-start timestamps failing datetime parsing: ",
  n_exam_start_parse_failures
)

message(
  "Exam-start records with day component 00: ",
  sum(
    exam_start_second_component == 0L,
    na.rm = TRUE
  )
)

message(
  "Parse failures with day component 00: ",
  sum(
    exam_start_parse_failed &
      exam_start_second_component == 0L,
    na.rm = TRUE
  )
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

# ---- 19. Add acquisition timing to the >60-minute pair audit -----------------

chest_exam_row <- match(
  timing_discrepant_pair_audit$chest_accession,
  cohort_source[["EEMR Accession Number"]]
)

abdomen_exam_row <- match(
  timing_discrepant_pair_audit$abdomen_pelvis_accession,
  cohort_source[["EEMR Accession Number"]]
)

if (
  any(is.na(chest_exam_row)) ||
  any(is.na(abdomen_exam_row))
) {
  stop(
    "At least one timing-discrepant accession could not be matched back to the source data.",
    call. = FALSE
  )
}

timing_discrepant_pair_audit$chest_exam_start <-
  cohort_source$exam_start_datetime[chest_exam_row]

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

# ---- 20. Evaluate acquisition timing in the >60-minute pair audit ------------

timing_discrepant_pair_audit$exam_start_gap_minutes <-
  abs(
    as.numeric(
      difftime(
        timing_discrepant_pair_audit$chest_exam_start,
        timing_discrepant_pair_audit$abdomen_pelvis_exam_start,
        units = "mins"
      )
    )
  )

message(
  "\nExam-start gap summary among the 20 >60-minute order pairs:"
)

print(
  summary(
    timing_discrepant_pair_audit$exam_start_gap_minutes
  )
)

message(
  "\nExam-start gap categories among the 20 >60-minute order pairs:"
)

print(
  table(
    cut(
      timing_discrepant_pair_audit$exam_start_gap_minutes,
      breaks = c(-Inf, 0, 15, 30, 60, 120, Inf),
      labels = c(
        "0 minutes",
        ">0 to 15",
        ">15 to 30",
        ">30 to 60",
        ">60 to 120",
        ">120"
      ),
      right = TRUE
    ),
    useNA = "ifany"
  )
)

timing_discrepant_pair_audit$same_exam_start_date <-
  as.Date(
    timing_discrepant_pair_audit$chest_exam_start,
    tz = "America/New_York"
  ) ==
  as.Date(
    timing_discrepant_pair_audit$abdomen_pelvis_exam_start,
    tz = "America/New_York"
  )

message(
  "\nExam-start date relationship among >60-minute order pairs:"
)

print(
  table(
    timing_discrepant_pair_audit$same_exam_start_date,
    useNA = "ifany"
  )
)


# ---- 21. Flag acquisition-timing cases requiring local review ----------------

acquisition_review_cases <-
  timing_discrepant_pair_audit[
    is.na(timing_discrepant_pair_audit$exam_start_gap_minutes) |
      timing_discrepant_pair_audit$exam_start_gap_minutes > 30,
    ,
    drop = FALSE
  ]

message(
  "\nTiming-discrepant encounters requiring targeted acquisition review: ",
  nrow(acquisition_review_cases)
)

# ---- 22. Audit adult-age eligibility in the provisional primary cohort -------

primary_encounter_ids <-
  primary_cohort_encounters$encounter

primary_cohort_source_rows <-
  cohort_source[
    cohort_source[["Encounter"]] %in% primary_encounter_ids,
    ,
    drop = FALSE
  ]

primary_cohort_source_rows$age_numeric <-
  suppressWarnings(
    as.numeric(
      primary_cohort_source_rows[["Age"]]
    )
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
  function(x) {
    length(unique(x[!is.na(x)]))
  }
)

message(
  "\nPrimary-cohort encounters with more than one recorded age: ",
  sum(ages_per_primary_encounter > 1L)
)

primary_age_by_encounter <- tapply(
  primary_cohort_source_rows$age_numeric,
  primary_cohort_source_rows[["Encounter"]],
  function(x) {
    
    values <- unique(x[!is.na(x)])
    
    if (length(values) == 1L) {
      return(values)
    }
    
    return(NA_real_)
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

# ---- 23. Apply adult-age eligibility to the primary cohort -------------------

primary_cohort_encounters$age <- unname(
  primary_age_by_encounter[
    primary_cohort_encounters$encounter
  ]
)

if (any(is.na(primary_cohort_encounters$age))) {
  stop(
    "At least one provisional primary-cohort encounter has unresolved age.",
    call. = FALSE
  )
}

underage_primary_encounters <-
  primary_cohort_encounters[
    primary_cohort_encounters$age < 18,
    ,
    drop = FALSE
  ]

message(
  "\nUnder-18 encounters excluded from primary cohort: ",
  nrow(underage_primary_encounters)
)

message(
  "Under-18 encounters satisfying the <=60-minute timing flag: ",
  sum(
    underage_primary_encounters$within_60_minutes,
    na.rm = TRUE
  )
)

final_cohort_encounters <-
  primary_cohort_encounters[
    primary_cohort_encounters$age >= 18,
    ,
    drop = FALSE
  ]

message(
  "Final adult primary cohort encounters: ",
  nrow(final_cohort_encounters)
)

# ---- 24. Retain all accession rows for included adult encounters -------------

final_cohort_source_rows <-
  cohort_source[
    cohort_source[["Encounter"]] %in%
      final_cohort_encounters$encounter,
    ,
    drop = FALSE
  ]

message(
  "\nAccession rows belonging to final adult cohort: ",
  nrow(final_cohort_source_rows)
)

message(
  "Unique encounters represented in final accession dataset: ",
  length(
    unique(
      final_cohort_source_rows[["Encounter"]]
    )
  )
)
if (
  length(unique(final_cohort_source_rows[["Encounter"]])) !=
  nrow(final_cohort_encounters)
) {
  stop(
    "Final encounter index and accession-level cohort do not contain the same encounters.",
    call. = FALSE
  )
}
if (
  length(unique(final_cohort_source_rows[["Encounter"]])) !=
  nrow(final_cohort_encounters)
) {
  stop(
    "Final encounter index and accession-level cohort do not contain the same encounters.",
    call. = FALSE
  )
}

# ---- 25. Characterize patient contribution to the final cohort ---------------

included_encounters_per_patient <- table(
  final_cohort_encounters$patient_empi
)

n_final_unique_patients <-
  length(included_encounters_per_patient)

n_repeat_patients <-
  sum(included_encounters_per_patient > 1L)

max_encounters_per_patient <-
  max(included_encounters_per_patient)

message(
  "\nUnique patients in final adult primary cohort: ",
  n_final_unique_patients
)

message(
  "Patients contributing more than one included encounter: ",
  n_repeat_patients
)

message(
  "Maximum included encounters contributed by one patient: ",
  max_encounters_per_patient
)

message("\nDistribution of included encounters per patient:")

print(
  table(
    as.integer(included_encounters_per_patient)
  )
)

# ---- 26. Verify repeated encounters occur on distinct visit dates ------------

ed_arrival_raw <-
  final_cohort_source_rows[["ED Arrival Timestamp"]]

ed_arrival_shapes <- gsub(
  "[0-9]",
  "D",
  ed_arrival_raw
)

message("\nED-arrival timestamp format patterns:")

print(
  sort(
    table(
      ed_arrival_shapes,
      useNA = "ifany"
    ),
    decreasing = TRUE
  )
)

final_cohort_source_rows$ed_arrival_datetime <-
  as.POSIXct(
    final_cohort_source_rows[["ED Arrival Timestamp"]],
    format = "%m/%d/%Y %H:%M:%S",
    tz = "America/New_York"
  )

n_ed_arrival_parse_failures <-
  sum(
    is.na(
      final_cohort_source_rows$ed_arrival_datetime
    )
  )

message(
  "\nED-arrival timestamps failing parsing: ",
  n_ed_arrival_parse_failures
)

# ---- 27. Record timing-restricted sensitivity-cohort count -------------------

n_timing_sensitivity_encounters <-
  sum(
    final_cohort_encounters$within_60_minutes,
    na.rm = TRUE
  )

message(
  "\nAdult encounters satisfying <=60-minute order-time criterion: ",
  n_timing_sensitivity_encounters
)

message(
  "This timing restriction is reserved for R/07_sensitivity_analyses.R."
)

# ---- 28. Create cohort-flow summary ------------------------------------------

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

print(
  cohort_flow,
  row.names = FALSE
)

# ---- 29. Final cohort integrity checks ---------------------------------------

if (any(final_cohort_encounters$age < 18)) {
  stop(
    "Final cohort contains an encounter younger than 18 years.",
    call. = FALSE
  )
}

if (!all(final_cohort_encounters$has_both_components)) {
  stop(
    "Final cohort contains an encounter missing a required CTA component.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    final_cohort_encounters$encounter
  ) > 0L
) {
  stop(
    "Final encounter-level cohort contains duplicated Encounter identifiers.",
    call. = FALSE
  )
}

message(
  "\nFinal cohort integrity checks passed."
)

# ---- 30. Save derived cohort objects -----------------------------------------

final_encounter_file <-
  file.path(
    aas_derived_dir,
    "aas_final_cohort_encounters.rds"
  )

final_accession_file <-
  file.path(
    aas_derived_dir,
    "aas_final_cohort_accessions.rds"
  )

saveRDS(
  final_cohort_encounters,
  final_encounter_file
)

saveRDS(
  final_cohort_source_rows,
  final_accession_file
)

message("\nDerived cohort files written:")
message(final_encounter_file)
message(final_accession_file)

# ---- 31. Stage completion checkpoint -----------------------------------------

message(
  "\n03_cohort_construction.R completed successfully."
)

message(
  "Primary cohort definition: adult encounters containing both chest and ",
  "abdomen/pelvis CTA components within the same encounter."
)

message(
  "The <=60-minute order-time restriction is not part of the primary cohort ",
  "and is reserved for sensitivity analysis."
)

message(
  "Final primary cohort encounters: ",
  nrow(final_cohort_encounters)
)

message(
  "Unique patients represented: ",
  n_final_unique_patients
)