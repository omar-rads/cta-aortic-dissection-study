# Why this script is here
# I derive encounter-level clinical, provider, and outcome variables.
# ADD-RS uses available documentation, and a missing D-dimer result is not a negative test.

# 04_variable_derivation.R
# AAS CTA reanalysis, stage 5: encounter-level variable derivation.
#
# Builds one analytic record per included CTA encounter, with the variables used
# for descriptive analyses, algorithm classification, outcomes and regression.
# An encounter-level value is never inherited from an arbitrary component row:
# component rows must agree, or an explicit attribution rule applies.
#
# Input: final_cohort_encounters and final_cohort_source_rows from
# R/03_cohort_construction.R.

source("R/03_cohort_construction.R")

analytic_data <- final_cohort_encounters

message("\nVariable-derivation cohort loaded: ", nrow(analytic_data), " encounters.")
message("Accession-level component records available: ", nrow(final_cohort_source_rows))

# ---- Agreement of source fields within encounters -----------------------------
# I check whether component rows agree before carrying shared fields to the encounter level.

is_nonmissing <- function(x) !is.na(x) & trimws(as.character(x)) != ""

count_distinct_nonmissing <- function(x) length(unique(x[is_nonmissing(x)]))

# Number of encounters whose component rows carry more than one nonmissing value.
count_disagreeing_encounters <- function(variables) {
  data.frame(
    variable = variables,
    n_encounters_with_multiple_values = vapply(
      variables,
      function(variable_name) {
        value <- final_cohort_source_rows[[variable_name]]
        keep <- is_nonmissing(value)
        distinct_values <- tapply(
          value[keep],
          final_cohort_source_rows[["Encounter"]][keep],
          function(x) length(unique(x))
        )
        sum(distinct_values > 1L)
      },
      integer(1),
      USE.NAMES = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

invariant_audit <- count_disagreeing_encounters(c(
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
))

message("\nEncounter-level agreement audit:")
print(invariant_audit, row.names = FALSE)

accession_specific_audit <- count_disagreeing_encounters(c(
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
))

message("\nAccession-specific disagreement audit:")
print(accession_specific_audit, row.names = FALSE)

# ---- Provider fields: normalization and role mapping --------------------------
# I standardize provider fields for the audit and keep missing and unmatched types distinct.

normalize_audit_value <- function(x) {
  value <- trimws(as.character(x))
  value[is.na(value) | value == ""] <- NA_character_
  value
}

# Maps raw provider types to roles. The caller chooses the label for a missing
# type and for a nonmissing type outside the map.
classify_provider_role <- function(x, missing_label, unmatched_label) {
  value <- normalize_audit_value(x)
  role <- rep(missing_label, length(value))

  role[value %in% c(
    "ED Physician Director",
    "ED Provider",
    "Physician",
    "Physician Orthopaedic"
  )] <- "Attending physician"

  role[value %in% c(
    "Resident",
    "Resident Medicine",
    "Resident Results Route to Resident"
  )] <- "Resident physician"

  role[value %in% c(
    "ED MLP",
    "Nurse Practitioner w Rx Emergency Medicine",
    "Nurse Practitioner wo Rx Emergency Medicine",
    "Physicians Assistant CT Surgery",
    "Physicians Assistant Emergency Medicine"
  )] <- "APP"

  role[!is.na(value) & role == missing_label] <- unmatched_label
  role
}

provider_role_from_raw_type <- function(x) {
  classify_provider_role(x, missing_label = "Missing", unmatched_label = "Other")
}

component_set_status <- function(chest_values, abdomen_pelvis_values) {
  # sort() drops missing values.
  chest_set <- sort(unique(normalize_audit_value(chest_values)))
  abdomen_pelvis_set <- sort(unique(normalize_audit_value(abdomen_pelvis_values)))

  if (length(chest_set) == 0L && length(abdomen_pelvis_set) == 0L) {
    return("Both components missing")
  }
  if (length(chest_set) == 0L || length(abdomen_pelvis_set) == 0L) {
    return("Missing vs nonmissing")
  }
  if (identical(chest_set, abdomen_pelvis_set)) {
    "Same nonmissing set"
  } else {
    "Different nonmissing sets"
  }
}

# Role combinations of a chest/abdomen-pelvis order pair. The raw-type audit and
# the personnel audit report the same combinations under different labels.
provider_pair_labels <- data.frame(
  roles = c(
    "Attending physician/Attending physician",
    "Attending physician/Resident physician",
    "APP/Attending physician",
    "APP/APP",
    "Resident physician/Resident physician",
    "APP/Resident physician"
  ),
  raw_type = c(
    "Two physicians",
    "Physician vs resident",
    "Physician vs APP",
    "APP vs APP",
    "Resident vs resident",
    "Resident vs APP"
  ),
  personnel = c(
    "Two attending physicians",
    "Attending physician vs resident",
    "Attending physician vs APP",
    "APP vs APP",
    "Resident vs resident",
    "Resident vs APP"
  ),
  stringsAsFactors = FALSE
)

provider_pair_pattern <- function(chest_type, abdomen_pelvis_type,
                                  audit = c("raw_type", "personnel")) {
  audit <- match.arg(audit)
  types <- normalize_audit_value(c(chest_type, abdomen_pelvis_type))

  if (all(is.na(types))) {
    return(if (audit == "raw_type") "Both missing" else "Both provider types missing")
  }
  if (anyNA(types)) {
    return("Missing vs nonmissing")
  }
  if (audit == "raw_type" && types[1L] == types[2L]) {
    return("Same raw provider type")
  }

  roles <- paste(sort(provider_role_from_raw_type(types)), collapse = "/")
  label <- provider_pair_labels[[audit]][match(roles, provider_pair_labels$roles)]
  if (is.na(label)) "Other pattern" else label
}

# Stacks one record (a named list of scalars) per encounter into a data frame.
bind_records <- function(records) {
  as.data.frame(do.call(Map, c(f = c, unname(records))), stringsAsFactors = FALSE)
}

provider_audit_rows <- final_cohort_source_rows

provider_audit_rows$provider_name_audit <- normalize_audit_value(
  provider_audit_rows[["Ordering Personnel"]]
)
provider_audit_rows$provider_type_audit <- normalize_audit_value(
  provider_audit_rows[["ordering provider type"]]
)
provider_audit_rows$physician_flag_audit <- toupper(normalize_audit_value(
  provider_audit_rows[["Is the ordering personnel a physician? (Y/N)"]]
))
provider_audit_rows$provider_role_audit <- provider_role_from_raw_type(
  provider_audit_rows$provider_type_audit
)

recognized_physician_flag <-
  is.na(provider_audit_rows$physician_flag_audit) |
  provider_audit_rows$physician_flag_audit %in% c("Y", "N")

provider_audit_rows$physician_flag_audit[!recognized_physician_flag] <- "Other/indeterminate"

provider_audit_groups <- split(
  seq_len(nrow(provider_audit_rows)),
  provider_audit_rows[["Encounter"]]
)

provider_name <- provider_audit_rows$provider_name_audit
provider_type <- provider_audit_rows$provider_type_audit
physician_flag <- provider_audit_rows$physician_flag_audit
order_time <- provider_audit_rows$order_datetime_cohort

# ---- Provider disagreement within composite encounters (audit) ----------------
# I examine differences across component orders before choosing the primary attribution.

provider_disagreement_audit <- bind_records(lapply(
  provider_audit_groups,
  function(i) {
    region <- provider_audit_rows$scan_region[i]
    chest_i <- i[region == "Chest"]
    abdomen_pelvis_i <- i[region == "AbdomenPelvis"]

    pair <- closest_cross_region_pair(provider_audit_rows, i)
    chest_pair_type <- provider_type[pair$chest_row]
    abdomen_pelvis_pair_type <- provider_type[pair$abdomen_pelvis_row]
    chest_pair_name <- provider_name[pair$chest_row]
    abdomen_pelvis_pair_name <- provider_name[pair$abdomen_pelvis_row]

    list(
      encounter = provider_audit_rows[["Encounter"]][i[1L]],
      n_distinct_provider_names = count_distinct_nonmissing(provider_name[i]),
      n_distinct_provider_types = count_distinct_nonmissing(provider_type[i]),
      n_distinct_physician_flags = count_distinct_nonmissing(physician_flag[i]),
      provider_name_component_status = component_set_status(
        provider_name[chest_i], provider_name[abdomen_pelvis_i]
      ),
      provider_type_component_status = component_set_status(
        provider_type[chest_i], provider_type[abdomen_pelvis_i]
      ),
      physician_flag_component_status = component_set_status(
        physician_flag[chest_i], physician_flag[abdomen_pelvis_i]
      ),
      conflicting_y_n_flags = all(c("Y", "N") %in% physician_flag[i]),
      within_chest_provider_type_disagreement =
        count_distinct_nonmissing(provider_type[chest_i]) > 1L,
      within_abdomen_pelvis_provider_type_disagreement =
        count_distinct_nonmissing(provider_type[abdomen_pelvis_i]) > 1L,
      closest_chest_provider_type =
        ifelse(is.na(chest_pair_type), "<missing>", chest_pair_type),
      closest_abdomen_pelvis_provider_type =
        ifelse(is.na(abdomen_pelvis_pair_type), "<missing>", abdomen_pelvis_pair_type),
      closest_pair_provider_pattern =
        provider_pair_pattern(chest_pair_type, abdomen_pelvis_pair_type),
      closest_pair_provider_name_differs =
        !is.na(chest_pair_name) & !is.na(abdomen_pelvis_pair_name) &
        chest_pair_name != abdomen_pelvis_pair_name,
      closest_pair_provider_name_missing_vs_nonmissing =
        xor(is.na(chest_pair_name), is.na(abdomen_pelvis_pair_name)),
      closest_pair_personnel_pattern =
        provider_pair_pattern(chest_pair_type, abdomen_pelvis_pair_type, audit = "personnel"),
      closest_pair_provider_type_differs =
        !is.na(chest_pair_type) & !is.na(abdomen_pelvis_pair_type) &
        chest_pair_type != abdomen_pelvis_pair_type,
      closest_pair_missing_vs_nonmissing =
        xor(is.na(chest_pair_type), is.na(abdomen_pelvis_pair_type)),
      closest_pair_order_gap_min = pair$gap_minutes,
      closest_pair_abdomen_pelvis_minus_chest_min = as.numeric(difftime(
        order_time[pair$abdomen_pelvis_row], order_time[pair$chest_row],
        units = "mins"
      )),
      closest_pair_same_order_timestamp = pair$gap_minutes == 0
    )
  }
))

if (nrow(provider_disagreement_audit) != nrow(analytic_data)) {
  stop(
    "Provider disagreement audit does not contain one row per analytic encounter.",
    call. = FALSE
  )
}

provider_disagreement_summary <- data.frame(
  measure = c(
    "Analytic encounters",
    "More than one nonmissing ordering personnel",
    "Different nonmissing ordering-personnel sets between components",
    "Different nonmissing ordering personnel in closest component pair",
    "Ordering personnel missing on one component only",
    "More than one nonmissing raw provider type",
    "Different nonmissing raw provider-type sets between components",
    "Raw provider type missing on one component only",
    "Conflicting Y/N physician flags",
    "Physician flag missing on one component only",
    "Unrecognized physician-flag value present"
  ),
  n_encounters = with(provider_disagreement_audit, c(
    nrow(provider_disagreement_audit),
    sum(n_distinct_provider_names > 1L),
    sum(provider_name_component_status == "Different nonmissing sets"),
    sum(closest_pair_provider_name_differs),
    sum(provider_name_component_status == "Missing vs nonmissing"),
    sum(n_distinct_provider_types > 1L),
    sum(provider_type_component_status == "Different nonmissing sets"),
    sum(provider_type_component_status == "Missing vs nonmissing"),
    sum(conflicting_y_n_flags),
    sum(physician_flag_component_status == "Missing vs nonmissing"),
    sum(vapply(
      provider_audit_groups,
      function(i) any(physician_flag[i] == "Other/indeterminate", na.rm = TRUE),
      logical(1)
    ))
  )),
  stringsAsFactors = FALSE
)

message("\nProvider disagreement summary:")
print(provider_disagreement_summary, row.names = FALSE)

message("\nOrdering-personnel chest-versus-abdomen/pelvis status:")
print(table(provider_disagreement_audit$provider_name_component_status))

message("\nRaw provider-type chest-versus-abdomen/pelvis status:")
print(table(provider_disagreement_audit$provider_type_component_status))

message("\nPhysician-flag chest-versus-abdomen/pelvis status:")
print(table(provider_disagreement_audit$physician_flag_component_status))

closest_pair_provider_combinations <- as.data.frame(
  table(
    chest_provider_type = provider_disagreement_audit$closest_chest_provider_type,
    abdomen_pelvis_provider_type =
      provider_disagreement_audit$closest_abdomen_pelvis_provider_type
  ),
  stringsAsFactors = FALSE
)

closest_pair_provider_combinations <- closest_pair_provider_combinations[
  closest_pair_provider_combinations$Freq > 0L, , drop = FALSE
]

closest_pair_provider_combinations <- closest_pair_provider_combinations[
  order(
    -closest_pair_provider_combinations$Freq,
    closest_pair_provider_combinations$chest_provider_type,
    closest_pair_provider_combinations$abdomen_pelvis_provider_type
  ), , drop = FALSE
]

rownames(closest_pair_provider_combinations) <- NULL

message("\nClosest-order chest-versus-abdomen/pelvis raw provider-type combinations:")
for (i in seq_len(nrow(closest_pair_provider_combinations))) {
  message(
    "  Chest: ", closest_pair_provider_combinations$chest_provider_type[i],
    " | Abdomen/pelvis: ", closest_pair_provider_combinations$abdomen_pelvis_provider_type[i],
    " | n=", closest_pair_provider_combinations$Freq[i]
  )
}

message("\nClosest-order cross-component provider-pattern categories:")
print(sort(table(provider_disagreement_audit$closest_pair_provider_pattern), decreasing = TRUE))

personnel_disagreement_rows <-
  provider_disagreement_audit$closest_pair_provider_name_differs |
  provider_disagreement_audit$closest_pair_provider_name_missing_vs_nonmissing

raw_type_differs <- provider_disagreement_audit$closest_pair_provider_type_differs

provider_disagreement_overlap <- data.frame(
  category = c(
    "Different personnel and different raw provider type",
    "Different personnel with same raw provider type",
    "Same personnel with different raw provider type",
    "Same personnel and same raw provider type"
  ),
  n_encounters = c(
    sum(personnel_disagreement_rows & raw_type_differs),
    sum(personnel_disagreement_rows & !raw_type_differs),
    sum(!personnel_disagreement_rows & raw_type_differs),
    sum(!personnel_disagreement_rows & !raw_type_differs)
  ),
  stringsAsFactors = FALSE
)

message("\nOverlap of closest-pair personnel and raw provider-type disagreement:")
print(provider_disagreement_overlap, row.names = FALSE)

message("\nProvider-role patterns among closest pairs with different personnel:")
print(sort(
  table(provider_disagreement_audit$closest_pair_personnel_pattern[personnel_disagreement_rows]),
  decreasing = TRUE
))

# Where the raw provider type disagrees, and how that relates to order timing.
provider_disagreement_audit$provider_disagreement_location <- "None"

cross_component_type_difference <-
  provider_disagreement_audit$provider_type_component_status %in%
  c("Different nonmissing sets", "Missing vs nonmissing")

within_component_type_difference <-
  provider_disagreement_audit$within_chest_provider_type_disagreement |
  provider_disagreement_audit$within_abdomen_pelvis_provider_type_disagreement

provider_disagreement_audit$provider_disagreement_location[
  cross_component_type_difference & !within_component_type_difference
] <- "Cross-component only"

provider_disagreement_audit$provider_disagreement_location[
  !cross_component_type_difference & within_component_type_difference
] <- "Within-component only"

provider_disagreement_audit$provider_disagreement_location[
  cross_component_type_difference & within_component_type_difference
] <- "Cross- and within-component"

message("\nLocation of raw provider-type disagreement:")
print(sort(table(provider_disagreement_audit$provider_disagreement_location), decreasing = TRUE))

provider_disagreement_audit$closest_pair_order_gap_category <- cut(
  provider_disagreement_audit$closest_pair_order_gap_min,
  breaks = c(-Inf, 0, 15, 30, 60, Inf),
  labels = c(
    "0 minutes",
    ">0 to 15 minutes",
    ">15 to 30 minutes",
    ">30 to 60 minutes",
    ">60 minutes"
  ),
  right = TRUE
)

closest_pair_difference_status <- ifelse(
  raw_type_differs,
  "Different nonmissing raw types",
  ifelse(
    provider_disagreement_audit$closest_pair_missing_vs_nonmissing,
    "Missing vs nonmissing",
    "Same raw type or both missing"
  )
)

message("\nClosest-pair provider-type status by order-time gap:")
print(table(
  provider_type_status = closest_pair_difference_status,
  order_gap = provider_disagreement_audit$closest_pair_order_gap_category,
  useNA = "ifany"
))

message("\nDifferent-personnel closest pairs by raw-type difference and order-time gap:")
print(table(
  raw_type_differs = ifelse(
    raw_type_differs[personnel_disagreement_rows],
    "Different raw provider type",
    "Same raw provider type"
  ),
  order_gap =
    provider_disagreement_audit$closest_pair_order_gap_category[personnel_disagreement_rows],
  useNA = "ifany"
))

summarize_order_gap <- function(x) {
  c(n = length(x), median_min = median(x), min_min = min(x), max_min = max(x))
}

message("\nOrder-time summary among closest pairs with different personnel:")
print(summarize_order_gap(
  provider_disagreement_audit$closest_pair_order_gap_min[personnel_disagreement_rows]
))

closest_pair_order_sequence <- ifelse(
  provider_disagreement_audit$closest_pair_same_order_timestamp,
  "Same timestamp",
  ifelse(
    provider_disagreement_audit$closest_pair_abdomen_pelvis_minus_chest_min > 0,
    "Chest ordered first",
    "Abdomen/pelvis ordered first"
  )
)

message("\nOrder sequence among closest pairs with different personnel:")
print(table(closest_pair_order_sequence[personnel_disagreement_rows]))

message("\nOrder sequence among closest pairs with different raw provider types:")
print(table(closest_pair_order_sequence[raw_type_differs]))

message("\nClosest-pair order-time summary by provider-type status:")
print(do.call(rbind, lapply(
  split(provider_disagreement_audit$closest_pair_order_gap_min, closest_pair_difference_status),
  summarize_order_gap
)))

# ---- Primary provider attribution: earliest CTA order -------------------------
# I attribute the primary provider using the earliest CTA order and check the encounter matches.

provider_detailed_role <- function(x) {
  classify_provider_role(x, missing_label = "Unknown", unmatched_label = "Unknown")
}

provider_binary_group <- function(detailed_role) {
  ifelse(
    detailed_role %in% c("Attending physician", "Resident physician"),
    "Physician",
    ifelse(detailed_role == "APP", "Non-physician", "Unknown")
  )
}

first_nonmissing <- function(x) {
  value <- normalize_audit_value(x)
  value <- value[!is.na(value)]
  if (length(value) == 0L) NA_character_ else value[1L]
}

provider_attribution <- bind_records(lapply(
  provider_audit_groups,
  function(i) {
    earliest_time <- min(order_time[i])
    earliest_i <- i[order_time[i] == earliest_time]

    if (
      count_distinct_nonmissing(provider_name[earliest_i]) > 1L ||
      count_distinct_nonmissing(provider_type[earliest_i]) > 1L
    ) {
      stop(
        paste0(
          "A tied earliest order has conflicting provider metadata. ",
          "Provider attribution requires review."
        ),
        call. = FALSE
      )
    }

    attributed_name <- first_nonmissing(provider_name[earliest_i])
    attributed_raw_type <- first_nonmissing(provider_type[earliest_i])
    attributed_role <- provider_detailed_role(attributed_raw_type)
    attribution_resolution <- "Earliest-order raw provider type"

    # An uninformative earliest-order type falls back to a recognized role
    # recorded for the same person elsewhere in the encounter.
    if (attributed_role == "Unknown" && !is.na(attributed_name)) {
      same_person_i <- i[!is.na(provider_name[i]) & provider_name[i] == attributed_name]
      same_person_roles <- provider_detailed_role(provider_type[same_person_i])
      informative_i <- same_person_i[same_person_roles != "Unknown"]
      informative_roles <- unique(same_person_roles[same_person_roles != "Unknown"])

      if (length(informative_roles) > 1L) {
        stop(
          paste0(
            "The earliest-order personnel has multiple recognized provider roles. ",
            "Provider attribution requires review."
          ),
          call. = FALSE
        )
      }

      if (length(informative_roles) == 1L) {
        attributed_raw_type <- provider_type[informative_i[1L]]
        attributed_role <- informative_roles
        attribution_resolution <- "Same-person recognized-role fallback"
      }
    }

    raw_broad_classes <- provider_binary_group(provider_detailed_role(provider_type[i]))

    list(
      encounter = provider_audit_rows[["Encounter"]][i[1L]],
      primary_order_datetime = earliest_time,
      primary_order_component = paste(
        sort(unique(provider_audit_rows$scan_region[earliest_i])),
        collapse = "+"
      ),
      primary_provider_id = attributed_name,
      primary_provider_raw_type = attributed_raw_type,
      provider_role = attributed_role,
      provider_group = provider_binary_group(attributed_role),
      provider_attribution_resolution = attribution_resolution,
      earliest_raw_physician_flag = first_nonmissing(physician_flag[earliest_i]),
      multiple_ordering_personnel = count_distinct_nonmissing(provider_name[i]) > 1L,
      conflicting_raw_provider_type = count_distinct_nonmissing(provider_type[i]) > 1L,
      conflicting_physician_flag = all(c("Y", "N") %in% physician_flag[i]),
      cross_class_provider_disagreement =
        all(c("Physician", "Non-physician") %in% raw_broad_classes)
    )
  }
))

if (anyDuplicated(provider_attribution$encounter)) {
  stop("Provider attribution contains duplicated encounters.", call. = FALSE)
}

if (nrow(provider_attribution) != nrow(analytic_data)) {
  stop("Provider attribution is not one row per analytic encounter.", call. = FALSE)
}

provider_match <- match(analytic_data$encounter, provider_attribution$encounter)

if (anyNA(provider_match)) {
  stop("At least one analytic encounter lacks provider attribution.", call. = FALSE)
}

provider_columns <- setdiff(names(provider_attribution), "encounter")

analytic_data[provider_columns] <-
  provider_attribution[provider_match, provider_columns, drop = FALSE]

provider_reconciliation <- data.frame(
  measure = c(
    "Attending physician",
    "Resident physician",
    "APP",
    "Physician",
    "Non-physician",
    "Unknown"
  ),
  r_earliest_order_n = c(
    sum(analytic_data$provider_role == "Attending physician"),
    sum(analytic_data$provider_role == "Resident physician"),
    sum(analytic_data$provider_role == "APP"),
    sum(analytic_data$provider_group == "Physician"),
    sum(analytic_data$provider_group == "Non-physician"),
    sum(analytic_data$provider_group == "Unknown")
  ),
  historical_manuscript_sas_n = c(957L, 119L, 118L, 1076L, 118L, 0L),
  stringsAsFactors = FALSE
)

provider_reconciliation$difference_r_minus_historical <-
  provider_reconciliation$r_earliest_order_n -
  provider_reconciliation$historical_manuscript_sas_n

message("\nApproved earliest-order provider attribution:")
print(provider_reconciliation, row.names = FALSE)

message("\nProvider-attribution resolution:")
print(table(analytic_data$provider_attribution_resolution))

message("\nPreserved provider disagreement flags:")
print(
  data.frame(
    flag = c(
      "Multiple ordering personnel",
      "Conflicting raw provider type",
      "Conflicting physician Y/N",
      "Cross-class provider disagreement"
    ),
    n = c(
      sum(analytic_data$multiple_ordering_personnel),
      sum(analytic_data$conflicting_raw_provider_type),
      sum(analytic_data$conflicting_physician_flag),
      sum(analytic_data$cross_class_provider_disagreement)
    ),
    stringsAsFactors = FALSE
  ),
  row.names = FALSE
)

# ---- Demographic and ordering variables ---------------------------------------
# I use agreement across component rows for shared fields and the primary order where specified.

# The single nonmissing value of a source field within each encounter. Stops if
# component rows disagree.
consensus_character <- function(variable_name) {
  value <- normalize_audit_value(provider_audit_rows[[variable_name]])

  vapply(
    analytic_data$encounter,
    function(encounter_value) {
      values <- unique(value[provider_audit_groups[[encounter_value]]])
      values <- values[!is.na(values)]

      if (length(values) > 1L) {
        stop(
          paste0("Encounter-level consensus failed for variable: ", variable_name),
          call. = FALSE
        )
      }

      if (length(values) == 0L) NA_character_ else values[1L]
    },
    character(1)
  )
}

# Source row of each encounter's earliest CTA order.
primary_source_index <- vapply(
  analytic_data$encounter,
  function(encounter_value) {
    i <- provider_audit_groups[[encounter_value]]
    i[which.min(order_time[i])][1L]
  },
  integer(1)
)

analytic_data$sex <- consensus_character("Gender")
analytic_data$race_raw <- consensus_character("Race")
analytic_data$site_raw <- consensus_character("Healthcare Entity")
analytic_data$ed_disposition_raw <- consensus_character("ED Disposition")
analytic_data$discharge_disposition_raw <- consensus_character("Discharge Disposition DESC")
analytic_data$chief_complaint_raw <- consensus_character("ED Chief Complaint")
analytic_data$esi_raw <- consensus_character("esi")

analytic_data$site <- unname(
  c(
    "Emory Johns Creek Hospital" = "Hospital A",
    "Emory University Hospital" = "Hospital B",
    "Saint Josephs Hospital of Atlanta" = "Hospital C",
    "Crawford Long Hospital" = "Hospital D"
  )[analytic_data$site_raw]
)

if (anyNA(analytic_data$site)) {
  stop("At least one site could not be mapped to Hospital A-D.", call. = FALSE)
}

analytic_data$race_group <- ifelse(
  analytic_data$race_raw == "African American  or Black",
  "Black",
  ifelse(
    analytic_data$race_raw == "Caucasian or White",
    "White",
    ifelse(
      analytic_data$race_raw == "Asian",
      "Asian",
      ifelse(
        analytic_data$race_raw %in% c(
          "American Indian or Alaskan Native",
          "Multiple",
          "Native Hawaiian or Other Pacific Islander"
        ),
        "Other/Multiracial",
        ifelse(
          analytic_data$race_raw == "Unknown, Unavailable or Unreported",
          "Unknown",
          "Missing"
        )
      )
    )
  )
)

analytic_data$esi_num <- suppressWarnings(as.integer(sub(" .*", "", analytic_data$esi_raw)))
analytic_data$esi_num[!analytic_data$esi_num %in% 1:4] <- NA_integer_

analytic_data$esi_group <- ifelse(
  analytic_data$esi_num %in% c(1L, 2L),
  "ESI 1-2",
  ifelse(analytic_data$esi_num %in% c(3L, 4L), "ESI 3-4", "Missing")
)

primary_order_description <- normalize_audit_value(
  provider_audit_rows[["Order Description"]][primary_source_index]
)
primary_order_prefix <- toupper(trimws(sub(",.*$", "", primary_order_description)))

analytic_data$order_priority <- ifelse(
  is.na(primary_order_prefix),
  "Missing",
  ifelse(primary_order_prefix == "STAT", "STAT", "Non-STAT")
)

chief_lower <- tolower(
  ifelse(is.na(analytic_data$chief_complaint_raw), "", analytic_data$chief_complaint_raw)
)

analytic_data$chief_concern <- ifelse(
  grepl("chest pain", chief_lower, fixed = TRUE),
  "Chest pain",
  ifelse(
    grepl("back pain|flank", chief_lower),
    "Back/flank pain",
    ifelse(
      grepl("shortness|\\bsob\\b|dyspnea|cough", chief_lower, perl = TRUE),
      "SOB/respiratory",
      "Other/not recorded"
    )
  )
)

analytic_data$hospital_disposition <- ifelse(
  analytic_data$discharge_disposition_raw == "HOME SELF CARE",
  "Discharged home",
  ifelse(
    analytic_data$discharge_disposition_raw == "LEFT AMA",
    "Left AMA",
    ifelse(
      analytic_data$discharge_disposition_raw == "EXPIRED",
      "Expired",
      "Transferred/other"
    )
  )
)

# ---- ADD-RS with explicit missing-component flags -----------------------------
# I retain flags for missing components. The primary score sums observed components unless all three are missing.

find_add_rs_column <- function(pattern) {
  column <- grep(pattern, names(provider_audit_rows), value = TRUE)
  if (length(column) != 1L) {
    stop("ADD-RS component columns were not identified uniquely.", call. = FALSE)
  }
  column
}

normalize_add_rs_component <- function(x, allow_pain_text = FALSE) {
  value <- tolower(normalize_audit_value(x))
  result <- rep(NA_integer_, length(value))
  result[value == "0"] <- 0L
  result[value == "1"] <- 1L

  if (allow_pain_text) {
    result[value == "chest pain (severe)"] <- 1L
  }

  result
}

risk_condition_column <- find_add_rs_column("^High risk conditions\\?")
risk_pain_column <- find_add_rs_column("^High risk pain features\\?")
risk_exam_column <- find_add_rs_column("^High risk exam features\\?")

condition_raw <- consensus_character(risk_condition_column)
pain_raw <- consensus_character(risk_pain_column)
exam_raw <- consensus_character(risk_exam_column)

analytic_data$add_rs_condition <- normalize_add_rs_component(condition_raw)
analytic_data$add_rs_pain <- normalize_add_rs_component(pain_raw, allow_pain_text = TRUE)
analytic_data$add_rs_exam <- normalize_add_rs_component(exam_raw)

add_rs_components <- cbind(
  analytic_data$add_rs_condition,
  analytic_data$add_rs_pain,
  analytic_data$add_rs_exam
)

analytic_data$add_rs_missing_components <- rowSums(is.na(add_rs_components))
analytic_data$add_rs_partial_missing <- analytic_data$add_rs_missing_components %in% c(1L, 2L)
analytic_data$add_rs_all_missing <- analytic_data$add_rs_missing_components == 3L

# Primary score: observed components are summed, and only encounters missing all
# three components have no score.
analytic_data$add_rs <- rowSums(add_rs_components, na.rm = TRUE)
analytic_data$add_rs[analytic_data$add_rs_all_missing] <- NA_integer_

analytic_data$add_rs_strict_complete <- ifelse(
  analytic_data$add_rs_missing_components == 0L,
  rowSums(add_rs_components),
  NA_integer_
)

# ---- D-dimer status from abstracted values ------------------------------------
# I separate absent, positive, negative, and indeterminate results using the recorded value.

d_dimer_raw <- consensus_character("d-dimer")

derive_d_dimer_status <- function(x, threshold = 500) {
  if (is.na(x) || trimws(x) == "") {
    return("Absent/not obtained")
  }

  value <- gsub(",", "", tolower(trimws(x)))

  if (grepl("^[0-9]+(?:\\.[0-9]+)?$", value, perl = TRUE)) {
    numeric_value <- as.numeric(value)
    return(ifelse(numeric_value >= threshold, "Positive", "Negative"))
  }

  if (grepl("^<[[:space:]]*[0-9]+(?:\\.[0-9]+)?$", value, perl = TRUE)) {
    bound <- as.numeric(sub("^<[[:space:]]*", "", value))
    return(ifelse(bound <= threshold, "Negative", "Indeterminate"))
  }

  if (grepl("^>[[:space:]]*[0-9]+(?:\\.[0-9]+)?$", value, perl = TRUE)) {
    bound <- as.numeric(sub("^>[[:space:]]*", "", value))
    return(ifelse(bound >= threshold, "Positive", "Indeterminate"))
  }

  "Indeterminate"
}

analytic_data$d_dimer_status <- vapply(d_dimer_raw, derive_d_dimer_status, character(1))

analytic_data$d_dimer_obtained <- analytic_data$d_dimer_status != "Absent/not obtained"
analytic_data$d_dimer_positive <- ifelse(
  analytic_data$d_dimer_status == "Positive",
  1L,
  ifelse(analytic_data$d_dimer_status == "Negative", 0L, NA_integer_)
)

analytic_data$d_dimer_numeric_value <- suppressWarnings(as.numeric(gsub(",", "", d_dimer_raw)))

# ---- Algorithm classification and discordance reason --------------------------
# I apply the study classification to the reconstructed score and D-dimer status, including unclassifiable cases.

# ADD-RS >= 2 is concordant. ADD-RS 0-1 is concordant with a positive D-dimer and
# discordant with a negative or absent one. A missing ADD-RS, or ADD-RS 0-1 with
# an indeterminate D-dimer, is unclassifiable.
classify_algorithm <- function(add_rs, d_dimer_status) {
  algorithm_class <- rep(NA_character_, length(add_rs))
  algorithm_class[add_rs >= 2L] <- "Concordant"
  algorithm_class[add_rs <= 1L & d_dimer_status == "Positive"] <- "Concordant"
  algorithm_class[
    add_rs <= 1L & d_dimer_status %in% c("Negative", "Absent/not obtained")
  ] <- "Discordant"
  algorithm_class[
    is.na(add_rs) | (add_rs <= 1L & d_dimer_status == "Indeterminate")
  ] <- "Unclassifiable"
  algorithm_class
}

discordant_indicator <- function(algorithm_class) {
  ifelse(
    algorithm_class == "Discordant",
    1L,
    ifelse(algorithm_class == "Concordant", 0L, NA_integer_)
  )
}

analytic_data$algorithm_class <- classify_algorithm(
  analytic_data$add_rs,
  analytic_data$d_dimer_status
)
analytic_data$algorithm_discordant <- discordant_indicator(analytic_data$algorithm_class)

analytic_data$discordance_reason <- ifelse(
  analytic_data$algorithm_class != "Discordant",
  NA_character_,
  ifelse(
    analytic_data$d_dimer_status == "Absent/not obtained",
    "Low ADD-RS with absent D-dimer",
    "Low ADD-RS with negative D-dimer"
  )
)

# ---- Encounter-level AAS outcome ----------------------------------------------
# I combine component outcomes at encounter level and retain unknown outcomes when reports do not resolve them.

# Positive if any component report is positive, negative if every recorded
# component is negative, otherwise unknown.
positive_flag <- tolower(normalize_audit_value(provider_audit_rows[["Postive? (Y/N)"]]))

aas_outcome <- vapply(
  analytic_data$encounter,
  function(encounter_value) {
    values <- positive_flag[provider_audit_groups[[encounter_value]]]
    values <- unique(values[!is.na(values)])

    if ("y" %in% values) {
      return("Positive")
    }
    if (length(values) > 0L && all(values == "n")) {
      return("Negative")
    }
    "Unknown"
  },
  character(1)
)

analytic_data$aas_outcome <- aas_outcome
analytic_data$aas_positive <- ifelse(
  analytic_data$aas_outcome == "Positive",
  1L,
  ifelse(analytic_data$aas_outcome == "Negative", 0L, NA_integer_)
)

# ---- Alternative-diagnosis flags from incidental-findings text ----------------
# I apply the historical text patterns to the findings. These are rule-based flags from recorded text.
# Historical broad-net text rules, kept verbatim. The patterns are not edited here.

incidental_text <- local({
  finding <- normalize_audit_value(
    provider_audit_rows[["Incidental findings? (e.g. pulmonary embolism, etc)"]]
  )

  vapply(
    analytic_data$encounter,
    function(encounter_value) {
      values <- finding[provider_audit_groups[[encounter_value]]]
      values <- values[!is.na(values)]
      tolower(gsub("[[:space:]]+", " ", paste(values, collapse = " ")))
    },
    character(1)
  )
})

has_incidental_pattern <- function(pattern) {
  grepl(pattern, incidental_text, perl = TRUE)
}

alt_pe <- has_incidental_pattern(
  "\\b(pulmonary embol|pulmonary emboli|segmental pulmonary embol|subsegmental pulmonary embol|saddle pulmonary embol|pulmonary infarct|chronic pulmonary embol|chronic pulmonary emboli)\\b"
)

alt_pulmonary_infection <- has_incidental_pattern(
  "\\b(pneumonia|multifocal pneumonia|consolidation|consolidative|airspace opacity|airspace opacities|infiltrate|multifocal infection|aspiration|infectious/inflammatory)\\b"
) |
  (
    has_incidental_pattern(
      "\\b(tree-?in-?bud|tree in bud|mucoid impaction|mucus plugging|bronchial wall thickening|bronchiol|groundglass|ground glass|opacities|opacity)\\b"
    ) &
      has_incidental_pattern(
        "\\b(new|worsening|progressed|patchy|multifocal|consolidation|consolidative|airspace|infection|infectious|aspiration|infiltrate|pneumonia|opacification)\\b"
      )
  )

alt_ptx_hemopericardium <- has_incidental_pattern(
  "\\b(pneumothorax|hemothorax|hemopericardium|tamponade|large volume hemopericardium|resultant tamponade)\\b"
)

alt_pulmonary_edema <- has_incidental_pattern(
  "\\b(pulmonary edema|interstitial pulmonary edema|moderate pulmonary edema|volume overload)\\b"
)

alt_gi_inflammatory <- has_incidental_pattern(
  "\\b(diverticulitis|acute diverticulitis|colitis|proctitis|proctocolitis|enteritis|gastritis|duodenitis|pancreatitis|cholecystitis|stercoral colitis|appendicitis)\\b"
) |
  (
    has_incidental_pattern("\\b(appendix|appendiceal|appendicolith)\\b") &
      has_incidental_pattern(
        "\\b(thickened|prominent|fluid-filled|mucosal hyperemia|hyperemia|periappendiceal|stranding|fat stranding|wall thickening|mild periappendiceal|mild mucosal hyperemia)\\b"
      )
  ) |
  (
    has_incidental_pattern(
      "\\b(colon|colonic|rectum|rectal|sigmoid|descending colon|ascending colon|transverse colon|cecal|cecum|ileum|ileal|small bowel|jejunal|jejunum|gastric|stomach|pylorus|duodenal|duodenum)\\b"
    ) &
      has_incidental_pattern(
        "\\b(wall thickening|thickening|mucosal enhancement|hyperenhancement|hyperemia|fat stranding|inflammatory|pericolonic inflammatory|perirectal fat stranding|perirectal stranding|surrounding inflammatory change|mild perirectal fat stranding)\\b"
      )
  ) |
  (
    has_incidental_pattern(
      "\\b(gallbladder|cholecystic|pericholecystic|cholelithiasis|gallstone|gallstones)\\b"
    ) &
      has_incidental_pattern(
        "\\b(gallbladder wall thickening|wall thickening|pericholecystic fluid|pericholecystic fat stranding|inflammatory change|edema)\\b"
      )
  )

alt_gi_obstruction_perforation <- has_incidental_pattern(
  "\\b(small bowel obstruction|bowel obstruction|ileus|partial small bowel obstruction|transition point|fecalization|pneumoperitoneum|free air|free intraperitoneal air|perforation|loculated mesenteric air)\\b"
) |
  (
    has_incidental_pattern(
      "\\b(dilated loops of small bowel|fluid-filled loops of small bowel|multiple fluid-filled loops|fluid-filled bowel loops|distended bowel loops|bowel loops)\\b"
    ) &
      has_incidental_pattern(
        "\\b(obstruction|obstructive|ileus|transition point|fecalization|air-fluid levels|borderline dilated)\\b"
      )
  ) |
  (
    has_incidental_pattern(
      "\\b(hernia|ventral hernia|umbilical hernia|inguinal hernia|parastomal hernia|richter|small bowel containing hernia|contains a loop of small bowel|contains a knuckle of small bowel)\\b"
    ) &
      has_incidental_pattern(
        "\\b(obstruction|obstructive|angulation|stranding|narrow neck|bowel containing|contains bowel|incarcerat|fluid and fat stranding)\\b"
      )
  ) |
  (
    has_incidental_pattern(
      "\\b(stercoral|fecal impaction|stool ball|significant stool burden|markedly distended colon with fecal material)\\b"
    ) &
      has_incidental_pattern(
        "\\b(stranding|wall thickening|inflammatory|stercoral colitis)\\b"
      )
  )

alt_gu_acute <- has_incidental_pattern(
  "\\b(obstructing calculus|obstructive calculus|obstructing stone|ureteral stone|uvj calculus|ureterovesicular junction ureteral stone|midureter|proximal ureter|distal ureter|right midureter|left uvj|hydroureteronephrosis|hydronephrosis|hydroureter|periureteral fat stranding|perinephric stranding|delayed nephrogram|diminished renal enhancement|decreased enhancement|decreased perfusion|delayed enhancement|hypoperfusion|renal infarct|renal infarction|wedge-shaped areas of hypoattenuation|wedge-shaped hypodensity|nonenhancement in the kidney|cystitis|pyelonephritis)\\b"
) |
  (
    has_incidental_pattern(
      "\\b(bladder wall thickening|circumferential bladder wall thickening|urinary bladder wall thickening|thickening of the urinary bladder|marked circumferential bladder wall thickening|concentric thickening of the urinary bladder)\\b"
    ) &
      has_incidental_pattern(
        "\\b(perivesicular|perivesical|stranding|inflammatory|mucosal hyperenhancement|hyperenhancement|suspicious for cystitis|cystitis)\\b"
      )
  ) |
  (
    has_incidental_pattern(
      "\\b(distended urinary bladder|massively dilated urinary bladder|markedly distended urinary bladder)\\b"
    ) &
      has_incidental_pattern(
        "\\b(hydroureter|hydronephrosis|hydroureteronephrosis|caliectasis)\\b"
      )
  )

alt_hemorrhage_trauma <- has_incidental_pattern(
  "\\b(hematoma|retroperitoneal hematoma|intramuscular hematoma|hemoperitoneum|active hemorrhage|contrast blush|laceration|splenic injury|rupture|contained rupture|contusion|seatbelt sign|hemorrhagic pleural effusion|hemothorax)\\b"
) |
  (
    has_incidental_pattern(
      "\\b(fracture|fractures|stern(al)? fracture|rib fracture|pelvic injury|pubic rami fracture|acetabular fracture|splenic laceration)\\b"
    ) &
      has_incidental_pattern(
        "\\b(acute|subacute|nondisplaced|minimally displaced|displaced|compatible with)\\b"
      )
  )

alt_other_vascular <- has_incidental_pattern(
  "\\b(mesenteric venous thrombosis|portal vein.*thrombus|superior mesenteric vein|sma.*thrombus|mural thrombus of the proximal sma|pseudoaneurysm|endoleak|type i endoleak|type ii endoleak|type iii endoleak|aneurysm rupture|rupture of .* aneurysm|common femoral artery pseudoaneurysm|renal artery occlusion|common carotid artery.*filling defect|vertebral artery.*occlusion|focal filling defect in the right common femoral artery|occlusion of the proximal left superficial femoral artery|occluded right external iliac artery|acute rupture of large right common iliac artery aneurysm|superior mesenteric artery aneurysm|splenic artery aneurysm)\\b"
) |
  (
    has_incidental_pattern(
      "\\b(occlusion|near occlusion|critical stenosis|high-grade stenosis|severe narrowing|thrombus|filling defect|nonopacification)\\b"
    ) &
      has_incidental_pattern(
        "\\b(celiac|sma|superior mesenteric|renal artery|common carotid|vertebral|common femoral|iliac|subclavian|left atrial appendage)\\b"
      )
  )

alt_possible <- has_incidental_pattern(
  "\\b(pleural effusion|pericardial effusion|groundglass|ground glass|tree-?in-?bud|tree in bud|bronchial wall thickening|mucus plugging|mucoid impaction|atelectasis|collapse|small loculated pleural effusion)\\b"
) |
  has_incidental_pattern(
    "\\b(appendicolith|fluid-filled appendix|appendix is dilated|cecal stranding|bowel wall thickening|rectal wall thickening|gastric wall thickening|duodenal wall thickening|gallbladder distention|distended gallbladder|fluid-filled colon|stool burden|fecal load|constipation|dilated loops of small bowel|hernia containing bowel)\\b"
  ) |
  has_incidental_pattern(
    "\\b(bladder wall thickening|circumferential wall thickening of the urinary bladder|urinary bladder distention|hydronephrosis|hydroureter|perinephric stranding|perivesicular stranding|delayed nephrogram|delayed enhancement|decreased enhancement of the kidney)\\b"
  ) |
  has_incidental_pattern(
    "\\b(hiatal hernia|esophageal wall thickening|distal esophageal thickening|patulous esophagus|cholelithiasis|gallstones|gallstone|sludge)\\b"
  )

alternative_flags <- list(
  alt_pe = alt_pe,
  alt_pulmonary_infection = alt_pulmonary_infection,
  alt_ptx_hemopericardium = alt_ptx_hemopericardium,
  alt_pulmonary_edema = alt_pulmonary_edema,
  alt_gi_inflammatory = alt_gi_inflammatory,
  alt_gi_obstruction_perforation = alt_gi_obstruction_perforation,
  alt_gu_acute = alt_gu_acute,
  alt_hemorrhage_trauma = alt_hemorrhage_trauma,
  alt_other_vascular = alt_other_vascular,
  alt_possible = alt_possible
)

analytic_data[names(alternative_flags)] <- lapply(alternative_flags, as.integer)

# "Definite" is any category other than the possible-only patterns.
definite_alternative_columns <- setdiff(names(alternative_flags), "alt_possible")

analytic_data$alternative_diagnosis_definite <- as.integer(
  rowSums(analytic_data[definite_alternative_columns]) > 0L
)

analytic_data$alternative_diagnosis_definite_or_possible <- as.integer(
  analytic_data$alternative_diagnosis_definite == 1L | analytic_data$alt_possible == 1L
)

rm(incidental_text)

# ---- Validate and save the encounter-level analytic dataset -------------------
# I check the expected cohort and classification totals before saving the analytic dataset.

if (nrow(analytic_data) != 1193L) {
  stop("Analytic dataset no longer contains 1,193 encounters.", call. = FALSE)
}

if (anyDuplicated(analytic_data$encounter)) {
  stop("Analytic dataset contains duplicated encounters.", call. = FALSE)
}

if (length(unique(analytic_data$patient_empi)) != 1120L) {
  stop("Analytic dataset no longer contains 1,120 unique patients.", call. = FALSE)
}

if (!all(analytic_data$add_rs[!is.na(analytic_data$add_rs)] %in% 0:3)) {
  stop("ADD-RS values fall outside 0-3.", call. = FALSE)
}

if (
  sum(table(analytic_data$provider_role)) != nrow(analytic_data) ||
  sum(table(analytic_data$algorithm_class)) != nrow(analytic_data) ||
  sum(table(analytic_data$aas_outcome)) != nrow(analytic_data)
) {
  stop("One or more derived classification totals do not reconcile.", call. = FALSE)
}

analytic_data_file <- file.path(aas_derived_dir, "aas_analytic_encounters.rds")

saveRDS(analytic_data, analytic_data_file)

message("\nFinal encounter-level derivation summary:")
print(
  data.frame(
    measure = c(
      "Eligible encounters",
      "Unique patients",
      "Patients with repeated included encounters",
      "ADD-RS partially missing",
      "ADD-RS fully missing",
      "D-dimer obtained",
      "D-dimer positive",
      "D-dimer negative",
      "D-dimer indeterminate",
      "Algorithm concordant",
      "Algorithm discordant",
      "Algorithm unclassifiable",
      "AAS positive",
      "AAS negative",
      "AAS outcome unknown"
    ),
    n = c(
      nrow(analytic_data),
      length(unique(analytic_data$patient_empi)),
      sum(table(analytic_data$patient_empi) > 1L),
      sum(analytic_data$add_rs_partial_missing),
      sum(analytic_data$add_rs_all_missing),
      sum(analytic_data$d_dimer_obtained),
      sum(analytic_data$d_dimer_status == "Positive"),
      sum(analytic_data$d_dimer_status == "Negative"),
      sum(analytic_data$d_dimer_status == "Indeterminate"),
      sum(analytic_data$algorithm_class == "Concordant"),
      sum(analytic_data$algorithm_class == "Discordant"),
      sum(analytic_data$algorithm_class == "Unclassifiable"),
      sum(analytic_data$aas_outcome == "Positive"),
      sum(analytic_data$aas_outcome == "Negative"),
      sum(analytic_data$aas_outcome == "Unknown")
    ),
    stringsAsFactors = FALSE
  ),
  row.names = FALSE
)

message("\nAlgorithm class by AAS outcome:")
print(table(analytic_data$algorithm_class, analytic_data$aas_outcome))

message("\nDiscordance decomposition:")
print(table(analytic_data$discordance_reason, useNA = "ifany"))

message("\nAlternative-diagnosis categories among known AAS-negative encounters:")
known_negative <- analytic_data$aas_outcome == "Negative"
print(vapply(
  c(
    any_definite = "alternative_diagnosis_definite",
    pulmonary_embolism = "alt_pe",
    pulmonary_infection = "alt_pulmonary_infection",
    pneumothorax_hemopericardium = "alt_ptx_hemopericardium",
    pulmonary_edema = "alt_pulmonary_edema",
    gi_inflammatory = "alt_gi_inflammatory",
    gi_obstruction_perforation = "alt_gi_obstruction_perforation",
    gu_acute = "alt_gu_acute",
    hemorrhage_trauma = "alt_hemorrhage_trauma",
    other_vascular = "alt_other_vascular"
  ),
  function(column) sum(analytic_data[[column]][known_negative]),
  integer(1)
))

message("\nAnalytic encounter file written outside Git:")
message(analytic_data_file)
