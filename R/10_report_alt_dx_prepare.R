# Why this script is here
# I prepare report-analysis packets for eligible AAS-negative encounters.
# Private text and linkage material stay outside the repository.

# =============================================================================
# 10_report_alt_dx_prepare.R
# Exploratory report-based non-AAS imaging-diagnosis analysis
#
# Creates a deidentified, model-bound classification packet for each eligible
# AAS-negative encounter. Clinical text, native identifiers, and the private
# crosswalk remain outside Git. Existing analytic variables are read only.
# =============================================================================

source("R/00_setup.R")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Package 'digest' is required.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
run_id <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("REPORT_ALT_DX_RUN_ID")
if (!nzchar(run_id)) {
  run_id <- paste0("report_alt_dx_v1_", format(Sys.Date(), "%Y%m%d"), "_01")
}
if (!grepl("^[A-Za-z0-9_.-]+$", run_id)) {
  stop("Run identifier contains unsupported characters.", call. = FALSE)
}

# Check the cohort and source rows before building any packets.
analysis_file <- file.path(aas_derived_dir, "aas_analytic_encounters.rds")
source_file <- file.path(aas_derived_dir, "aas_final_cohort_accessions.rds")

if (!file.exists(analysis_file) || !file.exists(source_file)) {
  stop("Required validated R data objects are missing.", call. = FALSE)
}

analytic_data <- readRDS(analysis_file)
source_rows <- readRDS(source_file)

if (nrow(analytic_data) != 1193L || anyDuplicated(analytic_data$encounter)) {
  stop("Validated adult encounter cohort failed the locked 1,193-row check.", call. = FALSE)
}
if (sum(analytic_data$aas_outcome == "Negative") != 1111L) {
  stop("Known AAS-negative cohort no longer contains 1,111 encounters.", call. = FALSE)
}

required_source_fields <- c(
  "Patient Name", "Patient EMPI Nbr", "Encounter", "EEMR Accession Number",
  "Ordering Personnel", "Primary Care Provider", "dictation",
  "Dictation Clinical Indication listed?", "scan_region"
)
missing_fields <- setdiff(required_source_fields, names(source_rows))
if (length(missing_fields) > 0L) {
  stop("Required source fields are missing: ", paste(missing_fields, collapse = ", "), call. = FALSE)
}

# Standardize and screen report text before it leaves the private workspace.
normalize_newlines <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\\\r\\\\n?", "\n", x, perl = TRUE)
  x <- gsub("\\\\n", "\n", x, perl = TRUE)
  gsub("\r\n?", "\n", x, perl = TRUE)
}

redact_dates <- function(x) {
  gsub(
    "(?:[0-9]{1,2}[/.-]){2}[0-9]{2,4}",
    "[DATE REDACTED]",
    x,
    perl = TRUE
  )
}

normalize_component <- function(x) {
  ifelse(x == "Chest", "chest", "abdomen_pelvis")
}

extract_report_text <- function(x) {
  full <- normalize_newlines(x)
  lines <- strsplit(full, "\n", fixed = TRUE)[[1L]]
  header <- grepl(
    "^[[:space:]]*IMPRESSION(?:[[:space:]]*:|[[:space:]]*$)",
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )
  header_i <- which(header)

  if (length(header_i) == 0L) {
    return(list(text = trimws(full), source = "full_report_fallback", n_headers = 0L))
  }

  start_i <- tail(header_i, 1L)
  first_line <- sub(
    "^[[:space:]]*IMPRESSION[[:space:]]*:[[:space:]]*",
    "",
    lines[[start_i]],
    ignore.case = TRUE,
    perl = TRUE
  )
  if (grepl("^[[:space:]]*IMPRESSION[[:space:]]*$", lines[[start_i]], ignore.case = TRUE)) {
    first_line <- ""
  }
  remainder <- if (start_i < length(lines)) lines[(start_i + 1L):length(lines)] else character()
  text <- trimws(paste(c(first_line, remainder), collapse = "\n"))
  list(text = text, source = "impression", n_headers = length(header_i))
}

is_placeholder <- function(x) {
  value <- toupper(trimws(x))
  value %in% c("", "REPORT", "N/A", "NA", "NONE", "NULL", "-", ".", "NOT AVAILABLE")
}

is_low_information <- function(x) {
  value <- trimws(x)
  is_placeholder(value) || !grepl("[[:alpha:]]", value)
}

is_short_text <- function(x) {
  nchar(trimws(x)) < 50L
}

# Limit the packets to encounters with a known negative AAS outcome.
negative_encounters <- as.character(
  analytic_data$encounter[analytic_data$aas_outcome == "Negative"]
)
source_rows$encounter_key_internal <- as.character(source_rows[["Encounter"]])
eligible_rows <- source_rows[source_rows$encounter_key_internal %in% negative_encounters, , drop = FALSE]

if (nrow(eligible_rows) != 2223L) {
  stop("Eligible source-row count no longer equals the audited 2,223 rows.", call. = FALSE)
}

# Preflight only: fail closed if obvious direct identifiers occur in model-bound
# text. No matching text or identifier is printed.
phi_hits <- logical(nrow(eligible_rows))
date_hits <- logical(nrow(eligible_rows))
for (i in seq_len(nrow(eligible_rows))) {
  combined <- paste(
    normalize_newlines(eligible_rows[["dictation"]][i]),
    as.character(eligible_rows[["Dictation Clinical Indication listed?"]][i]),
    sep = "\n"
  )
  direct_values <- unique(trimws(as.character(c(
    eligible_rows[["Patient Name"]][i],
    eligible_rows[["Patient EMPI Nbr"]][i],
    eligible_rows[["Encounter"]][i],
    eligible_rows[["EEMR Accession Number"]][i],
    eligible_rows[["Ordering Personnel"]][i],
    eligible_rows[["Primary Care Provider"]][i]
  ))))
  direct_values <- direct_values[!is.na(direct_values) & nchar(direct_values) >= 4L]
  if (length(direct_values) > 0L) {
    phi_hits[i] <- any(vapply(direct_values, function(v) grepl(v, combined, fixed = TRUE), logical(1)))
  }
  date_hits[i] <- grepl(
    "(?:[0-9]{1,2}[/.-]){2}[0-9]{2,4}",
    combined,
    perl = TRUE
  )
}
if (any(phi_hits)) {
  stop(
    paste0(
      "Model-bound text failed the direct-identifier preflight (direct-identifier rows: ",
      sum(phi_hits),
      "). No run directory or classification inputs were written."
    ),
    call. = FALSE
  )
}

# Create a separate run directory so an existing run is not overwritten.
private_root <- file.path(aas_derived_dir, "report_alt_dx")
run_dir <- file.path(private_root, run_id)
if (dir.exists(run_dir)) {
  stop("Refusing to overwrite an existing report-analysis run: ", run_dir, call. = FALSE)
}

dir.create(file.path(run_dir, "inputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "pass1"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "pass2"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "validation"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(run_dir, "clinician_audit"), recursive = TRUE, showWarnings = FALSE)

# A private salt prevents the pseudonymous audit label from being a direct hash
# of the native encounter identifier. Both the salt and crosswalk remain outside Git.
set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))
pseudonym_salt <- digest::digest(
  paste(run_id, format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6%z"), runif(1L)),
  algo = "sha256",
  serialize = FALSE
)
encounter_hash <- vapply(
  negative_encounters,
  function(x) digest::digest(paste0(pseudonym_salt, "|", x), algo = "sha256", serialize = FALSE),
  character(1)
)
ordered_encounters <- negative_encounters[order(encounter_hash)]
crosswalk <- data.frame(
  encounter = ordered_encounters,
  report_audit_id = sprintf("RDX-%04d", seq_along(ordered_encounters)),
  stringsAsFactors = FALSE
)
saveRDS(crosswalk, file.path(run_dir, "private_encounter_crosswalk.rds"))
writeLines(pseudonym_salt, file.path(run_dir, "private_pseudonym_salt.txt"), useBytes = TRUE)

audit_id_for <- setNames(crosswalk$report_audit_id, crosswalk$encounter)

# Group source reports by encounter and build the pseudonymous packets.
grouped_rows <- split(seq_len(nrow(eligible_rows)), eligible_rows$encounter_key_internal)
packet_paths <- character(length(ordered_encounters))
report_map <- vector("list", 0L)
source_audit_rows <- vector("list", length(ordered_encounters))

for (j in seq_along(ordered_encounters)) {
  encounter_value <- ordered_encounters[[j]]
  audit_id <- audit_id_for[[encounter_value]]
  i <- grouped_rows[[encounter_value]]
  encounter_source <- eligible_rows[i, , drop = FALSE]

  extracted <- lapply(encounter_source[["dictation"]], extract_report_text)
  report_work <- data.frame(
    row_index = seq_len(nrow(encounter_source)),
    component = normalize_component(encounter_source$scan_region),
    report_text = vapply(extracted, `[[`, character(1), "text"),
    text_source = vapply(extracted, `[[`, character(1), "source"),
    n_impression_headers = vapply(extracted, `[[`, integer(1), "n_headers"),
    full_report = normalize_newlines(encounter_source[["dictation"]]),
    stringsAsFactors = FALSE
  )
  report_work$low_information <- vapply(report_work$report_text, is_low_information, logical(1))
  report_work$short_text <- vapply(report_work$report_text, is_short_text, logical(1))
  needs_full_report_fallback <-
    report_work$low_information &
    !vapply(report_work$full_report, is_low_information, logical(1))
  report_work$report_text[needs_full_report_fallback] <-
    report_work$full_report[needs_full_report_fallback]
  report_work$text_source[needs_full_report_fallback] <- "full_report_fallback"
  report_work$low_information <- vapply(report_work$report_text, is_low_information, logical(1))
  report_work$short_text <- vapply(report_work$report_text, is_short_text, logical(1))

  usable <- report_work[!report_work$low_information, , drop = FALSE]
  if (nrow(usable) == 0L) {
    stop("At least one eligible encounter has no usable report text.", call. = FALSE)
  }

  text_hash <- vapply(
    usable$report_text,
    digest::digest,
    character(1),
    algo = "sha256",
    serialize = FALSE
  )
  text_groups <- split(seq_len(nrow(usable)), text_hash)
  text_groups <- text_groups[order(names(text_groups))]

  reports <- vector("list", length(text_groups))
  for (k in seq_along(text_groups)) {
    g <- text_groups[[k]]
    components <- sort(unique(usable$component[g]))
    component <- if (length(components) > 1L) "both" else components[[1L]]
    source_type <- if (any(usable$text_source[g] == "impression")) "impression" else "full_report_fallback"
    report_key <- sprintf("SR-%02d", k)
    reports[[k]] <- list(
      source_report_key = report_key,
      component = component,
      text_source = source_type,
      report_text = redact_dates(usable$report_text[g[[1L]]])
    )
    report_map[[length(report_map) + 1L]] <- data.frame(
      report_audit_id = audit_id,
      source_report_key = report_key,
      supplied_report_text = redact_dates(usable$report_text[g[[1L]]]),
      original_full_report = usable$full_report[g[[1L]]],
      stringsAsFactors = FALSE
    )
  }

  indication_work <- data.frame(
    component = normalize_component(encounter_source$scan_region),
    indication_text = trimws(as.character(encounter_source[["Dictation Clinical Indication listed?"]])),
    stringsAsFactors = FALSE
  )
  indication_work <- indication_work[nzchar(indication_work$indication_text), , drop = FALSE]
  if (nrow(indication_work) == 0L) {
    stop("At least one eligible encounter has no documented clinical indication.", call. = FALSE)
  }
  indication_groups <- split(
    seq_len(nrow(indication_work)),
    vapply(indication_work$indication_text, digest::digest, character(1), algo = "sha256", serialize = FALSE)
  )
  indication_groups <- indication_groups[order(names(indication_groups))]
  indications <- lapply(indication_groups, function(g) {
    components <- sort(unique(indication_work$component[g]))
    list(
      component = if (length(components) > 1L) "both" else components[[1L]],
      indication_text = redact_dates(indication_work$indication_text[g[[1L]]])
    )
  })

  packet <- list(
    report_audit_id = audit_id,
    reports = reports,
    clinical_indications = indications
  )

  if (!identical(names(packet), c("report_audit_id", "reports", "clinical_indications"))) {
    stop("Model-bound packet allowlist failed.", call. = FALSE)
  }
  if (!all(vapply(reports, function(x) identical(names(x), c(
    "source_report_key", "component", "text_source", "report_text"
  )), logical(1)))) {
    stop("Model-bound report allowlist failed.", call. = FALSE)
  }
  if (!all(vapply(indications, function(x) identical(names(x), c(
    "component", "indication_text"
  )), logical(1)))) {
    stop("Model-bound indication allowlist failed.", call. = FALSE)
  }

  packet_path <- file.path(run_dir, "inputs", paste0(audit_id, ".json"))
  jsonlite::write_json(packet, packet_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  packet_paths[[j]] <- packet_path

  source_audit_rows[[j]] <- data.frame(
    report_audit_id = audit_id,
    n_component_rows = nrow(report_work),
    n_unique_usable_reports = length(reports),
    n_unique_indications = length(indications),
    any_impression = any(report_work$text_source == "impression" & !report_work$low_information),
    any_full_report_fallback = any(report_work$text_source == "full_report_fallback" & !report_work$low_information),
    any_low_information_component = any(report_work$low_information),
    any_short_text_component = any(report_work$short_text),
    any_multiple_impression_headers = any(report_work$n_impression_headers > 1L),
    stringsAsFactors = FALSE
  )
}

report_map <- do.call(rbind, report_map)
source_audit_private <- do.call(rbind, source_audit_rows)
saveRDS(report_map, file.path(run_dir, "private_report_evidence_map.rds"))
saveRDS(source_audit_private, file.path(run_dir, "private_source_audit_by_encounter.rds"))

# Summarize the source coverage without exporting encounter-level details.
aggregate_source_audit <- data.frame(
  metric = c(
    "Primary adult encounters",
    "Known AAS-negative eligible encounters",
    "Eligible CTA component rows",
    "Eligible encounters with at least one extracted impression",
    "Eligible encounters requiring any full-report fallback",
    "Eligible encounters with more than one unique report",
    "Eligible encounters with more than one unique indication",
    "Eligible encounters with any low-information component row",
    "Eligible encounters with any report text shorter than 50 characters",
    "Eligible encounters with multiple IMPRESSION headers",
    "Eligible encounters with unusable model input",
    "Direct-identifier preflight hits",
    "Source rows with date-like text redacted before classification"
  ),
  n = c(
    nrow(analytic_data),
    length(ordered_encounters),
    nrow(eligible_rows),
    sum(source_audit_private$any_impression),
    sum(source_audit_private$any_full_report_fallback),
    sum(source_audit_private$n_unique_usable_reports > 1L),
    sum(source_audit_private$n_unique_indications > 1L),
    sum(source_audit_private$any_low_information_component),
    sum(source_audit_private$any_short_text_component),
    sum(source_audit_private$any_multiple_impression_headers),
    0L,
    sum(phi_hits),
    sum(date_hits)
  ),
  denominator = c(
    nrow(analytic_data),
    nrow(analytic_data),
    nrow(eligible_rows),
    rep(length(ordered_encounters), 8L),
    nrow(eligible_rows),
    nrow(eligible_rows)
  ),
  stringsAsFactors = FALSE
)
aggregate_source_audit$percent <- 100 * aggregate_source_audit$n / aggregate_source_audit$denominator
aggregate_source_audit$generating_script <- "R/10_report_alt_dx_prepare.R"
aggregate_source_audit$status <- "Aggregate source audit; no model classifications"

dir.create("results/report_alt_dx", recursive = TRUE, showWarnings = FALSE)
write.csv(
  aggregate_source_audit,
  "results/report_alt_dx/source_audit.csv",
  row.names = FALSE,
  na = ""
)

# Record the run inputs and packet counts for later checks.
manifest <- list(
  run_id = run_id,
  analysis_id = "report_alt_dx_v1",
  classification_status = "exploratory_secondary",
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  analytic_file = normalizePath(analysis_file),
  analytic_file_sha256 = digest::digest(file = analysis_file, algo = "sha256"),
  source_file = normalizePath(source_file),
  source_file_sha256 = digest::digest(file = source_file, algo = "sha256"),
  eligible_encounters = length(ordered_encounters),
  packet_count = length(packet_paths),
  model_bound_top_level_allowlist = c("report_audit_id", "reports", "clinical_indications"),
  prohibited_fields_excluded = c(
    "provider type", "provider identity", "site", "algorithm concordance",
    "incidental findings", "historical regex result", "manuscript result",
    "patient name", "EMPI", "native encounter", "accession"
  )
)
jsonlite::write_json(
  manifest,
  file.path(run_dir, "source_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)
writeLines(run_dir, file.path(private_root, "current_run_path.txt"), useBytes = TRUE)

message("Report-analysis source preparation completed.")
message("Eligible known AAS-negative encounters: ", length(ordered_encounters))
message("Model-bound input packets: ", length(packet_paths))
message("Direct-identifier preflight hits: 0")
message("Date-like source rows redacted before classification: ", sum(date_hits))
message("Clinical text and native-ID crosswalk retained outside Git.")
