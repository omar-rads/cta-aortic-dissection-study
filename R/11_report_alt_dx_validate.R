# Why this script is here
# I check both classification passes and their evidence against the preserved endpoint rules.
# This verification does not establish independent clinical validation.

# =============================================================================
# 11_report_alt_dx_validate.R
# Validate two independent report-classification passes, verify exact evidence,
# derive the frozen endpoint, and construct the low-burden clinician audit set.
# Patient-level outputs remain outside Git; only aggregate tables are exported.
# =============================================================================

source("R/00_setup.R")

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
run_id <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("REPORT_ALT_DX_RUN_ID")
if (!nzchar(run_id)) {
  pointer <- file.path(aas_derived_dir, "report_alt_dx", "current_run_path.txt")
  if (!file.exists(pointer)) stop("No report-analysis run was specified.", call. = FALSE)
  run_dir <- trimws(readLines(pointer, warn = FALSE)[1L])
  run_id <- basename(run_dir)
} else {
  run_dir <- file.path(aas_derived_dir, "report_alt_dx", run_id)
}
if (!dir.exists(run_dir)) stop("Report-analysis run directory was not found.", call. = FALSE)

# Load the frozen rules and confirm that every expected packet is present.
config <- jsonlite::fromJSON("config/report_alt_dx_v1.json", simplifyVector = TRUE)
input_paths <- sort(list.files(file.path(run_dir, "inputs"), pattern = "^RDX-.*\\.json$", full.names = TRUE))
if (length(input_paths) != 1111L) stop("Expected 1,111 model input packets.", call. = FALSE)

packets <- lapply(input_paths, jsonlite::fromJSON, simplifyVector = FALSE)
packet_ids <- vapply(packets, `[[`, character(1), "report_audit_id")
if (anyDuplicated(packet_ids)) stop("Input packets contain duplicated audit labels.", call. = FALSE)
packet_by_id <- setNames(packets, packet_ids)

report_map <- readRDS(file.path(run_dir, "private_report_evidence_map.rds"))
if (anyDuplicated(report_map[c("report_audit_id", "source_report_key")])) {
  stop("Private report-evidence map has duplicate keys.", call. = FALSE)
}

# Read each pass and check that its audit labels match the input packets.
read_pass <- function(pass_number) {
  pass_dir <- file.path(run_dir, paste0("pass", pass_number))
  manifest_path <- file.path(pass_dir, "pass_manifest.json")
  output_paths <- sort(list.files(pass_dir, pattern = "^batch_[0-9]+\\.json$", full.names = TRUE))
  if (!file.exists(manifest_path) || length(output_paths) == 0L) {
    stop("Classifier pass ", pass_number, " is incomplete.", call. = FALSE)
  }
  output_objects <- lapply(output_paths, jsonlite::fromJSON, simplifyVector = FALSE)
  classifications <- unlist(lapply(output_objects, `[[`, "classifications"), recursive = FALSE)
  ids <- vapply(classifications, `[[`, character(1), "report_audit_id")
  if (length(ids) != 1111L || anyDuplicated(ids) || !setequal(ids, packet_ids)) {
    stop("Classifier pass ", pass_number, " failed exact coverage/uniqueness.", call. = FALSE)
  }
  setNames(classifications, ids)
}

pass1 <- read_pass(1L)
pass2 <- read_pass(2L)

# Flatten the candidate findings so their evidence can be checked consistently.
candidate_fields <- c(
  "normalized_diagnosis", "diagnostic_category", "cta_component",
  "finding_status", "diagnostic_certainty", "acuity",
  "plausibly_explains_indication", "report_evidence_quote",
  "indication_evidence_quote", "source_report_keys"
)

flatten_candidates <- function(classifications, pass_number) {
  rows <- vector("list", 0L)
  for (audit_id in names(classifications)) {
    candidates <- classifications[[audit_id]]$candidates
    if (length(candidates) == 0L) next
    for (j in seq_along(candidates)) {
      candidate <- candidates[[j]]
      if (!identical(sort(names(candidate)), sort(candidate_fields))) {
        stop("Classifier candidate fields do not match the frozen schema.", call. = FALSE)
      }
      source_keys <- unlist(candidate$source_report_keys, use.names = FALSE)
      rows[[length(rows) + 1L]] <- data.frame(
        report_audit_id = audit_id,
        pass = pass_number,
        candidate_number = j,
        normalized_diagnosis = as.character(candidate$normalized_diagnosis),
        diagnostic_category = as.character(candidate$diagnostic_category),
        cta_component = as.character(candidate$cta_component),
        finding_status = as.character(candidate$finding_status),
        diagnostic_certainty = as.character(candidate$diagnostic_certainty),
        acuity = as.character(candidate$acuity),
        plausibly_explains_indication = as.character(candidate$plausibly_explains_indication),
        report_evidence_quote = as.character(candidate$report_evidence_quote),
        indication_evidence_quote = as.character(candidate$indication_evidence_quote),
        source_report_keys = paste(source_keys, collapse = "\u001f"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(
      report_audit_id = character(), pass = integer(), candidate_number = integer(),
      normalized_diagnosis = character(), diagnostic_category = character(),
      cta_component = character(), finding_status = character(),
      diagnostic_certainty = character(), acuity = character(),
      plausibly_explains_indication = character(), report_evidence_quote = character(),
      indication_evidence_quote = character(), source_report_keys = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

candidates <- rbind(flatten_candidates(pass1, 1L), flatten_candidates(pass2, 2L))

# Match required evidence to the preserved source text using exact matching.
fixed_contains <- function(text, quote) {
  nzchar(quote) && grepl(quote, text, fixed = TRUE)
}

if (nrow(candidates) > 0L) {
  candidates$report_key_valid <- FALSE
  candidates$report_quote_in_supplied_text <- FALSE
  candidates$report_quote_in_original_text <- FALSE
  candidates$indication_quote_valid <- FALSE

  for (i in seq_len(nrow(candidates))) {
    audit_id <- candidates$report_audit_id[[i]]
    packet <- packet_by_id[[audit_id]]
    keys <- strsplit(candidates$source_report_keys[[i]], "\u001f", fixed = TRUE)[[1L]]
    keys <- keys[nzchar(keys)]
    packet_keys <- vapply(packet$reports, `[[`, character(1), "source_report_key")
    candidates$report_key_valid[[i]] <- length(keys) > 0L && all(keys %in% packet_keys)

    supplied_reports <- packet$reports[packet_keys %in% keys]
    candidates$report_quote_in_supplied_text[[i]] <-
      candidates$report_key_valid[[i]] &&
      any(vapply(
        supplied_reports,
        function(x) fixed_contains(x$report_text, candidates$report_evidence_quote[[i]]),
        logical(1)
      ))

    original_rows <- report_map[
      report_map$report_audit_id == audit_id & report_map$source_report_key %in% keys,
      ,
      drop = FALSE
    ]
    candidates$report_quote_in_original_text[[i]] <-
      nrow(original_rows) > 0L &&
      any(vapply(
        original_rows$original_full_report,
        fixed_contains,
        logical(1),
        quote = candidates$report_evidence_quote[[i]]
      ))

    if (candidates$plausibly_explains_indication[[i]] == "no") {
      candidates$indication_quote_valid[[i]] <- TRUE
    } else {
      indication_text <- vapply(packet$clinical_indications, `[[`, character(1), "indication_text")
      candidates$indication_quote_valid[[i]] <-
        nzchar(candidates$indication_evidence_quote[[i]]) &&
        any(vapply(
          indication_text,
          fixed_contains,
          logical(1),
          quote = candidates$indication_evidence_quote[[i]]
        ))
    }
  }

  candidates$evidence_valid <-
    nzchar(candidates$report_evidence_quote) &
    candidates$report_key_valid &
    candidates$report_quote_in_supplied_text &
    candidates$report_quote_in_original_text &
    candidates$indication_quote_valid

  candidates$endpoint_qualifying <-
    candidates$evidence_valid &
    candidates$finding_status == "present" &
    candidates$diagnostic_certainty %in% c("definite", "probable_likely") &
    candidates$plausibly_explains_indication == "yes" &
    candidates$acuity %in% unlist(config$accepted_endpoint_acuity)

  candidates$endpoint_uncertain_signal <-
    candidates$plausibly_explains_indication %in% c("yes", "uncertain") &
    candidates$acuity != "chronic" &
    (
      !candidates$evidence_valid |
      candidates$finding_status == "uncertain" |
      candidates$diagnostic_certainty == "possible_indeterminate" |
      candidates$plausibly_explains_indication == "uncertain"
    )
} else {
  candidates$evidence_valid <- logical()
  candidates$endpoint_qualifying <- logical()
  candidates$endpoint_uncertain_signal <- logical()
}

# Derive each encounter endpoint and compare the two passes.
endpoint_for <- function(audit_id, pass_number) {
  rows <- candidates[candidates$report_audit_id == audit_id & candidates$pass == pass_number, , drop = FALSE]
  if (nrow(rows) > 0L && any(rows$endpoint_qualifying)) return("positive")
  if (nrow(rows) > 0L && any(rows$endpoint_uncertain_signal)) return("uncertain")
  "negative"
}

value_signature <- function(rows, field, normalize = FALSE) {
  if (nrow(rows) == 0L) return("")
  values <- as.character(rows[[field]])
  if (normalize) values <- tolower(trimws(gsub("[[:space:]]+", " ", values)))
  paste(sort(unique(values)), collapse = " | ")
}

candidate_signature <- function(rows) {
  if (nrow(rows) == 0L) return("")
  tuples <- apply(
    rows[c(
      "normalized_diagnosis", "diagnostic_category", "cta_component", "finding_status",
      "diagnostic_certainty", "acuity", "plausibly_explains_indication"
    )],
    1L,
    function(x) paste(tolower(trimws(x)), collapse = "\u001e")
  )
  paste(sort(unique(tuples)), collapse = "\u001d")
}

encounter_validation <- data.frame(report_audit_id = packet_ids, stringsAsFactors = FALSE)
for (pass_number in 1:2) {
  prefix <- paste0("pass", pass_number, "_")
  endpoint <- character(length(packet_ids))
  diagnosis <- category <- status <- certainty <- acuity <- plausibility <- complete <- character(length(packet_ids))
  invalid_n <- integer(length(packet_ids))
  qualifying_category <- character(length(packet_ids))
  for (i in seq_along(packet_ids)) {
    audit_id <- packet_ids[[i]]
    rows <- candidates[candidates$report_audit_id == audit_id & candidates$pass == pass_number, , drop = FALSE]
    endpoint[[i]] <- endpoint_for(audit_id, pass_number)
    diagnosis[[i]] <- value_signature(rows, "normalized_diagnosis", normalize = TRUE)
    category[[i]] <- value_signature(rows, "diagnostic_category")
    status[[i]] <- value_signature(rows, "finding_status")
    certainty[[i]] <- value_signature(rows, "diagnostic_certainty")
    acuity[[i]] <- value_signature(rows, "acuity")
    plausibility[[i]] <- value_signature(rows, "plausibly_explains_indication")
    complete[[i]] <- candidate_signature(rows)
    invalid_n[[i]] <- if (nrow(rows) == 0L) 0L else sum(!rows$evidence_valid)
    qualifying_category[[i]] <- value_signature(rows[rows$endpoint_qualifying, , drop = FALSE], "diagnostic_category")
  }
  encounter_validation[[paste0(prefix, "endpoint")]] <- endpoint
  encounter_validation[[paste0(prefix, "diagnosis_signature")]] <- diagnosis
  encounter_validation[[paste0(prefix, "category_signature")]] <- category
  encounter_validation[[paste0(prefix, "status_signature")]] <- status
  encounter_validation[[paste0(prefix, "certainty_signature")]] <- certainty
  encounter_validation[[paste0(prefix, "acuity_signature")]] <- acuity
  encounter_validation[[paste0(prefix, "plausibility_signature")]] <- plausibility
  encounter_validation[[paste0(prefix, "complete_signature")]] <- complete
  encounter_validation[[paste0(prefix, "invalid_evidence_n")]] <- invalid_n
  encounter_validation[[paste0(prefix, "qualifying_category")]] <- qualifying_category
}

encounter_validation$endpoint_agreement <-
  encounter_validation$pass1_endpoint == encounter_validation$pass2_endpoint
encounter_validation$diagnosis_agreement <-
  encounter_validation$pass1_diagnosis_signature == encounter_validation$pass2_diagnosis_signature
encounter_validation$category_agreement <-
  encounter_validation$pass1_category_signature == encounter_validation$pass2_category_signature
encounter_validation$status_agreement <-
  encounter_validation$pass1_status_signature == encounter_validation$pass2_status_signature
encounter_validation$certainty_agreement <-
  encounter_validation$pass1_certainty_signature == encounter_validation$pass2_certainty_signature
encounter_validation$acuity_agreement <-
  encounter_validation$pass1_acuity_signature == encounter_validation$pass2_acuity_signature
encounter_validation$plausibility_agreement <-
  encounter_validation$pass1_plausibility_signature == encounter_validation$pass2_plausibility_signature
encounter_validation$complete_agreement <-
  encounter_validation$pass1_complete_signature == encounter_validation$pass2_complete_signature
encounter_validation$any_invalid_evidence <-
  encounter_validation$pass1_invalid_evidence_n > 0L |
  encounter_validation$pass2_invalid_evidence_n > 0L
encounter_validation$any_uncertain_endpoint <-
  encounter_validation$pass1_endpoint == "uncertain" |
  encounter_validation$pass2_endpoint == "uncertain"
encounter_validation$clinically_meaningful_disagreement <-
  !encounter_validation$endpoint_agreement
encounter_validation$mandatory_clinician_review <-
  !encounter_validation$endpoint_agreement |
  encounter_validation$any_uncertain_endpoint |
  encounter_validation$any_invalid_evidence

# Summarize agreement and endpoint transitions before clinician review.
agreement_metrics <- c(
  endpoint = "endpoint_agreement",
  normalized_diagnosis_set = "diagnosis_agreement",
  diagnostic_category_set = "category_agreement",
  finding_status_set = "status_agreement",
  diagnostic_certainty_set = "certainty_agreement",
  acuity_set = "acuity_agreement",
  plausibly_explanatory_set = "plausibility_agreement",
  complete_structured_classification = "complete_agreement"
)
agreement_statistics <- data.frame(
  agreement_dimension = names(agreement_metrics),
  n_agree = vapply(agreement_metrics, function(x) sum(encounter_validation[[x]]), integer(1)),
  denominator = nrow(encounter_validation),
  stringsAsFactors = FALSE
)
agreement_statistics$percent_agree <- 100 * agreement_statistics$n_agree / agreement_statistics$denominator
agreement_statistics$interpretation <- "Two-pass reproducibility only; not clinical validation"
agreement_statistics$generating_script <- "R/11_report_alt_dx_validate.R"

endpoint_levels <- c("positive", "negative", "uncertain")
pre_clinician_counts <- do.call(rbind, lapply(1:2, function(pass_number) {
  values <- factor(encounter_validation[[paste0("pass", pass_number, "_endpoint")]], levels = endpoint_levels)
  counts <- table(values)
  data.frame(
    pass = paste0("Pass ", pass_number),
    endpoint = endpoint_levels,
    n = as.integer(counts),
    denominator = nrow(encounter_validation),
    percent = 100 * as.integer(counts) / nrow(encounter_validation),
    status = "Pre-clinician classifier output; not final report-based prevalence",
    generating_script = "R/11_report_alt_dx_validate.R",
    stringsAsFactors = FALSE
  )
}))

endpoint_transition <- as.data.frame(
  table(
    pass_1 = factor(encounter_validation$pass1_endpoint, levels = endpoint_levels),
    pass_2 = factor(encounter_validation$pass2_endpoint, levels = endpoint_levels)
  ),
  stringsAsFactors = FALSE
)
names(endpoint_transition)[3L] <- "n"
endpoint_transition$denominator <- nrow(encounter_validation)
endpoint_transition$generating_script <- "R/11_report_alt_dx_validate.R"

mandatory_n <- sum(encounter_validation$mandatory_clinician_review)
minimum_random <- as.integer(config$audit_minimum_random_agreements)
audit_flow <- data.frame(
  metric = c(
    "Eligible known AAS-negative encounters",
    "Endpoint-disagreement encounters",
    "Encounters with uncertain endpoint in either pass",
    "Encounters with invalid evidence in either pass",
    "Distinct mandatory clinician-review encounters",
    "Minimum random agreement cases required",
    "Mandatory plus minimum random agreement cases"
  ),
  n = c(
    nrow(encounter_validation),
    sum(!encounter_validation$endpoint_agreement),
    sum(encounter_validation$any_uncertain_endpoint),
    sum(encounter_validation$any_invalid_evidence),
    mandatory_n,
    minimum_random,
    mandatory_n + minimum_random
  ),
  denominator = nrow(encounter_validation),
  stringsAsFactors = FALSE
)
audit_flow$percent <- 100 * audit_flow$n / audit_flow$denominator
audit_flow$generating_script <- "R/11_report_alt_dx_validate.R"

dir.create("results/report_alt_dx", recursive = TRUE, showWarnings = FALSE)
write.csv(agreement_statistics, "results/report_alt_dx/agreement_statistics.csv", row.names = FALSE, na = "")
write.csv(pre_clinician_counts, "results/report_alt_dx/pre_clinician_endpoint_counts.csv", row.names = FALSE, na = "")
write.csv(endpoint_transition, "results/report_alt_dx/endpoint_transition.csv", row.names = FALSE, na = "")
write.csv(audit_flow, "results/report_alt_dx/clinician_audit_flow.csv", row.names = FALSE, na = "")

# Describe uncertainty by reason and category across the two passes.
uncertainty_reason_rows <- vector("list", 0L)
uncertainty_category_rows <- vector("list", 0L)
for (pass_number in 1:2) {
  endpoint_field <- paste0("pass", pass_number, "_endpoint")
  uncertain_ids <- encounter_validation$report_audit_id[
    encounter_validation[[endpoint_field]] == "uncertain"
  ]
  pass_candidates <- candidates[
    candidates$pass == pass_number & candidates$report_audit_id %in% uncertain_ids,
    ,
    drop = FALSE
  ]
  reason_flags <- list(
    "Finding status uncertain" = pass_candidates$finding_status == "uncertain",
    "Diagnostic certainty possible/indeterminate" = pass_candidates$diagnostic_certainty == "possible_indeterminate",
    "Plausibly explanatory uncertain" = pass_candidates$plausibly_explains_indication == "uncertain",
    "Invalid required evidence" = !pass_candidates$evidence_valid
  )
  for (reason_name in names(reason_flags)) {
    reason_ids <- unique(pass_candidates$report_audit_id[reason_flags[[reason_name]]])
    uncertainty_reason_rows[[length(uncertainty_reason_rows) + 1L]] <- data.frame(
      pass = paste0("Pass ", pass_number),
      uncertainty_mechanism = reason_name,
      n_encounters = length(reason_ids),
      uncertain_endpoint_denominator = length(uncertain_ids),
      percent = if (length(uncertain_ids) > 0L) 100 * length(reason_ids) / length(uncertain_ids) else NA_real_,
      mutually_exclusive = FALSE,
      generating_script = "R/11_report_alt_dx_validate.R",
      stringsAsFactors = FALSE
    )
  }

  signal_candidates <- pass_candidates[pass_candidates$endpoint_uncertain_signal, , drop = FALSE]
  if (nrow(signal_candidates) > 0L) {
    unique_pairs <- unique(signal_candidates[c("report_audit_id", "diagnostic_category")])
    category_counts <- table(unique_pairs$diagnostic_category)
    uncertainty_category_rows[[length(uncertainty_category_rows) + 1L]] <- data.frame(
      pass = paste0("Pass ", pass_number),
      diagnostic_category = names(category_counts),
      n_encounters = as.integer(category_counts),
      uncertain_endpoint_denominator = length(uncertain_ids),
      percent = 100 * as.integer(category_counts) / length(uncertain_ids),
      mutually_exclusive = FALSE,
      generating_script = "R/11_report_alt_dx_validate.R",
      stringsAsFactors = FALSE
    )
  }
}
uncertainty_reasons <- do.call(rbind, uncertainty_reason_rows)
uncertainty_categories <- do.call(rbind, uncertainty_category_rows)
write.csv(
  uncertainty_reasons,
  "results/report_alt_dx/uncertainty_mechanisms.csv",
  row.names = FALSE,
  na = ""
)
write.csv(
  uncertainty_categories,
  "results/report_alt_dx/uncertainty_categories.csv",
  row.names = FALSE,
  na = ""
)

saveRDS(candidates, file.path(run_dir, "validation", "private_candidate_validation.rds"))
saveRDS(encounter_validation, file.path(run_dir, "validation", "private_encounter_validation.rds"))

# Make sure the mandatory reviews and random agreement sample fit the audit limits.
maximum_total <- as.integer(config$audit_maximum_total)
if (mandatory_n + minimum_random > maximum_total) {
  writeLines(
    paste0(
      "Mandatory review plus the minimum agreement sample is ", mandatory_n + minimum_random,
      ", exceeding the frozen low-burden maximum of ", maximum_total, "."
    ),
    file.path(run_dir, "clinician_audit", "REQUIRES_AUDIT_BURDEN_DECISION.txt")
  )
  stop(
    "The mandatory clinician-review burden exceeds the frozen 80-case maximum. Aggregate flow was saved; no larger workbook was constructed.",
    call. = FALSE
  )
}

mandatory_ids <- encounter_validation$report_audit_id[encounter_validation$mandatory_clinician_review]
agreement_pool <- encounter_validation[
  !encounter_validation$mandatory_clinician_review & encounter_validation$endpoint_agreement,
  ,
  drop = FALSE
]
agreement_pool$sample_stratum <- ifelse(
  agreement_pool$pass1_endpoint == "positive",
  paste0("positive|", ifelse(nzchar(agreement_pool$pass1_qualifying_category), agreement_pool$pass1_qualifying_category, "other")),
  paste0(agreement_pool$pass1_endpoint, "|none")
)

target_total <- max(as.integer(config$audit_target_total), mandatory_n + minimum_random)
agreement_needed <- target_total - mandatory_n
if (agreement_needed > nrow(agreement_pool)) {
  stop("Insufficient agreement cases for the frozen audit sample.", call. = FALSE)
}

set.seed(as.integer(config$audit_sampling_seed))
agreement_pool$random_order <- runif(nrow(agreement_pool))
by_stratum <- split(agreement_pool, agreement_pool$sample_stratum)
by_stratum <- lapply(by_stratum, function(x) x[order(x$random_order), , drop = FALSE])
stratum_names <- sort(names(by_stratum))
selected_agreement <- character()
position <- setNames(rep(1L, length(stratum_names)), stratum_names)
while (length(selected_agreement) < agreement_needed) {
  added <- FALSE
  for (stratum in stratum_names) {
    pos <- position[[stratum]]
    if (pos <= nrow(by_stratum[[stratum]])) {
      selected_agreement <- c(selected_agreement, by_stratum[[stratum]]$report_audit_id[[pos]])
      position[[stratum]] <- pos + 1L
      added <- TRUE
      if (length(selected_agreement) >= agreement_needed) break
    }
  }
  if (!added) break
}
if (length(selected_agreement) != agreement_needed) {
  stop("Stratified agreement sampling did not reach the frozen target.", call. = FALSE)
}

# Assemble the private clinician review set and its supporting details.
audit_ids <- c(mandatory_ids, selected_agreement)
audit_index <- match(audit_ids, encounter_validation$report_audit_id)
audit_summary <- encounter_validation[audit_index, , drop = FALSE]
audit_summary$sample_reason <- ifelse(
  !audit_summary$endpoint_agreement,
  "endpoint_disagreement",
  ifelse(
    audit_summary$any_uncertain_endpoint,
    "uncertain_endpoint",
    ifelse(audit_summary$any_invalid_evidence, "invalid_evidence", "stratified_random_agreement")
  )
)
audit_summary$sample_order <- seq_len(nrow(audit_summary))

classification_to_list <- function(classification) {
  list(candidates = classification$candidates)
}

review_rows <- vector("list", length(audit_ids))
for (i in seq_along(audit_ids)) {
  audit_id <- audit_ids[[i]]
  packet <- packet_by_id[[audit_id]]
  reports <- lapply(packet$reports, function(x) {
    list(
      source_report_key = x$source_report_key,
      component = x$component,
      text_source = x$text_source,
      report_text = x$report_text
    )
  })
  indications <- lapply(packet$clinical_indications, function(x) {
    list(component = x$component, indication_text = x$indication_text)
  })
  validation_row <- audit_summary[audit_summary$report_audit_id == audit_id, , drop = FALSE]
  review_rows[[i]] <- list(
    report_audit_id = audit_id,
    sample_reason = validation_row$sample_reason[[1L]],
    reports = reports,
    clinical_indications = indications,
    pass_1_endpoint = validation_row$pass1_endpoint[[1L]],
    pass_2_endpoint = validation_row$pass2_endpoint[[1L]],
    pass_1 = classification_to_list(pass1[[audit_id]]),
    pass_2 = classification_to_list(pass2[[audit_id]])
  )
}

jsonlite::write_json(
  list(
    run_id = run_id,
    endpoint_rule_version = config$endpoint_rule_version,
    sampling_seed = config$audit_sampling_seed,
    n_total = length(review_rows),
    n_mandatory = mandatory_n,
    n_random_agreement = length(selected_agreement),
    review_rows = review_rows
  ),
  file.path(run_dir, "clinician_audit", "clinician_audit_source.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
saveRDS(audit_summary, file.path(run_dir, "clinician_audit", "private_audit_sample_metadata.rds"))

message("Two-pass validation completed with aggregate-only terminal output.")
message("Eligible encounters: ", nrow(encounter_validation))
message("Endpoint agreement: ", sum(encounter_validation$endpoint_agreement), "/", nrow(encounter_validation))
message("Invalid-evidence encounters: ", sum(encounter_validation$any_invalid_evidence))
message("Mandatory clinician review: ", mandatory_n)
message("Clinician audit sample: ", length(review_rows), " total; ", length(selected_agreement), " sampled agreement cases")
