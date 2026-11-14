# Why this script is here
# I prepare selected cases and ambiguity summaries for clinician calibration.
# These development cases are not an independent validation sample.

# =============================================================================
# 13_report_alt_dx_calibration.R
# Limited classifier-development calibration for report-based non-AAS diagnoses
#
# Uses frozen v1 outputs without modifying them. Creates aggregate ambiguity
# summaries plus a private, pseudonymous 25-case clinician-calibration source.
# No prevalence, association, historical-regex, or manuscript analysis occurs.
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
  pointer <- file.path(aas_derived_dir, "report_alt_dx", "current_run_path.txt")
  if (!file.exists(pointer)) stop("No report-analysis run was specified.", call. = FALSE)
  run_dir <- trimws(readLines(pointer, warn = FALSE)[1L])
  run_id <- basename(run_dir)
} else {
  run_dir <- file.path(aas_derived_dir, "report_alt_dx", run_id)
}
if (!dir.exists(run_dir)) stop("Report-analysis run directory was not found.", call. = FALSE)

# Load the preserved validation outputs and confirm the expected inputs.
validation_dir <- file.path(run_dir, "validation")
calibration_run_id <- "calibration_v1_20260910_01"
audit_dir <- file.path(run_dir, "clinician_audit", calibration_run_id)
candidates_file <- file.path(validation_dir, "private_candidate_validation.rds")
encounters_file <- file.path(validation_dir, "private_encounter_validation.rds")
if (!file.exists(candidates_file) || !file.exists(encounters_file)) {
  stop("Frozen v1 validation objects are missing.", call. = FALSE)
}

candidates <- readRDS(candidates_file)
encounters <- readRDS(encounters_file)
input_paths <- sort(list.files(file.path(run_dir, "inputs"), pattern = "^RDX-.*\\.json$", full.names = TRUE))
packets <- lapply(input_paths, jsonlite::fromJSON, simplifyVector = FALSE)
packet_ids <- vapply(packets, `[[`, character(1), "report_audit_id")
packets_by_id <- setNames(packets, packet_ids)

if (
  nrow(encounters) != 1111L ||
  anyDuplicated(encounters$report_audit_id) ||
  !setequal(encounters$report_audit_id, packet_ids)
) {
  stop("Frozen v1 encounter coverage failed.", call. = FALSE)
}
if (!all(c("pass", "evidence_valid", "endpoint_qualifying", "endpoint_uncertain_signal") %in% names(candidates))) {
  stop("Frozen v1 candidate-validation fields are incomplete.", call. = FALSE)
}

# Prepare diagnosis and indication groupings for the ambiguity summaries.
collapse_space <- function(x) {
  tolower(trimws(gsub("[[:space:]]+", " ", as.character(x))))
}

indication_groups <- function(packet) {
  text <- collapse_space(paste(
    vapply(packet$clinical_indications, `[[`, character(1), "indication_text"),
    collapse = " "
  ))
  groups <- character()
  if (grepl("chest[^.;,]*pain|pain[^.;,]*chest|\\bcp\\b", text, perl = TRUE)) groups <- c(groups, "chest pain")
  if (grepl("back[^.;,]*pain|pain[^.;,]*back|interscapular", text, perl = TRUE)) groups <- c(groups, "back pain")
  if (grepl("abdom|epigastr|right upper quadrant|left upper quadrant|right lower quadrant|left lower quadrant|\\bruq\\b|\\bllq\\b|\\brlq\\b|\\bluq\\b", text, perl = TRUE)) groups <- c(groups, "abdominal pain/symptoms")
  if (grepl("flank", text, perl = TRUE)) groups <- c(groups, "flank pain")
  if (grepl("shortness of breath|dyspn|hypoxi|respiratory distress|\\bsob\\b", text, perl = TRUE)) groups <- c(groups, "dyspnea/hypoxia")
  if (grepl("syncope|near.syncope|dizz|weakness|neurolog|stroke|numb|altered mental|confusion", text, perl = TRUE)) groups <- c(groups, "syncope/neurologic symptoms")
  if (grepl("trauma|fall|motor vehicle|\\bmvc\\b|injur", text, perl = TRUE)) groups <- c(groups, "trauma")
  if (grepl("hypotensi|shock", text, perl = TRUE)) groups <- c(groups, "hypotension/shock")
  if (grepl("fever|sepsis|infect", text, perl = TRUE)) groups <- c(groups, "fever/infection")
  if (grepl("nausea|vomit|diarrhea|constipation|gastrointestinal|\\bgi\\b", text, perl = TRUE)) groups <- c(groups, "gastrointestinal symptoms")
  if (length(groups) == 0L) groups <- "other/nonspecific indication"
  sort(unique(groups))
}

canonical_diagnosis <- function(diagnosis, category) {
  x <- collapse_space(diagnosis)
  if (grepl("pulmonary embol", x)) return("pulmonary embolism")
  if (grepl("pneumonia|aspiration", x)) return("pneumonia/aspiration")
  if (grepl("ground.?glass|pulmonary opacit|airspace opacit|infiltrat|consolidat", x)) return("pulmonary opacity/consolidation")
  if (grepl("atelect", x)) return("atelectasis")
  if (grepl("pleural effusion", x)) return("pleural effusion")
  if (grepl("pneumothor", x)) return("pneumothorax")
  if (grepl("pulmonary edema|interstitial edema", x)) return("pulmonary edema")
  if (grepl("pericardial effusion", x)) return("pericardial effusion")
  if (grepl("cardiomegal", x)) return("cardiomegaly")
  if (grepl("hiatal hernia", x)) return("hiatal hernia")
  if (grepl("esoph", x)) return("esophageal abnormality")
  if (grepl("cholelith|gallstone", x)) return("gallstones")
  if (grepl("cholecyst", x)) return("cholecystitis")
  if (grepl("pancreat", x)) return("pancreatitis")
  if (grepl("diverticul", x)) return("diverticulitis")
  if (grepl("colitis|enteritis|gastritis|duodenitis", x)) return("gastrointestinal inflammation")
  if (grepl("appendic", x)) return("appendiceal abnormality")
  if (grepl("bowel obstruction|small bowel obstruction|transition point", x)) return("bowel obstruction")
  if (grepl("ileus", x)) return("ileus")
  if (grepl("hydroneph|hydroureter", x)) return("hydronephrosis/hydroureter")
  if (grepl("ureter.*stone|renal.*stone|urinary.*stone|calculus|nephrolith", x)) return("urinary stone")
  if (grepl("pyeloneph", x)) return("pyelonephritis")
  if (grepl("cystitis", x)) return("cystitis")
  if (grepl("renal infarct", x)) return("renal infarction")
  if (grepl("aneurysm", x)) return("non-AAS aneurysm")
  if (grepl("stenosis|narrowing", x) && grepl("vascular|arter|celiac|mesenteric|iliac|carotid|vertebral|subclavian", paste(x, category))) return("arterial stenosis")
  if (grepl("occlusion|thrombus|thrombosis", x)) return("vascular occlusion/thrombosis")
  if (grepl("hematoma|hemorrhage|hemoperitoneum|hemothorax", x)) return("hemorrhage/hematoma")
  if (grepl("fracture", x)) return("fracture")
  if (grepl("mass|malignan|neoplasm|tumou?r|cancer", x)) return("mass/malignancy")
  if (grepl("degenerat|spondyl|osteoarth|arthrosis", x)) return("degenerative musculoskeletal finding")
  if (grepl("hernia", x)) return("non-hiatal hernia")
  x
}

indication_map <- setNames(lapply(packets, indication_groups), packet_ids)
candidates$canonical_diagnosis <- mapply(
  canonical_diagnosis,
  candidates$normalized_diagnosis,
  candidates$diagnostic_category,
  USE.NAMES = FALSE
)
candidates$endpoint_relevant_candidate <-
  candidates$plausibly_explains_indication %in% c("yes", "uncertain") &
  candidates$acuity != "chronic"
candidates$vague_finding <-
  grepl(
    "opacity|stranding|thickening|effusion|atelect|edema|lesion|density|nodule|fluid|dilat|distent|enlarg|inflammatory change|collapse",
    collapse_space(candidates$normalized_diagnosis),
    perl = TRUE
  ) &
  (
    candidates$finding_status == "uncertain" |
    candidates$diagnostic_certainty == "possible_indeterminate"
  )

# Collect encounter-level disagreement and uncertainty signals for selection.
case_rows <- vector("list", nrow(encounters))
for (i in seq_len(nrow(encounters))) {
  audit_id <- encounters$report_audit_id[[i]]
  e <- encounters[i, , drop = FALSE]
  c1 <- candidates[candidates$report_audit_id == audit_id & candidates$pass == 1L, , drop = FALSE]
  c2 <- candidates[candidates$report_audit_id == audit_id & candidates$pass == 2L, , drop = FALSE]
  both <- rbind(c1, c2)
  relevant <- both[both$endpoint_relevant_candidate, , drop = FALSE]
  indication <- indication_map[[audit_id]]

  endpoint_pair <- sort(unique(c(e$pass1_endpoint, e$pass2_endpoint)))
  transition_type <- if (identical(endpoint_pair, c("negative", "positive"))) {
    "positive_vs_negative"
  } else if (identical(endpoint_pair, c("positive", "uncertain"))) {
    "positive_vs_uncertain"
  } else if (identical(endpoint_pair, c("negative", "uncertain"))) {
    "negative_vs_uncertain"
  } else {
    paste0(e$pass1_endpoint, "_agreement")
  }

  flag_plausibility <-
    !e$plausibility_agreement ||
    any(relevant$plausibly_explains_indication == "uncertain")
  flag_nonspecific_indication <- "other/nonspecific indication" %in% indication
  flag_certainty <-
    !e$certainty_agreement ||
    any(relevant$diagnostic_certainty == "possible_indeterminate")
  flag_acuity <-
    !e$acuity_agreement ||
    any(relevant$acuity == "indeterminate")
  flag_negation <-
    any(both$finding_status == "negated_absent") ||
    (!e$status_agreement && any(both$finding_status %in% c("negated_absent", "uncertain")))
  flag_multiple <-
    max(sum(c1$endpoint_relevant_candidate), sum(c2$endpoint_relevant_candidate)) > 1L ||
    length(unique(relevant$canonical_diagnosis)) > 1L
  flag_diagnosis_category <- !e$diagnosis_agreement || !e$category_agreement
  flag_vague <- any(relevant$vague_finding)
  flag_evidence <- e$any_invalid_evidence

  flags <- c(
    evidence_quotation_failure = flag_evidence,
    explicit_negation = flag_negation,
    plausibility_mapping = flag_plausibility,
    nonspecific_indication = flag_nonspecific_indication,
    finding_certainty = flag_certainty,
    acuity_chronicity = flag_acuity,
    multiple_competing_findings = flag_multiple,
    diagnosis_category_disagreement = flag_diagnosis_category,
    vague_nonspecific_finding = flag_vague
  )
  primary_mechanism <- names(flags)[which(flags)[1L]]
  if (is.na(primary_mechanism)) primary_mechanism <- "other"

  case_rows[[i]] <- data.frame(
    report_audit_id = audit_id,
    pass1_endpoint = e$pass1_endpoint,
    pass2_endpoint = e$pass2_endpoint,
    endpoint_disagreement = !e$endpoint_agreement,
    uncertain_either = e$any_uncertain_endpoint,
    invalid_evidence = e$any_invalid_evidence,
    problem_case = !e$endpoint_agreement || e$any_uncertain_endpoint,
    calibration_problem_or_evidence = !e$endpoint_agreement || e$any_uncertain_endpoint || e$any_invalid_evidence,
    transition_type = transition_type,
    indication_groups = paste(indication, collapse = " | "),
    categories = paste(sort(unique(relevant$diagnostic_category)), collapse = " | "),
    diagnoses = paste(sort(unique(relevant$canonical_diagnosis)), collapse = " | "),
    flag_plausibility_mapping = flag_plausibility,
    flag_nonspecific_indication = flag_nonspecific_indication,
    flag_finding_certainty = flag_certainty,
    flag_acuity_chronicity = flag_acuity,
    flag_explicit_negation = flag_negation,
    flag_multiple_competing_findings = flag_multiple,
    flag_diagnosis_category_disagreement = flag_diagnosis_category,
    flag_vague_nonspecific_finding = flag_vague,
    flag_evidence_quotation_failure = flag_evidence,
    primary_mechanism = primary_mechanism,
    stringsAsFactors = FALSE
  )
}
case_data <- do.call(rbind, case_rows)

problem_data <- case_data[case_data$problem_case, , drop = FALSE]
if (nrow(problem_data) != 411L) {
  stop("Uncertain/disagreement development set no longer contains 411 encounters.", call. = FALSE)
}

# Summarize the recurring ambiguity mechanisms, including overlapping signals.
mechanism_definitions <- c(
  plausibility_mapping = "Passes differ on plausibility or an endpoint-relevant candidate is plausibility-uncertain",
  nonspecific_indication = "No prespecified symptom/indication group can be identified from the supplied indication",
  finding_certainty = "Passes differ on certainty or an endpoint-relevant candidate is possible/indeterminate",
  acuity_chronicity = "Passes differ on acuity or an endpoint-relevant candidate has indeterminate acuity",
  explicit_negation = "A pass records a negated/absent candidate or status disagreement involves negation/uncertainty",
  multiple_competing_findings = "A pass has multiple endpoint-relevant candidates or the encounter has multiple such diagnoses",
  diagnosis_category_disagreement = "Passes differ in normalized diagnosis set or category set",
  vague_nonspecific_finding = "An uncertain/possible candidate uses a nonspecific imaging-finding term",
  evidence_quotation_failure = "At least one required exact quotation fails source verification",
  other = "No listed structured mechanism is detected"
)

mechanism_flags <- c(
  plausibility_mapping = "flag_plausibility_mapping",
  nonspecific_indication = "flag_nonspecific_indication",
  finding_certainty = "flag_finding_certainty",
  acuity_chronicity = "flag_acuity_chronicity",
  explicit_negation = "flag_explicit_negation",
  multiple_competing_findings = "flag_multiple_competing_findings",
  diagnosis_category_disagreement = "flag_diagnosis_category_disagreement",
  vague_nonspecific_finding = "flag_vague_nonspecific_finding",
  evidence_quotation_failure = "flag_evidence_quotation_failure"
)

ambiguity_nonexclusive <- do.call(rbind, lapply(names(mechanism_flags), function(mechanism) {
  flag <- problem_data[[mechanism_flags[[mechanism]]]]
  data.frame(
    summary_type = "Nonmutually exclusive mechanism",
    ambiguity_mechanism = mechanism,
    n_problem_encounters = sum(flag),
    problem_denominator = nrow(problem_data),
    percent_problem = 100 * sum(flag) / nrow(problem_data),
    n_endpoint_disagreements = sum(flag & problem_data$endpoint_disagreement),
    endpoint_disagreement_denominator = sum(problem_data$endpoint_disagreement),
    n_uncertain_either = sum(flag & problem_data$uncertain_either),
    uncertain_either_denominator = sum(problem_data$uncertain_either),
    mutually_exclusive = FALSE,
    operational_definition = mechanism_definitions[[mechanism]],
    stringsAsFactors = FALSE
  )
}))

primary_counts <- table(factor(problem_data$primary_mechanism, levels = names(mechanism_definitions)))
ambiguity_primary <- data.frame(
  summary_type = "Primary mechanism by prespecified priority",
  ambiguity_mechanism = names(primary_counts),
  n_problem_encounters = as.integer(primary_counts),
  problem_denominator = nrow(problem_data),
  percent_problem = 100 * as.integer(primary_counts) / nrow(problem_data),
  n_endpoint_disagreements = vapply(
    names(primary_counts),
    function(x) sum(problem_data$primary_mechanism == x & problem_data$endpoint_disagreement),
    integer(1)
  ),
  endpoint_disagreement_denominator = sum(problem_data$endpoint_disagreement),
  n_uncertain_either = vapply(
    names(primary_counts),
    function(x) sum(problem_data$primary_mechanism == x & problem_data$uncertain_either),
    integer(1)
  ),
  uncertain_either_denominator = sum(problem_data$uncertain_either),
  mutually_exclusive = TRUE,
  operational_definition = unname(mechanism_definitions[names(primary_counts)]),
  stringsAsFactors = FALSE
)

ambiguity_summary <- rbind(ambiguity_nonexclusive, ambiguity_primary)
ambiguity_summary$generating_script <- "R/13_report_alt_dx_calibration.R"
ambiguity_summary$generating_object <- "ambiguity_summary"

# One row per encounter/pass/canonical finding/indication group. This is used
# only to aggregate recurring ambiguity; no patient-level version is committed.
pair_rows <- vector("list", 0L)
for (i in seq_len(nrow(candidates))) {
  candidate <- candidates[i, , drop = FALSE]
  if (candidate$finding_status == "negated_absent") next
  groups <- indication_map[[candidate$report_audit_id]]
  for (group in groups) {
    pair_rows[[length(pair_rows) + 1L]] <- data.frame(
      report_audit_id = candidate$report_audit_id,
      pass = candidate$pass,
      canonical_diagnosis = candidate$canonical_diagnosis,
      diagnostic_category = candidate$diagnostic_category,
      indication_group = group,
      explanatory_judgment = candidate$plausibly_explains_indication,
      stringsAsFactors = FALSE
    )
  }
}
pair_long <- unique(do.call(rbind, pair_rows))
pair_long <- merge(
  pair_long,
  case_data[c("report_audit_id", "problem_case", "endpoint_disagreement", "uncertain_either", "primary_mechanism")],
  by = "report_audit_id",
  all.x = TRUE,
  sort = FALSE
)

pair_keys <- unique(pair_long[c("canonical_diagnosis", "indication_group")])
pair_summary_rows <- vector("list", 0L)
pair_problem_frequency <- list()
for (i in seq_len(nrow(pair_keys))) {
  diagnosis <- pair_keys$canonical_diagnosis[[i]]
  indication <- pair_keys$indication_group[[i]]
  rows <- pair_long[
    pair_long$canonical_diagnosis == diagnosis & pair_long$indication_group == indication,
    ,
    drop = FALSE
  ]
  all_ids <- unique(rows$report_audit_id)
  problem_ids <- unique(rows$report_audit_id[rows$problem_case])
  if (length(problem_ids) < 2L) next
  problem_rows <- rows[rows$report_audit_id %in% problem_ids, , drop = FALSE]

  judgment_summary <- function(pass_number) {
    pass_rows <- problem_rows[problem_rows$pass == pass_number, , drop = FALSE]
    observed <- unique(pass_rows[c("report_audit_id", "explanatory_judgment")])
    counts <- table(factor(observed$explanatory_judgment, levels = c("yes", "no", "uncertain")))
    absent_n <- length(setdiff(problem_ids, unique(pass_rows$report_audit_id)))
    paste0(
      "yes ", counts[["yes"]], "; no ", counts[["no"]],
      "; uncertain ", counts[["uncertain"]], "; absent ", absent_n
    )
  }

  mechanism_counts <- sort(table(case_data$primary_mechanism[match(problem_ids, case_data$report_audit_id)]), decreasing = TRUE)
  dominant_mechanism <- names(mechanism_counts)[[1L]]
  categories <- sort(unique(problem_rows$diagnostic_category))
  pair_name <- paste(diagnosis, indication, sep = " × ")
  pair_problem_frequency[[pair_name]] <- length(problem_ids)

  pair_summary_rows[[length(pair_summary_rows) + 1L]] <- data.frame(
    normalized_imaging_diagnosis_or_finding = diagnosis,
    diagnostic_category = paste(categories, collapse = " | "),
    normalized_indication_group = indication,
    n_affected_encounters_all_classifier_outputs = length(all_ids),
    n_problem_encounters = length(problem_ids),
    pass_1_explanatory_judgments_in_problem_encounters = judgment_summary(1L),
    pass_2_explanatory_judgments_in_problem_encounters = judgment_summary(2L),
    n_endpoint_disagreement = sum(case_data$endpoint_disagreement[match(problem_ids, case_data$report_audit_id)]),
    n_uncertain_either = sum(case_data$uncertain_either[match(problem_ids, case_data$report_audit_id)]),
    problem_proportion_percent = 100 * length(problem_ids) / length(all_ids),
    dominant_ambiguity_mechanism = dominant_mechanism,
    stringsAsFactors = FALSE
  )
}
diagnosis_indication_summary <- do.call(rbind, pair_summary_rows)
diagnosis_indication_summary <- diagnosis_indication_summary[
  order(
    -diagnosis_indication_summary$n_problem_encounters,
    diagnosis_indication_summary$normalized_imaging_diagnosis_or_finding,
    diagnosis_indication_summary$normalized_indication_group
  ),
  ,
  drop = FALSE
]
diagnosis_indication_summary$generating_script <- "R/13_report_alt_dx_calibration.R"
diagnosis_indication_summary$generating_object <- "diagnosis_indication_summary"

# Track recurring diagnosis and indication combinations for case selection.
case_pair_signatures <- setNames(rep("", nrow(case_data)), case_data$report_audit_id)
pair_problem_n <- setNames(
  diagnosis_indication_summary$n_problem_encounters,
  paste(
    diagnosis_indication_summary$normalized_imaging_diagnosis_or_finding,
    diagnosis_indication_summary$normalized_indication_group,
    sep = " × "
  )
)
for (audit_id in case_data$report_audit_id) {
  rows <- pair_long[pair_long$report_audit_id == audit_id, , drop = FALSE]
  signatures <- unique(paste(rows$canonical_diagnosis, rows$indication_group, sep = " × "))
  signatures <- signatures[signatures %in% names(pair_problem_n)]
  case_pair_signatures[[audit_id]] <- paste(sort(signatures), collapse = " | ")
}
case_data$recurring_pairs <- unname(case_pair_signatures[case_data$report_audit_id])
case_data$maximum_pair_frequency <- vapply(
  case_data$recurring_pairs,
  function(x) {
    pairs <- strsplit(x, " \\| ", perl = TRUE)[[1L]]
    pairs <- pairs[nzchar(pairs) & pairs %in% names(pair_problem_n)]
    if (length(pairs) == 0L) return(0L)
    max(unname(pair_problem_n[pairs]))
  },
  integer(1)
)

# Use a stable selection order while seeking variety across difficult cases.
selection_seed <- 20260910L
stable_tie <- function(audit_id) {
  digest::digest(paste(selection_seed, audit_id, sep = "|"), algo = "sha256", serialize = FALSE)
}

selected_ids <- character()
selected_categories <- selected_indications <- selected_mechanisms <- selected_pairs <- character()

update_selected_sets <- function(audit_id) {
  row <- case_data[case_data$report_audit_id == audit_id, , drop = FALSE]
  selected_categories <<- unique(c(selected_categories, strsplit(row$categories, " \\| ", perl = TRUE)[[1L]]))
  selected_indications <<- unique(c(selected_indications, strsplit(row$indication_groups, " \\| ", perl = TRUE)[[1L]]))
  selected_mechanisms <<- unique(c(selected_mechanisms, row$primary_mechanism))
  selected_pairs <<- unique(c(selected_pairs, strsplit(row$recurring_pairs, " \\| ", perl = TRUE)[[1L]]))
}

diversity_score <- function(audit_id, recurring_weight = 0) {
  row <- case_data[case_data$report_audit_id == audit_id, , drop = FALSE]
  categories <- strsplit(row$categories, " \\| ", perl = TRUE)[[1L]]
  indications <- strsplit(row$indication_groups, " \\| ", perl = TRUE)[[1L]]
  pairs <- strsplit(row$recurring_pairs, " \\| ", perl = TRUE)[[1L]]
  categories <- categories[nzchar(categories)]
  indications <- indications[nzchar(indications)]
  pairs <- pairs[nzchar(pairs)]
  12 * sum(!categories %in% selected_categories) +
    8 * sum(!indications %in% selected_indications) +
    6 * as.integer(!row$primary_mechanism %in% selected_mechanisms) +
    4 * sum(!pairs %in% selected_pairs) +
    recurring_weight * row$maximum_pair_frequency
}

greedy_select <- function(pool_ids, n, recurring_weight = 0) {
  chosen <- character()
  available <- setdiff(pool_ids, selected_ids)
  while (length(chosen) < n && length(available) > 0L) {
    scores <- vapply(available, diversity_score, numeric(1), recurring_weight = recurring_weight)
    best <- available[scores == max(scores)]
    best <- best[order(vapply(best, stable_tie, character(1)))][[1L]]
    chosen <- c(chosen, best)
    selected_ids <<- c(selected_ids, best)
    update_selected_sets(best)
    available <- setdiff(available, best)
  }
  chosen
}

# A. Ten endpoint-relevant disagreements: all positive-vs-negative, then four
# positive-vs-uncertain, with the remainder negative-vs-uncertain.
disagreement_pool <- case_data[case_data$endpoint_disagreement, , drop = FALSE]
a_pos_neg <- greedy_select(
  disagreement_pool$report_audit_id[disagreement_pool$transition_type == "positive_vs_negative"],
  min(2L, sum(disagreement_pool$transition_type == "positive_vs_negative"))
)
a_pos_uncertain <- greedy_select(
  disagreement_pool$report_audit_id[disagreement_pool$transition_type == "positive_vs_uncertain"],
  4L
)
a_remaining_n <- 10L - length(a_pos_neg) - length(a_pos_uncertain)
a_neg_uncertain <- greedy_select(
  disagreement_pool$report_audit_id[disagreement_pool$transition_type == "negative_vs_uncertain"],
  a_remaining_n
)
selection_a <- c(a_pos_neg, a_pos_uncertain, a_neg_uncertain)

# B. Ten recurring plausibility-uncertainty cases, preferring uncertainty in
# both passes and recurrent diagnosis-indication combinations.
plausibility_pool <- case_data[
  case_data$flag_plausibility_mapping & case_data$uncertain_either &
    !case_data$report_audit_id %in% selected_ids,
  ,
  drop = FALSE
]
plausibility_pool <- plausibility_pool[
  order(
    plausibility_pool$pass1_endpoint != "uncertain" | plausibility_pool$pass2_endpoint != "uncertain",
    -plausibility_pool$maximum_pair_frequency
  ),
  ,
  drop = FALSE
]
selection_b <- greedy_select(plausibility_pool$report_audit_id, 10L, recurring_weight = 3)

# C. Five additional edge cases from distinct structured mechanisms where
# available. These remain development cases and are not a validation sample.
edge_priority <- c(
  "flag_evidence_quotation_failure",
  "flag_acuity_chronicity",
  "flag_finding_certainty",
  "flag_multiple_competing_findings",
  "flag_nonspecific_indication",
  "flag_explicit_negation",
  "flag_diagnosis_category_disagreement",
  "flag_vague_nonspecific_finding"
)
selection_c <- character()
for (flag_name in edge_priority) {
  if (length(selection_c) >= 5L) break
  pool <- case_data$report_audit_id[
    case_data[[flag_name]] & case_data$calibration_problem_or_evidence &
      !case_data$report_audit_id %in% selected_ids
  ]
  chosen <- greedy_select(pool, 1L)
  if (length(chosen) == 1L) selection_c <- c(selection_c, chosen)
}
if (length(selection_c) < 5L) {
  fill_pool <- case_data$report_audit_id[
    case_data$calibration_problem_or_evidence & !case_data$report_audit_id %in% selected_ids
  ]
  selection_c <- c(selection_c, greedy_select(fill_pool, 5L - length(selection_c)))
}

if (length(selection_a) != 10L || length(selection_b) != 10L || length(selection_c) != 5L) {
  stop("Calibration selection did not produce the required 10/10/5 allocation.", call. = FALSE)
}
if (anyDuplicated(c(selection_a, selection_b, selection_c))) {
  stop("Calibration selection contains duplicate encounters.", call. = FALSE)
}

# Check and describe the selected development cases before preparing review material.
selection <- data.frame(
  report_audit_id = c(selection_a, selection_b, selection_c),
  calibration_group = c(
    rep("A. Endpoint-relevant disagreement", 10L),
    rep("B. Recurring plausibility uncertainty", 10L),
    rep("C. Additional edge case", 5L)
  ),
  calibration_order = seq_len(25L),
  stringsAsFactors = FALSE
)
selection <- merge(selection, case_data, by = "report_audit_id", all.x = TRUE, sort = FALSE)
selection <- selection[order(selection$calibration_order), , drop = FALSE]

auto_reason <- function(row) {
  parts <- character()
  if (row$endpoint_disagreement) {
    parts <- c(parts, paste0("Pass endpoints differ (", row$pass1_endpoint, " vs ", row$pass2_endpoint, ")"))
  } else if (row$uncertain_either) {
    parts <- c(parts, paste0("Endpoint uncertain in ", ifelse(row$pass1_endpoint == "uncertain" && row$pass2_endpoint == "uncertain", "both passes", "one pass")))
  }
  labels <- c(
    flag_plausibility_mapping = "plausibility linkage differs or remains uncertain",
    flag_finding_certainty = "diagnostic certainty differs or remains indeterminate",
    flag_acuity_chronicity = "acuity/chronicity differs or remains indeterminate",
    flag_explicit_negation = "finding status involves negation/uncertainty",
    flag_multiple_competing_findings = "multiple potentially relevant findings",
    flag_diagnosis_category_disagreement = "diagnosis/category representation differs",
    flag_nonspecific_indication = "clinical indication is nonspecific under the prespecified grouping",
    flag_vague_nonspecific_finding = "candidate wording is nonspecific",
    flag_evidence_quotation_failure = "a required quotation failed exact verification"
  )
  for (flag_name in names(labels)) {
    if (isTRUE(row[[flag_name]])) parts <- c(parts, labels[[flag_name]])
  }
  paste(head(parts, 4L), collapse = "; ")
}
selection$automatic_ambiguity_description <- vapply(
  seq_len(nrow(selection)),
  function(i) auto_reason(selection[i, , drop = FALSE]),
  character(1)
)

selection_summary <- rbind(
  data.frame(
    summary_dimension = "Calibration group",
    value = names(table(selection$calibration_group)),
    n = as.integer(table(selection$calibration_group)),
    denominator = nrow(selection),
    stringsAsFactors = FALSE
  ),
  data.frame(
    summary_dimension = "Endpoint transition",
    value = names(table(selection$transition_type)),
    n = as.integer(table(selection$transition_type)),
    denominator = nrow(selection),
    stringsAsFactors = FALSE
  ),
  data.frame(
    summary_dimension = "Primary ambiguity mechanism",
    value = names(table(selection$primary_mechanism)),
    n = as.integer(table(selection$primary_mechanism)),
    denominator = nrow(selection),
    stringsAsFactors = FALSE
  ),
  data.frame(
    summary_dimension = "Calibration group x endpoint transition",
    value = names(table(paste(selection$calibration_group, selection$transition_type, sep = " | "))),
    n = as.integer(table(paste(selection$calibration_group, selection$transition_type, sep = " | "))),
    denominator = nrow(selection),
    stringsAsFactors = FALSE
  ),
  data.frame(
    summary_dimension = "Indication group represented",
    value = names(table(unlist(strsplit(selection$indication_groups, " \\| ", perl = TRUE)))),
    n = as.integer(table(unlist(strsplit(selection$indication_groups, " \\| ", perl = TRUE)))),
    denominator = nrow(selection),
    stringsAsFactors = FALSE
  )
)
selection_summary$percent <- 100 * selection_summary$n / selection_summary$denominator
selection_summary$mutually_exclusive <- selection_summary$summary_dimension %in% c(
  "Calibration group", "Endpoint transition", "Primary ambiguity mechanism",
  "Calibration group x endpoint transition"
)
selection_summary$generating_script <- "R/13_report_alt_dx_calibration.R"
selection_summary$generating_object <- "selection_summary"

numbered_field <- function(rows, field) {
  if (nrow(rows) == 0L) return("")
  paste(paste0(seq_len(nrow(rows)), ". ", as.character(rows[[field]])), collapse = "\n")
}

# Build the private clinician review source for the selected cases.
review_rows <- vector("list", nrow(selection))
for (i in seq_len(nrow(selection))) {
  row <- selection[i, , drop = FALSE]
  audit_id <- row$report_audit_id
  packet <- packets_by_id[[audit_id]]
  c1 <- candidates[candidates$report_audit_id == audit_id & candidates$pass == 1L, , drop = FALSE]
  c2 <- candidates[candidates$report_audit_id == audit_id & candidates$pass == 2L, , drop = FALSE]

  report_text_for <- function(component) {
    report_rows <- packet$reports[vapply(
      packet$reports,
      function(x) x$component %in% c(component, "both"),
      logical(1)
    )]
    if (length(report_rows) == 0L) return("")
    paste(vapply(
      report_rows,
      function(x) paste0("[", x$source_report_key, "; ", x$text_source, "]\n", x$report_text),
      character(1)
    ), collapse = "\n\n")
  }
  report_source_for <- function(component) {
    report_rows <- packet$reports[vapply(
      packet$reports,
      function(x) x$component %in% c(component, "both"),
      logical(1)
    )]
    if (length(report_rows) == 0L) return("")
    paste(unique(vapply(report_rows, `[[`, character(1), "text_source")), collapse = " | ")
  }
  indications <- paste(vapply(
    packet$clinical_indications,
    function(x) paste0("[", x$component, "] ", x$indication_text),
    character(1)
  ), collapse = "\n")

  pass_fields <- function(rows, endpoint) {
    list(
      endpoint = endpoint,
      normalized_diagnosis = numbered_field(rows, "normalized_diagnosis"),
      category = numbered_field(rows, "diagnostic_category"),
      finding_status = numbered_field(rows, "finding_status"),
      certainty = numbered_field(rows, "diagnostic_certainty"),
      acuity = numbered_field(rows, "acuity"),
      plausibly_explains = numbered_field(rows, "plausibly_explains_indication"),
      report_evidence_quote = numbered_field(rows, "report_evidence_quote"),
      indication_evidence_quote = numbered_field(rows, "indication_evidence_quote")
    )
  }

  review_rows[[i]] <- list(
    calibration_order = row$calibration_order,
    calibration_group = row$calibration_group,
    pseudonymous_study_id = audit_id,
    automatic_ambiguity_description = row$automatic_ambiguity_description,
    clinical_indication = indications,
    chest_report_source = report_source_for("chest"),
    chest_impression_or_fallback = report_text_for("chest"),
    abdomen_pelvis_report_source = report_source_for("abdomen_pelvis"),
    abdomen_pelvis_impression_or_fallback = report_text_for("abdomen_pelvis"),
    pass_1 = pass_fields(c1, row$pass1_endpoint),
    pass_2 = pass_fields(c2, row$pass2_endpoint),
    clinician_review = list(
      imaging_diagnosis_present = "",
      diagnostic_statement_sufficiently_affirmative = "",
      could_reasonably_explain_documented_symptom = "",
      acuity_classification = "",
      correct_diagnosis_or_category = "",
      classifier_difficulty = "",
      clinician_endpoint_under_current_v1_rule = "",
      comments = ""
    )
  )
}

dir.create("results/report_alt_dx", recursive = TRUE, showWarnings = FALSE)
write.csv(
  ambiguity_summary,
  "results/report_alt_dx/calibration_ambiguity_mechanisms.csv",
  row.names = FALSE,
  na = ""
)
write.csv(
  diagnosis_indication_summary,
  "results/report_alt_dx/calibration_diagnosis_indication_pairs.csv",
  row.names = FALSE,
  na = ""
)
write.csv(
  selection_summary,
  "results/report_alt_dx/calibration_selection_summary.csv",
  row.names = FALSE,
  na = ""
)

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(selection, file.path(audit_dir, "private_calibration_selection_25.rds"))
jsonlite::write_json(
  list(
    run_id = run_id,
    classifier_version = "v1",
    endpoint_rule_version = "v1_unchanged",
    selection_seed = selection_seed,
    random_selection_used = FALSE,
    calibration_case_count = nrow(selection),
    later_validation_exclusion_required = TRUE,
    review_rows = review_rows
  ),
  file.path(audit_dir, "clinician_calibration_25_source.json"),
  auto_unbox = TRUE,
  pretty = TRUE,
  null = "null"
)
jsonlite::write_json(
  list(
    run_id = run_id,
    classifier_version = "v1",
    exclusion_reason = "Classifier-development limited calibration set; exclude from independent validation",
    pseudonymous_study_ids = selection$report_audit_id
  ),
  file.path(audit_dir, "calibration_development_exclusions.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

# Record the calibration inputs and outputs for the run.
manifest <- list(
  run_id = run_id,
  calibration_run_id = calibration_run_id,
  created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  classifier_version = "v1 preserved unchanged",
  endpoint_definition = "v1 preserved unchanged",
  candidate_validation_sha256 = digest::digest(file = candidates_file, algo = "sha256"),
  encounter_validation_sha256 = digest::digest(file = encounters_file, algo = "sha256"),
  selection_seed = selection_seed,
  random_selection_used = FALSE,
  selection_allocation = list(
    endpoint_disagreements = length(selection_a),
    recurring_plausibility_uncertainty = length(selection_b),
    additional_edge_cases = length(selection_c)
  ),
  prohibited_inputs_used = FALSE,
  prohibited_inputs = c(
    "prevalence", "diagnostic-category prevalence", "provider association",
    "algorithm-concordance association", "historical regex outcome", "manuscript result"
  )
)
jsonlite::write_json(
  manifest,
  file.path(audit_dir, "calibration_selection_manifest.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

message("Limited calibration source created with aggregate-only terminal output.")
message("Uncertain/disagreement development set: ", nrow(problem_data))
message("Recurring diagnosis-indication pairs: ", nrow(diagnosis_indication_summary))
message("Calibration cases selected: ", nrow(selection), " (10 disagreement, 10 plausibility, 5 edge)")
message("Calibration cases marked for exclusion from later independent validation: ", nrow(selection))
