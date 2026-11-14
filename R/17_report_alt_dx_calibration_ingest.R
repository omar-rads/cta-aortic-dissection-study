# Why this script is here
# I ingest completed clinician calibration judgments for the report analysis.
# Case-level calibration artifacts remain outside the repository.
# Ingest completed candidate-level clinician calibration; never edit v1.
# Identifiable and encounter/candidate-level artifacts stay outside Git.
source("R/00_setup.R")

suppressPackageStartupMessages({
    library(readxl)
    library(jsonlite)
    library(digest)
})

args <- commandArgs(TRUE)

stopifnot(length(args) == 2L)

workbook_path <- normalizePath(args[1], mustWork = TRUE)

run_id <- args[2]

stopifnot(grepl("^report_alt_dx_v2_[0-9]{8}_[0-9]+$", run_id))

v1_dir <- file.path(aas_derived_dir, "report_alt_dx", "report_alt_dx_v1_20260910_01")

run_dir <- file.path(aas_derived_dir, "report_alt_dx", run_id)

cal_dir <- file.path(run_dir, "calibration")

out_dir <- "results/report_alt_dx_v2"

dir.create(cal_dir, recursive = TRUE, showWarnings = FALSE)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Read the completed candidate review and check the expected fields.
x <- as.data.frame(read_xlsx(workbook_path, sheet = "Candidate Review", trim_ws = FALSE))

stopifnot(ncol(x) == 40L, nrow(x) == 84L, length(unique(x[[1]])) == 25L, !anyDuplicated(x[[2]]))

original_names <- names(x)

names(x)[1:16] <- c("report_audit_id", "candidate_id", "linkage_label", "appeared_in", "indication",
    "chest", "abdomen_pelvis", "clinician_present", "clinician_affirmative", "clinician_explains", "clinician_acuity",
    "corrected_diagnosis", "corrected_category", "main_error", "comments", "linkage_note")

for (j in 8:11) stopifnot(!anyNA(x[[j]]))

stopifnot(all(x$clinician_present %in% c("Yes", "No", "Uncertain")), all(x$clinician_affirmative %in%
    c("Yes", "No", "Uncertain")), all(x$clinician_explains %in% c("Yes", "No", "Context-dependent/Uncertain")),
    all(x$clinician_acuity %in% c("Acute", "Likely acute", "Chronic", "Indeterminate", "Not relevant")))

# Match the review rows to the preserved v1 calibration sources.
exclusions <- fromJSON(file.path(v1_dir, "clinician_audit/calibration_v1_20260910_01/calibration_development_exclusions.json"))

stopifnot(setequal(x$report_audit_id, exclusions$pseudonymous_study_ids))

v1c <- readRDS(file.path(v1_dir, "validation/private_candidate_validation.rds"))

v1e <- readRDS(file.path(v1_dir, "validation/private_encounter_validation.rds"))

# Excel serialized linefeeds as CRLF on save. Normalize this transport-only
# difference for provenance, never for the classifier's exact verifier.
for (p in 1:2) {
    start <- if (p == 1L)
        17L
    else 29L
    fields <- c("candidate_number", "normalized_diagnosis", "diagnostic_category", "finding_status",
        "diagnostic_certainty", "acuity", "plausibly_explains_indication", "report_evidence_quote", "indication_evidence_quote",
        "cta_component", "source_report_keys", "evidence_valid")
    names(x)[start:(start + 11L)] <- paste0("pass", p, "_", fields)
    for (i in seq_len(nrow(x))) {
        cn <- x[[start]][i]
        if (is.na(cn))
            next
        old <- v1c[v1c$report_audit_id == x$report_audit_id[i] & v1c$pass == p & v1c$candidate_number ==
            cn, , drop = FALSE]
        stopifnot(nrow(old) == 1L)
        for (k in seq_along(fields)) {
            a <- as.character(x[[start + k - 1L]][i])
            b <- as.character(old[[fields[k]]])
            if (is.na(a))
                a <- ""
            a <- gsub("\r\n", "\n", a, fixed = TRUE)
            b <- gsub("\r\n", "\n", b, fixed = TRUE)
            if (fields[k] == "source_report_keys")
                b <- gsub("\037", " | ", b, fixed = TRUE)
            if (!identical(tolower(a), tolower(b)))
                stop("Workbook source provenance differs at row ", i + 1L, ", pass ", p, ", field ",
                  fields[k], " (values suppressed)")
        }
    }
}

x$workbook_row <- seq_len(nrow(x)) + 1L

x$qualifying <- x$clinician_present == "Yes" & x$clinician_affirmative == "Yes" & x$clinician_explains ==
    "Yes" & x$clinician_acuity %in% c("Acute", "Likely acute", "Indeterminate")

x$excluded <- x$clinician_present == "No" | x$clinician_affirmative == "No" | x$clinician_explains ==
    "No" | x$clinician_acuity == "Chronic"

x$unresolved <- !x$excluded & !x$qualifying & (x$clinician_present == "Uncertain" | x$clinician_affirmative ==
    "Uncertain" | x$clinician_explains == "Context-dependent/Uncertain" | x$clinician_acuity == "Not relevant")

stopifnot(all(x$qualifying | x$excluded | x$unresolved))

# Roll candidate judgments up to encounter-level calibration endpoints.
ids <- unique(x$report_audit_id)

enc <- do.call(rbind, lapply(ids, function(id) {
    z <- x[x$report_audit_id == id, ]
    old <- v1e[v1e$report_audit_id == id, ]
    data.frame(report_audit_id = id, n_candidates = nrow(z), n_qualifying = sum(z$qualifying), n_unresolved = sum(z$unresolved),
        clinician_endpoint = if (any(z$qualifying))
            "positive"
        else if (any(z$unresolved))
            "uncertain"
        else "negative", pass1_endpoint = old$pass1_endpoint, pass2_endpoint = old$pass2_endpoint)
}))

write.csv(enc, file.path(cal_dir, "clinician_encounter_endpoints_25.csv"), row.names = FALSE)

saveRDS(x, file.path(cal_dir, "private_clinician_candidate_judgments.rds"))

write_json(list(workbook_sha256 = digest(file = workbook_path, algo = "sha256"), workbook_path = workbook_path,
    n_candidates = nrow(x), n_encounters = nrow(enc), encounter_summary_blanks_expected = TRUE, endpoint_rule = "User-specified clinician mapping of frozen v1 endpoint",
    development_cases_excluded_from_independent_validation = TRUE), file.path(cal_dir, "ingestion_manifest.json"),
    pretty = TRUE, auto_unbox = TRUE)

# Keep a private copy of the reviewed workbook and check an existing copy by hash.
locked <- file.path(cal_dir, basename(workbook_path))

if (file.exists(locked)) stopifnot(digest(file = locked, algo = "sha256") == digest(file = workbook_path,
    algo = "sha256")) else stopifnot(file.copy(workbook_path, locked, overwrite = FALSE))

summary <- data.frame(metric = c("Reviewed candidates", "Reviewed encounters", "Candidate qualifying",
    "Candidate unresolved potentially qualifying", "Candidate excluded", "Clinician positive encounters",
    "Clinician negative encounters", "Clinician uncertain encounters", "Comments present", "Corrected diagnosis present",
    "Corrected category present"), n = c(nrow(x), nrow(enc), sum(x$qualifying), sum(x$unresolved), sum(x$excluded),
    sum(enc$clinician_endpoint == "positive"), sum(enc$clinician_endpoint == "negative"), sum(enc$clinician_endpoint ==
        "uncertain"), sum(!is.na(x$comments)), sum(!is.na(x$corrected_diagnosis)), sum(!is.na(x$corrected_category))))

summary$generating_script <- "R/17_report_alt_dx_calibration_ingest.R"

write.csv(summary, file.path(out_dir, "calibration_summary.csv"), row.names = FALSE)

# Summarize endpoint transitions and agreement for each pass.
transitions <- do.call(rbind, lapply(1:2, function(p) {
    d <- as.data.frame(table(clinician = factor(enc$clinician_endpoint, levels = c("positive", "negative",
        "uncertain")), classifier = factor(enc[[paste0("pass", p, "_endpoint")]], levels = c("positive",
        "negative", "uncertain"))))
    d$pass <- p
    d
}))

write.csv(transitions, file.path(out_dir, "calibration_endpoint_transitions.csv"), row.names = FALSE)

comparisons <- list()

for (p in 1:2) {
    present <- !is.na(x[[paste0("pass", p, "_candidate_number")]])
    a <- x[present, ]
    pred_present <- a[[paste0("pass", p, "_finding_status")]] == "present"
    pred_affirmative <- a[[paste0("pass", p, "_finding_status")]] == "present" & a[[paste0("pass", p,
        "_diagnostic_certainty")]] %in% c("definite", "probable_likely")
    acmap <- c(Acute = "acute", `Likely acute` = "likely_acute", Chronic = "chronic", Indeterminate = "indeterminate",
        `Not relevant` = "not_relevant")
    plmap <- c(Yes = "yes", No = "no", `Context-dependent/Uncertain` = "uncertain")
    counts <- c(sum(!present), sum(pred_present != (a$clinician_present == "Yes")), sum(pred_affirmative !=
        (a$clinician_affirmative == "Yes")), sum(a[[paste0("pass", p, "_acuity")]] != acmap[a$clinician_acuity]),
        sum(a[[paste0("pass", p, "_plausibly_explains_indication")]] != plmap[a$clinician_explains]),
        sum(!a[[paste0("pass", p, "_evidence_valid")]]))
    comparisons[[p]] <- data.frame(pass = p, metric = c("Candidate linkage rows absent from pass", "Presence/affirmation status differs from clinician",
        "Sufficiently affirmative joint status-certainty differs", "Acuity differs", "Plausibility differs",
        "Candidate linkage rows with invalid frozen evidence"), n = counts, denominator = c(nrow(x),
        rep(nrow(a), 5)), unit = "Workbook linked candidate row; shared composite source candidates may repeat")
}

write.csv(do.call(rbind, comparisons), file.path(out_dir, "calibration_attribute_comparison.csv"), row.names = FALSE)

# Count supplied error labels and prepare the private comments for synthesis.
errors <- as.data.frame(table(error = ifelse(is.na(x$main_error), "No label supplied", x$main_error)),
    stringsAsFactors = FALSE)

write.csv(errors, file.path(out_dir, "calibration_error_labels.csv"), row.names = FALSE)

# Input for a saved, isolated calibration-comments synthesis. No original clinical
# report is emitted to the terminal; only this private file contains comments.
synthesis <- x[!is.na(x$comments) | !is.na(x$main_error) | !is.na(x$corrected_diagnosis) | !is.na(x$corrected_category),
    c("workbook_row", "linkage_label", "clinician_present", "clinician_affirmative", "clinician_explains",
        "clinician_acuity", "corrected_diagnosis", "corrected_category", "main_error", "comments", "linkage_note",
        grep("^pass[12]_(normalized_diagnosis|diagnostic_category|finding_status|diagnostic_certainty|acuity|plausibly_explains_indication|evidence_valid)$",
            names(x), value = TRUE))]

write_json(synthesis, file.path(cal_dir, "private_comments_synthesis_input.json"), dataframe = "rows",
    pretty = TRUE, auto_unbox = TRUE, na = "null")

message("Calibration ingested: ", nrow(x), " candidates, ", nrow(enc), " encounters; all endpoint fields complete.")

print(summary, row.names = FALSE)
