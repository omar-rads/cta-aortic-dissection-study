# Why this script is here
# I verify the frozen v2 sources and derive the report endpoints.
# The source files must match the filenames and hashes recorded for that run.
# Frozen v2 evidence verification and endpoint derivation. The literal v1
# expressions are evaluated without its export/review stages, so v1 is intact.
source("R/00_setup.R")

suppressPackageStartupMessages({
    library(jsonlite)
    library(digest)
})

args <- commandArgs(TRUE)

stopifnot(length(args) == 1L)

run_dir <- file.path(aas_derived_dir, "report_alt_dx", args[1])

out_dir <- "results/report_alt_dx_v2"

# Verify every frozen source against the recorded hash before deriving results.
frozen <- fromJSON(file.path(run_dir, "frozen_specification.json"), simplifyVector = FALSE)

for (p in names(frozen$file_sha256)) {
    if (!file.exists(p)) {
        stop("Frozen source file is missing: ", p,
            ". Use the source snapshot recorded for this run. ",
            "The earlier layout is preserved at commit 9336d82. ",
            "Do not replace historical hashes to accept a different source version.",
            call. = FALSE)
    }
    if (!identical(digest::digest(file = p, algo = "sha256"), frozen$file_sha256[[p]])) {
        stop("Frozen source hash changed: ", p,
            ". Restore the recorded source version before reproducing this run.",
            call. = FALSE)
    }
}

# Load the v2 rules, packets, and private evidence map.
config <- fromJSON("config/report_alt_dx_v2.json")

input_paths <- sort(list.files(file.path(run_dir, "inputs"), pattern = "^RDX-.*\\.json$", full.names = TRUE))

stopifnot(length(input_paths) == 1111L)

packets <- lapply(input_paths, fromJSON, simplifyVector = FALSE)

packet_ids <- vapply(packets, `[[`, character(1), "report_audit_id")

packet_by_id <- setNames(packets, packet_ids)

report_map <- readRDS(file.path(run_dir, "private_report_evidence_map.rds"))

# Read both classification passes and check their packet coverage.
read_v2_pass <- function(p) {
    d <- file.path(run_dir, paste0("pass", p))
    manifest <- fromJSON(file.path(d, "pass_manifest.json"), simplifyVector = FALSE)
    files <- sort(list.files(d, pattern = "^batch_[0-9]+\\.json$", full.names = TRUE))
    for (f in files) stopifnot(digest(file = f, algo = "sha256") == manifest$batch_sha256[[basename(f)]])
    objs <- lapply(files, fromJSON, simplifyVector = FALSE)
    values <- unlist(lapply(objs, `[[`, "classifications"), recursive = FALSE)
    ids <- vapply(values, `[[`, character(1), "report_audit_id")
    stopifnot(length(ids) == 1111L, !anyDuplicated(ids), setequal(ids, packet_ids))
    setNames(values, ids)
}

pass1 <- read_v2_pass(1L)

pass2 <- read_v2_pass(2L)

# Exact expression boundaries fail closed if the preserved v1 implementation changes.
old <- parse("R/11_report_alt_dx_validate.R")

assigned <- vapply(old, function(e) if (is.call(e) && identical(e[[1]], as.name("<-")) && is.symbol(e[[2]])) as.character(e[[2]]) else "",
    character(1))

start <- which(assigned == "candidate_fields")

end <- which(assigned == "agreement_metrics") - 1L

stopifnot(length(start) == 1L, length(end) == 1L, start < end)

for (i in start:end) eval(old[[i]], envir = .GlobalEnv)

# No fuzzy matching, whitespace normalization, or manual candidate correction.
stopifnot(nrow(encounter_validation) == 1111L, !anyDuplicated(encounter_validation$report_audit_id))

dir.create(file.path(run_dir, "validation"), showWarnings = FALSE)

saveRDS(candidates, file.path(run_dir, "validation/private_candidate_validation.rds"))

saveRDS(encounter_validation, file.path(run_dir, "validation/private_encounter_validation.rds"))

# Export aggregate endpoint and agreement summaries after exact verification.
script <- "R/18_report_alt_dx_v2_finalize.R"

export <- function(x, name) {
    x$generating_script <- script
    write.csv(x, file.path(out_dir, name), row.names = FALSE, na = "")
}

levels <- c("positive", "negative", "uncertain")

endpoint_counts <- do.call(rbind, lapply(1:2, function(p) {
    e <- encounter_validation[[paste0("pass", p, "_endpoint")]]
    counts <- as.integer(table(factor(e, levels = levels)))
    data.frame(pass = p, analytic_role = if (p == 1)
        "Primary"
    else "Reproducibility", endpoint = c("positive", "negative", "unclassifiable"), n = counts, eligible_denominator = length(e),
        classifiable_denominator = sum(e != "uncertain"), percent_all_eligible = 100 * counts/length(e),
        percent_classifiable = c(100 * counts[1:2]/sum(e != "uncertain"), NA_real_))
}))

export(endpoint_counts, "final_endpoint_counts.csv")

transition <- as.data.frame(table(pass1 = factor(encounter_validation$pass1_endpoint, levels = levels),
    pass2 = factor(encounter_validation$pass2_endpoint, levels = levels)))

transition$pass1 <- sub("uncertain", "unclassifiable", transition$pass1)

transition$pass2 <- sub("uncertain", "unclassifiable", transition$pass2)

names(transition)[3] <- "n"

export(transition, "endpoint_transition.csv")

agreement_fields <- c(endpoint = "endpoint_agreement", diagnosis_set = "diagnosis_agreement", all_candidate_category_set = "category_agreement",
    finding_status_set = "status_agreement", certainty_set = "certainty_agreement", acuity_set = "acuity_agreement",
    plausibility_set = "plausibility_agreement", complete_structured = "complete_agreement")

agreement <- data.frame(dimension = names(agreement_fields), n_agree = vapply(agreement_fields, function(n) sum(encounter_validation[[n]]),
    integer(1)), denominator = 1111L)

agreement <- rbind(agreement, data.frame(dimension = "qualifying_category_set", n_agree = sum(encounter_validation$pass1_qualifying_category ==
    encounter_validation$pass2_qualifying_category), denominator = 1111L))

agreement$percent_agree <- 100 * agreement$n_agree/agreement$denominator

export(agreement, "agreement_statistics.csv")

# Calculate three-category agreement across the two passes.
tab <- table(factor(encounter_validation$pass1_endpoint, levels = levels), factor(encounter_validation$pass2_endpoint,
    levels = levels))

po <- sum(diag(tab))/sum(tab)

pe <- sum(rowSums(tab) * colSums(tab))/sum(tab)^2

kappa <- (po - pe)/(1 - pe)

export(data.frame(metric = c("Three-category unweighted Cohen kappa", "Endpoint disagreements", "Unclassifiable in either pass"),
    value = c(kappa, sum(!encounter_validation$endpoint_agreement), sum(encounter_validation$any_uncertain_endpoint)),
    denominator = c(NA, 1111, 1111), interpretation = "Reproducibility, not accuracy or independent clinical validation"),
    "reproducibility_summary.csv")

# Summarize qualifying categories, evidence quality, and multiplicity.
categories <- unlist(fromJSON("config/report_alt_dx_schema_v2.json", simplifyVector = FALSE)$properties$classifications$items$properties$candidates$items$properties$diagnostic_category$enum)

category_rows <- list()

quality_rows <- list()

multiplicity_rows <- list()

for (p in 1:2) {
    c <- candidates[candidates$pass == p, ]
    q <- c[c$endpoint_qualifying, ]
    npositive <- sum(encounter_validation[[paste0("pass", p, "_endpoint")]] == "positive")
    nclassifiable <- sum(encounter_validation[[paste0("pass", p, "_endpoint")]] != "uncertain")
    for (category in categories) {
        z <- q[q$diagnostic_category == category, ]
        category_rows[[length(category_rows) + 1L]] <- data.frame(pass = p, category = category, qualifying_candidate_count = nrow(z),
            encounters_with_qualifying_category = length(unique(z$report_audit_id)), positive_encounter_denominator = npositive,
            classifiable_encounter_denominator = nclassifiable, eligible_denominator = 1111L, overlap = "An encounter may contribute to multiple categories; category totals must not be summed as unique encounters")
    }
    quality_rows[[p]] <- data.frame(pass = p, metric = c("All extracted candidates", "Qualifying candidates",
        "Invalid required evidence candidates", "Encounters with any invalid required evidence", "Invalid report key candidates",
        "Report quote not in supplied text", "Report quote not in original report", "Invalid required indication quote"),
        n = c(nrow(c), nrow(q), sum(!c$evidence_valid), length(unique(c$report_audit_id[!c$evidence_valid])),
            sum(!c$report_key_valid), sum(!c$report_quote_in_supplied_text), sum(!c$report_quote_in_original_text),
            sum(!c$indication_quote_valid)), unit = c("candidate", "candidate", "candidate", "encounter",
            rep("candidate", 4)))
    nq <- table(factor(q$report_audit_id, levels = packet_ids))
    multiplicity_rows[[p]] <- data.frame(pass = p, n_qualifying = c("0", "1", ">1"), n_encounters = c(sum(nq ==
        0), sum(nq == 1), sum(nq > 1)), denominator = 1111L)
}

export(do.call(rbind, category_rows), "qualifying_diagnosis_categories.csv")

export(do.call(rbind, quality_rows), "evidence_quality.csv")

export(do.call(rbind, multiplicity_rows), "qualifying_diagnosis_multiplicity.csv")

agreement_categories <- do.call(rbind, lapply(categories, function(category) {
    ids <- lapply(1:2, function(p) unique(candidates$report_audit_id[candidates$pass == p & candidates$endpoint_qualifying &
        candidates$diagnostic_category == category]))
    a <- packet_ids %in% ids[[1]]
    b <- packet_ids %in% ids[[2]]
    data.frame(category = category, both_yes = sum(a & b), pass1_only = sum(a & !b), pass2_only = sum(!a &
        b), both_no = sum(!a & !b), denominator = 1111L)
}))

export(agreement_categories, "category_reproducibility.csv")

write_json(list(run_id = args[1], evidence_verifier = "Literal preserved v1 implementation: case-sensitive exact substrings in supplied and original report; indication exact when explanatory yes/uncertain",
    endpoint_rule = "v1 unchanged; uncertain relabeled unclassifiable only for final reporting", evaluated_v1_expression_range = c(start,
        end), v1_implementation_sha256 = digest(file = "R/11_report_alt_dx_validate.R", algo = "sha256"),
    n_encounters = 1111L, n_candidates = nrow(candidates)), file.path(out_dir, "verification_metadata.json"),
    pretty = TRUE, auto_unbox = TRUE)

message("Both v2 passes verified with the unchanged v1 evidence/endpoint implementation.")

print(endpoint_counts, row.names = FALSE)

message("Three-category Cohen kappa: ", signif(kappa, 5))
