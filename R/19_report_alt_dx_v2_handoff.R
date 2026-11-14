# Why this script is here
# I assemble the exploratory report summaries and compare them with the historical regex measure.
# Calibration agreement is not independent validation or cohort-wide accuracy.
# Final report-based results, historical-method audit, manuscript and master-record
# proposals. Run only after both frozen v2 passes and exact verification complete.
source("R/00_setup.R")

suppressPackageStartupMessages({
    library(jsonlite)
    library(digest)
})

args <- commandArgs(TRUE)

stopifnot(length(args) == 1L)

run_dir <- file.path(aas_derived_dir, "report_alt_dx", args[1])

out_dir <- "results/report_alt_dx_v2"

stopifnot(file.exists(file.path(run_dir, "pass1/pass_manifest.json")), file.exists(file.path(run_dir,
    "pass2/pass_manifest.json")))

# Load the verified v2 results and link them to the analytic cohort privately.
e <- readRDS(file.path(run_dir, "validation/private_encounter_validation.rds"))

candidates <- readRDS(file.path(run_dir, "validation/private_candidate_validation.rds"))

crosswalk <- readRDS(file.path(run_dir, "private_encounter_crosswalk.rds"))

analytic_path <- file.path(aas_derived_dir, "aas_analytic_encounters.rds")

analytic <- readRDS(analytic_path)

stopifnot(nrow(analytic) == 1193L, !anyDuplicated(analytic$encounter), nrow(e) == 1111L)

map <- merge(crosswalk, analytic[c("encounter", "aas_outcome", "alternative_diagnosis_definite", grep("^alt_",
    names(analytic), value = TRUE))], by = "encounter", all.x = TRUE, sort = FALSE)

stopifnot(nrow(map) == 1111L, all(map$aas_outcome == "Negative"), !anyNA(map$alternative_diagnosis_definite))

joined <- merge(e, map, by = "report_audit_id", all.x = TRUE, sort = FALSE)

stopifnot(nrow(joined) == 1111L, !anyNA(joined$alternative_diagnosis_definite))

# Retain a human-auditable private bridge from each frozen rule's workbook-row
# locators to the linked clinician candidate IDs. No source text is exported.
clinician_rows <- readRDS(file.path(run_dir, "calibration/private_clinician_candidate_judgments.rds"))

synthesis <- fromJSON(file.path(run_dir, "calibration/private_comments_synthesis.json"), simplifyVector = FALSE)

rule_support <- do.call(rbind, lapply(seq_along(synthesis$generalizable), function(i) {
    z <- synthesis$generalizable[[i]]
    rows <- as.integer(unlist(z$supporting_workbook_rows))
    idx <- match(rows, clinician_rows$workbook_row)
    stopifnot(!anyNA(idx))
    data.frame(rule_number = i, mechanism = z$mechanism, workbook_row = rows, candidate_id = clinician_rows$candidate_id[idx],
        report_audit_id = clinician_rows$report_audit_id[idx])
}))

write.csv(rule_support, file.path(run_dir, "calibration/private_rule_support.csv"), row.names = FALSE)

script <- "R/19_report_alt_dx_v2_handoff.R"

export <- function(x, name) {
    x$generating_script <- script
    write.csv(x, file.path(out_dir, name), row.names = FALSE, na = "")
}

# Describe the existing uncertainty signals, without reclassification or new
# inference. These attributes overlap and do not adjudicate clinical causes.
uncertainty_rows <- do.call(rbind, lapply(1:2, function(p) {
    ids <- e$report_audit_id[e[[paste0("pass", p, "_endpoint")]] == "uncertain"]
    z <- candidates[candidates$pass == p & candidates$report_audit_id %in% ids & candidates$endpoint_uncertain_signal,
        ]
    checks <- list(plausibility_uncertain = z$plausibly_explains_indication == "uncertain", finding_status_uncertain = z$finding_status ==
        "uncertain", certainty_possible_indeterminate = z$diagnostic_certainty == "possible_indeterminate",
        invalid_required_evidence = !z$evidence_valid)
    data.frame(pass = p, signal = names(checks), n_encounters = vapply(checks, function(flag) length(unique(z$report_audit_id[flag])),
        integer(1)), unclassifiable_denominator = length(ids), mutually_exclusive = FALSE, interpretation = "Saved uncertainty attributes among unclassifiable encounters; overlapping descriptors, not clinically adjudicated causes")
}))

export(uncertainty_rows, "unclassifiable_mechanisms.csv")

# Summarize calibration agreement and compare the historical and v2 endpoints.
cal_transitions <- read.csv(file.path(out_dir, "calibration_endpoint_transitions.csv"))

cal_agreement <- do.call(rbind, lapply(1:2, function(p) {
    z <- cal_transitions[cal_transitions$pass == p, ]
    data.frame(pass = p, n_agree = sum(z$Freq[z$clinician == z$classifier]), denominator = sum(z$Freq),
        interpretation = "Selected development encounters; not independent validation or a cohort accuracy estimate")
}))

export(cal_agreement, "calibration_endpoint_agreement.csv")

historical_transition <- as.data.frame(table(historical_regex = ifelse(joined$alternative_diagnosis_definite ==
    1L, "positive", "negative"), v2_pass1 = factor(joined$pass1_endpoint, levels = c("positive", "negative",
    "uncertain"))))

historical_transition$v2_pass1 <- sub("uncertain", "unclassifiable", historical_transition$v2_pass1)

names(historical_transition)[3] <- "n"

export(historical_transition, "historical_regex_transition.csv")

classified <- joined$pass1_endpoint != "uncertain"

binary_agreement <- sum((joined$alternative_diagnosis_definite[classified] == 1L) == (joined$pass1_endpoint[classified] ==
    "positive"))

historical <- data.frame(method = c("Preserved incidental-field/regex method on matched known AAS-negative cohort",
    "v2 report-based Pass 1 among classifiable", "v2 Pass 1 observed positive fraction among all eligible",
    "Binary agreement on v2-classifiable subset"), numerator = c(sum(joined$alternative_diagnosis_definite),
    sum(joined$pass1_endpoint == "positive"), sum(joined$pass1_endpoint == "positive"), binary_agreement),
    denominator = c(1111, sum(classified), 1111, sum(classified)), unclassifiable = c(0, sum(!classified),
        sum(!classified), sum(!classified)), interpretation = c("Historical implementation; not a reference standard",
        "Primary exploratory proportion; unclassifiable excluded explicitly", "Observed positive fraction; unclassifiable outcomes unresolved, not negative",
        "Agreement only; not accuracy"))

historical$percent <- 100 * historical$numerator/historical$denominator

export(historical, "historical_method_summary.csv")

# Broad compatible category unions only. Explicitly document imperfect taxonomy.
cross_categories <- list(pulmonary_vascular = list(old = "alt_pe", new = "pulmonary_vascular_thromboembolic",
    caveat = "Regex also recognizes chronic embolic disease; report endpoint excludes clearly chronic findings"),
    pulmonary_infection = list(old = "alt_pulmonary_infection", new = "pulmonary_infection_aspiration",
        caveat = "Regex includes descriptive opacity patterns without required symptom linkage"), gastrointestinal_hepatobiliary = list(old = c("alt_gi_inflammatory",
        "alt_gi_obstruction_perforation"), new = c("gastrointestinal_inflammatory", "gastrointestinal_obstructive_perforation",
        "hepatobiliary_pancreatic"), caveat = "Broad union only; hepatobiliary taxonomy differs"), genitourinary = list(old = "alt_gu_acute",
        new = "genitourinary", caveat = "Different assertion, chronicity and symptom-linkage requirements"),
    hemorrhage_trauma = list(old = "alt_hemorrhage_trauma", new = "hemorrhage_trauma", caveat = "Musculoskeletal fractures can be assigned differently"),
    non_aas_vascular = list(old = "alt_other_vascular", new = "non_aas_arterial_venous_vascular", caveat = "Different aneurysm/stenosis inclusion and chronicity handling"))

q <- candidates[candidates$pass == 1L & candidates$endpoint_qualifying, ]

category_compare <- do.call(rbind, lapply(names(cross_categories), function(n) {
    z <- cross_categories[[n]]
    a <- rowSums(joined[z$old]) > 0
    b <- joined$report_audit_id %in% q$report_audit_id[q$diagnostic_category %in% z$new]
    data.frame(comparison_group = n, historical_positive = sum(a), v2_qualifying_category = sum(b), both = sum(a &
        b), historical_only = sum(a & !b), v2_only = sum(!a & b), eligible_denominator = 1111L, interpretation = z$caveat)
}))

export(category_compare, "historical_category_comparison.csv")

# Diagnose observable reasons among historical-positive/v2-nonpositive cases.
# Nonexclusive descriptors are not clinician-confirmed causal explanations.
discrepancy_ids <- joined$report_audit_id[joined$alternative_diagnosis_definite == 1L & joined$pass1_endpoint !=
    "positive"]

reasons <- c(no_extracted_candidate = 0L, all_candidates_chronic = 0L, any_uncertain_plausibility = 0L,
    any_weak_assertion = 0L, any_invalid_evidence = 0L, all_candidates_nonexplanatory = 0L)

for (id in discrepancy_ids) {
    z <- candidates[candidates$pass == 1L & candidates$report_audit_id == id, ]
    reasons <- reasons + c(nrow(z) == 0, nrow(z) > 0 && all(z$acuity == "chronic"), any(z$plausibly_explains_indication ==
        "uncertain"), any(z$finding_status == "uncertain" | z$diagnostic_certainty == "possible_indeterminate"),
        any(!z$evidence_valid), nrow(z) > 0 && all(z$plausibly_explains_indication == "no"))
}

export(data.frame(descriptor = names(reasons), n = as.integer(reasons), denominator = length(discrepancy_ids),
    mutually_exclusive = FALSE, interpretation = "Observable v2 attributes among historical-positive/v2-nonpositive encounters; not independently adjudicated reasons"),
    "historical_discrepancy_descriptors.csv")

historical_flags <- data.frame(historical_variable = grep("^alt_", names(joined), value = TRUE))

historical_flags$n <- vapply(historical_flags$historical_variable, function(n) sum(joined[[n]]), numeric(1))

historical_flags$denominator <- 1111L

export(historical_flags, "historical_category_inventory.csv")

# Store a private encounter comparison with no report text or native identifiers.
private_comparison <- joined[c("report_audit_id", "pass1_endpoint", "pass1_qualifying_category", "alternative_diagnosis_definite",
    grep("^alt_", names(joined), value = TRUE))]

write.csv(private_comparison, file.path(run_dir, "validation/private_historical_comparison.csv"), row.names = FALSE)

# Read the checked aggregate outputs used in the handoff.
counts <- read.csv(file.path(out_dir, "final_endpoint_counts.csv"))

agreement <- read.csv(file.path(out_dir, "agreement_statistics.csv"))

repro <- read.csv(file.path(out_dir, "reproducibility_summary.csv"))

quality <- read.csv(file.path(out_dir, "evidence_quality.csv"))

categories <- read.csv(file.path(out_dir, "qualifying_diagnosis_categories.csv"))

multiplicity <- read.csv(file.path(out_dir, "qualifying_diagnosis_multiplicity.csv"))

cal <- read.csv(file.path(out_dir, "calibration_summary.csv"))

freeze <- fromJSON(file.path(run_dir, "frozen_specification.json"), simplifyVector = FALSE)

p1 <- counts[counts$pass == 1, ]

p2 <- counts[counts$pass == 2, ]

getn <- function(p, s) as.integer(p$n[p$endpoint == s])

p <- getn(p1, "positive")

neg <- getn(p1, "negative")

u <- getn(p1, "unclassifiable")

d <- p + neg

p_2 <- getn(p2, "positive")

n_2 <- getn(p2, "negative")

u_2 <- getn(p2, "unclassifiable")

d_2 <- p_2 + n_2

agree <- agreement$n_agree[agreement$dimension == "endpoint"]

kappa <- repro$value[repro$metric == "Three-category unweighted Cohen kappa"]

catagree <- agreement$n_agree[agreement$dimension == "all_candidate_category_set"]

qcatagree <- agreement$n_agree[agreement$dimension == "qualifying_category_set"]

nameagree <- agreement$n_agree[agreement$dimension == "diagnosis_set"]

tupleagree <- agreement$n_agree[agreement$dimension == "complete_structured"]

invalid <- function(pass, metric) quality$n[quality$pass == pass & quality$metric == metric]

ev1 <- invalid(1, "Encounters with any invalid required evidence")

ev2 <- invalid(2, "Encounters with any invalid required evidence")

mult <- multiplicity$n_encounters[multiplicity$pass == 1 & multiplicity$n_qualifying == ">1"]

pc <- function(n, d) sprintf("%.1f", 100 * n/d)

mdtable <- function(x) {
    if ("percent" %in% names(x))
        x$percent <- sprintf("%.1f", x$percent)
    x[] <- lapply(x, as.character)
    c(paste0("| ", paste(names(x), collapse = " | "), " |"), paste0("| ", paste(rep("---", ncol(x)),
        collapse = " | "), " |"), apply(x, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")),
        "")
}

# Keep representation agreement separate from clinical accuracy in the interpretation.
representation_note <- sprintf("Exact normalized diagnosis-name-set agreement was %d/1,111 (%s%%), and complete seven-attribute candidate-tuple agreement was %d/1,111 (%s%%). These strict representation measures are sensitive to wording, extraction and grouping differences across all candidates, including nonqualifying findings; they are not clinical error rates. High endpoint agreement therefore does not establish interchangeability of every diagnosis-level classification.",
    nameagree, pc(nameagree, 1111), tupleagree, pc(tupleagree, 1111))

representation_note <- paste0(representation_note, " Qualifying-category-set agreement includes ", sum(e$pass1_qualifying_category ==
    "" & e$pass2_qualifying_category == ""), "/1,111 encounters with empty qualifying sets in both passes; high category agreement is therefore not evidence of high diagnostic accuracy.")

placement <- sprintf("Report the detailed exploratory analysis in the Supplement, with only a brief main-text cross-reference if useful. With %d/1,111 (%s%%) unclassifiable and no independent clinical validation, the %d/%d classifiable-subset estimate should not be presented as full-cohort prevalence or used as evidence of CTA benefit. Omit the alternative-diagnosis estimate from the abstract.",
    u, pc(u, 1111), p, d)

main_methods <- "In an exploratory secondary analysis of AAS-negative encounters, a language-model classifier calibrated using 25 clinician-reviewed development encounters identified non-AAS CTA diagnoses potentially explaining documented presenting symptoms. A frozen revised classifier was applied in two independent passes; Pass 1 defined the analysis, unresolved encounters remained unclassifiable, and Pass 2 assessed reproducibility."

main_results <- sprintf("Among 1,111 AAS-negative encounters, %d were positive, %d negative, and %d (%s%%) unclassifiable for a non-AAS imaging diagnosis potentially explaining the presenting symptoms. The primary proportion was %d/%d (%s%%) among classifiable encounters. Independent-pass endpoint agreement was %d/1,111 (%s%%; Cohen kappa, %.3f).",
    p, neg, u, pc(u, 1111), p, d, pc(p, d), agree, pc(agree, 1111), kappa)

discussion <- "CTA reports contained non-AAS imaging diagnoses that could plausibly relate to documented symptoms in a subset of classifiable encounters. This exploratory estimate depends on the completeness of the recorded symptoms and the interpretation of report language. The result does not establish symptom causation, clinical confirmation, management change, improved outcomes, or the necessity or appropriateness of CTA."

limitation <- sprintf("Classification was report-based and used a language model calibrated on 25 selected difficult encounters, without complete clinician adjudication or an independent clinical validation sample. Pass agreement measures reproducibility rather than correctness; %d/1,111 encounters remained unclassifiable in the primary pass. Missing symptom context, shared model errors, and available-text limitations may affect both the numerator and which encounters enter the classifiable denominator.",
    u)

supplement <- c("This post hoc exploratory secondary analysis included the 1,111 encounters with a known negative abstracted AAS outcome from the validated 1,193-encounter adult cohort. It did not change the primary AAS, ADD-RS, D-dimer, provider, regression or economic analyses.",
    "The source was the final CTA report and documented clinical indication. The final IMPRESSION was used where available, with full-report fallback for inadequate/missing impressions. Identical component reports were deduplicated; distinct reports retained chest/abdomen-pelvis labels. Pseudonymous packets excluded native patient identifiers, provider, hospital/site, algorithm concordance, incidental-field text, historical regex output and manuscript results. Existing extraction supplied at least one Impression for 1,110 encounters and a full-report fallback for 1 encounter.",
    "Version 1 underwent two complete independent passes. A deterministic purposeful development sample of 25 difficult encounters comprised 10 endpoint-disagreement cases, 10 recurring plausibility-uncertainty cases and 5 additional edge cases. The investigator completed 84 linked candidate reviews. Blank encounter-summary fields were intentional; encounter judgments were derived from candidate judgments. Six development encounters were positive, 2 negative and 17 uncertain. These selected cases are calibration data, not independent validation, and were retained in the full eligible analytic cohort rather than substituted with clinician labels.",
    "One v2 prompt revision addressed extraction separately from relevance, actual symptoms versus diagnostic concerns, assertion separately from probability, report-supported acuity, encounter-specific explanatory linkage, conservative diagnostic specificity/composite representation, category mapping, and verbatim span selection. The schema and scientific endpoint were unchanged. Prompt/schema/configuration hashes and the final analysis specification were frozen before any v2 cohort inference; no further tuning or v3 was performed.",
    "Each extracted candidate specified diagnosis, category, component, finding status, certainty, acuity, explanatory relevance, exact report/indication quotes, and source keys. A qualifying candidate required case-sensitive exact evidence in the supplied and original report; present status; definite/probable certainty; explanatory Yes; and acute/likely-acute/indeterminate acuity. Clearly chronic candidates were excluded. At least one qualifying candidate made an encounter positive. Otherwise a nonchronic potentially explanatory candidate with unresolved certainty, relevance or invalid evidence made it uncertain, reported as unclassifiable; remaining encounters were negative. No uncertain encounter was recoded as negative.",
    "Both passes requested gpt-6-astra with high reasoning in fresh ephemeral CLI sessions and isolated working directories; Pass 2 independently shuffled encounter order and had no Pass 1 answers. CLI version, execution dates, prompt/schema hashes, requested model and any exposed backend identifiers are recorded in the runtime outputs. A specific backend snapshot and temperature were not available where the runtime did not expose them. Each response was schema/coverage-checked, and evidence was verified without fuzzy matching or whitespace repair. Execution/schema failures could be retried; evidence failures were retained for analysis and never manually patched.",
    "Pass 1 was designated primary before inference. The positive proportion uses positive plus negative encounters as denominator, with unclassifiable and total eligible counts separately reported. Category tables show qualifying candidate counts and unique encounters per category; categories overlap. Pass 2 is a reproducibility result, including its own proportion, three-state endpoint agreement, unweighted Cohen kappa, category-set agreement and evidence-failure counts. Attribute-specific agreement compares sets of distinct values, not matched individual candidates. Complete structured agreement compares normalized seven-attribute candidate tuples and does not require identical evidence quotations. Endpoint agreement incorporates the evidence-derived endpoint. Agreement is not accuracy, and no formal independent validation claim is made.",
    "Only after v2 completion was the preserved incidental-field/regex method compared on the same eligible cohort. Binary agreement uses the v2-classifiable subset; unclassifiable comparisons are shown separately. Broad compatible category unions are used where interpretable; different taxonomies and definitions limit equivalence. Historical classifications were never used as a gold standard or to tune v2.")

# Assemble the proposed manuscript text and study record updates.
manuscript <- c("# Exploratory non-AAS imaging diagnoses: manuscript language", "", "## Placement recommendation",
    "", placement, "", "## Short main-text Methods", "", main_methods, "", "## Main-text Results (if retained)",
    "", main_results, "", "## Complete eMethods", "", paste(supplement, collapse = "\n\n"), "", "## Discussion insertion",
    "", discussion, "", "## Limitation", "", limitation, "", representation_note, "")

while (length(manuscript) > 0L && !nzchar(tail(manuscript, 1L))) manuscript <- head(manuscript, -1L)

writeLines(manuscript, "docs/report_alt_dx_v2_manuscript_language.md", useBytes = TRUE)

primary_categories <- categories[categories$pass == 1, c("category", "qualifying_candidate_count", "encounters_with_qualifying_category",
    "positive_encounter_denominator", "classifiable_encounter_denominator")]

handoff <- c("# Final exploratory report-based non-AAS imaging-diagnosis handoff", "", "## Final result",
    "", main_results, "", sprintf("Pass 2: %d positive, %d negative, %d unclassifiable; %d/%d (%s%%) among classifiable encounters. Pass 1's observed positive fraction among all eligible encounters is %d/1,111 (%s%%), with %d unresolved encounters explicitly retained.",
        p_2, n_2, u_2, p_2, d_2, pc(p_2, d_2), p, pc(p, 1111), u), "", sprintf("Endpoint disagreements: %d/1,111. Exact all-candidate category-set agreement: %d/1,111 (%s%%). Qualifying-category-set agreement: %d/1,111 (%s%%). Encounters with any invalid required evidence: Pass 1 %d/1,111; Pass 2 %d/1,111. Pass 1 has %d encounters with more than one qualifying candidate.",
        1111 - agree, catagree, pc(catagree, 1111), qcatagree, pc(qcatagree, 1111), ev1, ev2, mult),
    "", sprintf("Pass 1 extracted %d candidates, including %d qualifying candidates across %d positive encounters. Pass 2 extracted %d candidates, including %d qualifying candidates across %d positive encounters. Candidate counts are not unique-encounter counts.",
        invalid(1, "All extracted candidates"), invalid(1, "Qualifying candidates"), p, invalid(2, "All extracted candidates"),
        invalid(2, "Qualifying candidates"), p_2), "", "Generating objects: `encounter_validation`, `candidates`, `endpoint_counts`, `agreement`, `quality_rows`, `category_rows` and `multiplicity_rows` in `R/18_report_alt_dx_v2_finalize.R`.",
    "", "## Qualifying categories", "", mdtable(primary_categories), "Candidate counts count model representations; distinct candidates can be related findings. Encounter-category counts are deduplicated within category, but categories overlap.",
    "", "## Historical method comparison", "", mdtable(historical), mdtable(historical_transition), "The preserved regex outcome applies a different construct to the abstracted incidental field. It lacks the v2 requirement for report assertion, nonchronic timing and documented symptom linkage. This is the preserved R regex implementation, not a claim to reproduce the August 2 SAS endpoint. Different definitions and classifiable denominators mean these are not directly comparable full-cohort prevalence estimates. The comparison is an audit, not clinical validation. Observable discrepancy descriptors and compatible category unions are in the associated CSVs; no causal error attribution is claimed.",
    "", "## Clinician calibration", "", mdtable(cal[c("metric", "n")]), mdtable(cal_agreement), "The 84 rows form a linked union of the two v1 passes, including explicitly shared composite source candidates. Pass 1 omitted 11/84 linked rows and Pass 2 omitted 17/84. On represented rows, sufficient-affirmation disagreements were 8/73 and 9/67; acuity disagreements 4/73 and 6/67; plausibility disagreements 12/73 and 11/67. These are purposeful difficult-case descriptions, not cohort error rates. All 84 were judged mentioned/present in the report, but sufficient diagnostic affirmation remained uncertain for 29. No false report-mention extractions were identified among these reviewed rows; this does not establish full-cohort extraction accuracy. The raw report-presence versus model status mismatch must not be interpreted as 35 or 32 false extractions or a separately validated finding-status error rate. Status and certainty are assessed jointly through sufficient affirmation; the comment review supplies the qualitative distinction.",
    "", "Candidate-level clinician corrections identified 5 diagnosis and 4 category edits. One main error label identified candidate representation. Category corrections are reported as clinician edits, not a mechanically inferred pass-specific error rate from free-text labels. Exact-evidence failures involved 1/73 represented Pass 1 rows and 2/67 represented Pass 2 rows. The comment synthesis distinguishes recurring mechanisms from isolated judgments and endpoint-neutral corrections. The exact 25-row clinician endpoint table remains private at `calibration/clinician_encounter_endpoints_25.csv` under the v2 run directory.",
    "", "## Remaining limitations and recommendation", "", mdtable(uncertainty_rows[c("pass", "signal",
        "n_encounters", "unclassifiable_denominator")]), limitation, "", representation_note, "", placement,
    "", "## Reproduction and closure", "", paste0("Run: `", args[1], "`. Frozen UTC: ", freeze$frozen_utc,
        ". Primary pass: 1. Reproducibility pass: 2."), "", "See the frozen specification, v1-to-v2 change log, complete prompts/schema, runtime metadata, and preservation validation. v1 and primary analyses are preserved. This alternative-diagnosis analysis is closed after the final preservation gate; no v3, further calibration, or additional sensitivity analysis is planned unless a hard technical defect is discovered.")

writeLines(handoff, "docs/report_alt_dx_v2_final_handoff.md", useBytes = TRUE)

issues <- data.frame(issue = c("Source field and construct ambiguity", "Report/indication information firewall",
    "Separation from D-dimer-dependent primary analyses", "Historical clinically meaningful terminology",
    "Classifier v1 ambiguity", "Missing clinician encounter-summary cells", "Incomplete clinical validation",
    "Unclassifiable encounters", "Model inference reproducibility", "Historical-vs-report-based discrepancy"),
    status = c("resolved", "resolved", "resolved for this isolated endpoint", "superseded", "partially resolved",
        "resolved", "open limitation", "open limitation", "partially resolved", "resolved as documented audit"),
    disposition = c("Final reports plus documented symptoms; exploratory secondary endpoint", "Saved allowlisted packets and prohibited-field checks",
        "No D-dimer/algorithm input or primary-analysis changes", "Replace with non-AAS imaging diagnoses potentially explaining presenting symptoms",
        "One calibrated frozen v2 revision; residual uncertainty reported", "Derived deterministically from 84 completed candidate reviews",
        "25 selected development cases are not independent validation; no further review planned", "Retained explicitly and excluded from classifiable denominator",
        "Saved outputs/hashes reproduce calculations; model regeneration may vary", "Preserve historical values alongside new operational definition; no gold-standard claim"))

export(issues, "issue_resolution_register.csv")

master <- c("# Proposed Canonical Study Record addendum", "", "Append this addendum; do not delete or overwrite the historical alternative-diagnosis statements or numbers.",
    "", "## Operational definition and status", "", "Exploratory secondary/post hoc analysis of a non-AAS imaging diagnosis reported on CTA that could plausibly explain at least one documented presenting symptom. It is not a clinical-confirmation, management-benefit, causation, or appropriateness endpoint. The frozen v1 scientific endpoint was retained in v2.",
    "", "## Calibration history", "", "Two v1 passes were followed by purposeful clinician review of 84 linked candidates from 25 difficult encounters. Derived clinician endpoints were 6 positive, 2 negative and 17 uncertain. A single v2 prompt clarification was frozen before full-cohort inference. The schema was unchanged. Development cases are not independent validation. No v3 or further clinician-review cycle was undertaken.",
    "", "## Final counts and reproducibility", "", main_results, "", sprintf("Pass 2: %d/%d positive (%s%%), with %d unclassifiable. Evidence-failure encounters: %d in Pass 1 and %d in Pass 2. Full runtime and category tables accompany the handoff.",
        p_2, d_2, pc(p_2, d_2), u_2, ev1, ev2), "", "## Remaining limitations", "", limitation, "", representation_note,
    "", "## Contradiction and terminology history", "", sprintf("The preserved incidental-field/regex implementation yields %d/1,111 on its own definition. The final report-based result is %d/%d with %d unclassifiable. These differ in source text, assertion, chronicity and symptom-linkage requirements; neither is silently substituted for the other. Historical claims of clinically meaningful alternative diagnoses are superseded by the narrower report-based terminology and retained only as historical statements.",
        sum(joined$alternative_diagnosis_definite), p, d, u), "", "## Issue disposition", "", mdtable(issues),
    "## Manuscript placement", "", placement, "", "## Closure", "", "The alternative-diagnosis analysis is closed. Residual methodological limitations are reported. Reopen only for a demonstrated hard technical defect, not to optimize prevalence, significance or narrative.")

writeLines(master, "docs/report_alt_dx_v2_canonical_record_addendum.md", useBytes = TRUE)

# Record the frozen run details alongside the handoff outputs.
metadata <- list(run_id = args[1], calibration_workbook_sha256 = freeze$calibration_workbook$workbook_sha256,
    analytic_rds_sha256 = digest(file = analytic_path, algo = "sha256"), finalizer_sha256 = digest(file = "R/18_report_alt_dx_v2_finalize.R",
        algo = "sha256"), handoff_script_sha256 = digest(file = script, algo = "sha256"), historical_object = "aas_analytic_encounters.rds: alternative_diagnosis_definite and alt_*",
    primary_analysis_modified = FALSE, version_3_created = FALSE)

write_json(metadata, file.path(out_dir, "final_handoff_provenance.json"), pretty = TRUE, auto_unbox = TRUE)

message("Final exploratory handoff, historical audit, manuscript language and proposed canonical addendum generated.")

print(historical, row.names = FALSE)
