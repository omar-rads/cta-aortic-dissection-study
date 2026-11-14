# Frozen v2 specification: final exploratory report-based analysis

Source-layout note, November 14: executable paths below use the current 17-19 numbering. The study definitions and original run records are unchanged. The previous text and filenames remain available at commit `9336d82`. This source update is not a new calibration or classification run.

The author authorized one final calibration/reanalysis cycle on 2026-09-23. Version 2 is the only classifier revision authorized. Version 1, the completed clinician workbook, primary AAS analyses, ADD-RS/D-dimer/cohort/provider/regression/economic implementations, and historical results remain unchanged. No additional clinician audit or v3 is planned.

## Calibration and scientific endpoint

The completed candidate workbook contains 84 linked candidate rows from 25 purposefully selected development encounters. Candidate-level judgments are authoritative; the blank Encounter Summary is intentional. A clinician candidate qualifies with Yes for report presence, sufficient affirmation, and symptom plausibility plus Acute/Likely acute/Indeterminate acuity. Any definitive No or Chronic excludes it. Without an exclusion, unresolved relevant judgments leave it unresolved. Any qualifying candidate makes the encounter positive; otherwise any unresolved potentially qualifying candidate makes it uncertain; otherwise negative. The 25 remain development cases and are never called independent validation.

The automated endpoint is the exact preserved v1 implementation in `R/11_report_alt_dx_validate.R`: valid literal evidence, present status, definite/probable certainty, explanatory yes, and acute/likely-acute/indeterminate acuity qualify. With no qualifier, a nonchronic potentially explanatory candidate with uncertainty/possible certainty/invalid evidence yields uncertain. Otherwise negative. `R/18_report_alt_dx_v2_finalize.R` evaluates those unchanged expressions while bypassing all v1 write/review stages. Uncertain is relabeled unclassifiable only in final reports; it is never silently negative.

## Prespecified choices for this final v2 run

These choices are frozen for v2 before inference; they do not make the analysis originally prespecified.

- All 1,111 known AAS-negative eligible encounters use the exact existing v1 input packets and pseudonym mapping. The original Impression extraction, date redaction, exact report deduplication and information firewall are retained.
- The v2 JSON schema is byte-for-byte identical to v1 under a versioned filename. Only prompt clarifications supported by the calibration and author principles change.
- Model request: `gpt-6-astra`, high reasoning. Four concurrent batches, at most 12 encounters or 85,000 input characters per batch. CLI version and available backend metadata are recorded. Temperature is not exposed/configured.
- Pass 1 is the primary analysis. Pass 2 uses separately shuffled order and fresh ephemeral sessions, with no pass-1 answers, clinician decisions, or downstream variables. Each session receives only the frozen prompt/schema and allowlisted report/indication packet.
- Failed execution/schema/coverage responses can be retried up to three attempts; original logs are retained privately. Exact-evidence failure is an analysis finding, not a reason to retry or manually fix a candidate.
- Verification is case-sensitive literal matching of report evidence in both the supplied extract and original full report, plus indication evidence when relevant. No fuzzy matching or whitespace repair occurs in the verifier.
- The primary positive proportion is positive/(positive + negative), with positive, negative, unclassifiable and total eligible counts separately reported. Positive/1,111 may be shown only as the observed positive fraction with unclassifiable separately visible. No assumption is made about unclassifiable outcomes.
- Category tables report qualifying candidate counts and unique encounters with each qualifying category. Multiple diagnoses/categories can overlap. Report encounters with more than one qualifying candidate separately.
- Reproducibility includes the three-state transition table, unweighted Cohen kappa, exact endpoint/category-set agreement, category presence comparisons, evidence failures, and pass-2 proportions. It is not clinical validation.
- Historical regex comparison is performed only after both passes are complete, with each method's own denominator and explicit unclassifiable handling. The old method is not a reference standard.
- Remaining uncertainty is reported. Manuscript placement is decided from interpretability, coverage, and reproducibility, never followed by another tuning cycle.

## Reproduction and provenance

Run `R/17_report_alt_dx_calibration_ingest.R` with the completed workbook and v2 run ID. Saved comment synthesis supports the documented general rules; it cannot override clinician judgments. Freeze with `scripts/report_alt_dx/run_classifier_v2.py freeze` before running `pass1` or `pass2`, then run `R/18_report_alt_dx_v2_finalize.R`. The freeze manifest hashes the prompt, schema, controls, endpoint implementation, calibration workbook, and frozen specification before cohort inference. Private source, crosswalks, clinician rows, logs, and outputs remain outside Git. Preservation manifests verify v1 and primary files at completion.

CLI execution follows the official [non-interactive execution documentation](https://learn.chatgpt.com/docs/non-interactive-mode). Fresh ephemeral sessions reduce shared conversational context; two model passes still share a model and rubric and can share systematic errors. Exact backend snapshots may be unavailable, so saved outputs and hashes reproduce the reported analysis, while bit-for-bit regenerated inference is not guaranteed.
