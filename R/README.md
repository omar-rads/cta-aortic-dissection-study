# R script guide

Here's how I've organized the code and what I need to remember when I come back to it. The numbers are labels, not an instruction to run every script in sequence. The clinical, economic, and exploratory report analyses have different inputs and dependencies.

Each script also has short comments beside its main sections. Those notes explain what I'm doing there and what I need to keep in mind when I read the results. The [historical SAS guide](../scripts/README.md) explains the earlier implementation and the move to R.

## Clinical analysis: 00-09

The clinical cohort consists of adult encounters with both chest and abdomen/pelvis CTA components. The unit is the encounter, so a patient can contribute more than one eligible encounter. Because everyone in this cohort underwent CTA, these analyses do not describe all patients evaluated for possible acute aortic syndrome.

| Script | What I use it for |
| --- | --- |
| [00_setup.R](00_setup.R) | Find and check the external research-data directories. |
| [01_import.R](01_import.R) | Read the source workbook and check its structure before recoding. |
| [02_cleaning.R](02_cleaning.R) | Clean source fields and examine how identifiers and rows relate. |
| [03_cohort_construction.R](03_cohort_construction.R) | Assemble the eligible encounter cohort. The order-time interval is a sensitivity flag, not a primary cohort restriction. |
| [04_variable_derivation.R](04_variable_derivation.R) | Create encounter-level ADD-RS, D-dimer, provider, outcome, and other analysis variables using the documented attribution rules. |
| [05_descriptive_analysis.R](05_descriptive_analysis.R) | Summarize characteristics, concordance, diagnostic yield, and provider comparisons. |
| [06_primary_models.R](06_primary_models.R) | Fit the algorithm-discordance regression and examine clustering. |
| [07_sensitivity_analyses.R](07_sensitivity_analyses.R) | Check how selected assumptions affect results and retain historical reconciliation calculations. |
| [08_tables_figures.R](08_tables_figures.R) | Run the clinical chain through 07 and export aggregate tables and manuscript values. |
| [09_complete_results_handoff.R](09_complete_results_handoff.R) | Read saved derived objects and assemble an aggregate results ledger. |

ADD-RS is reconstructed from available documentation. If I can't identify a D-dimer result, I can't call it negative or assume the test was never ordered. Provider categories describe the recorded ordering attribution and do not by themselves establish independent decision-making or supervision. These observational comparisons are associations, and algorithm discordance alone does not establish that imaging was unnecessary or safely avoidable.

## Exploratory report analysis: 10-13 and 17-19

This separate analysis examines non-AAS diagnoses in reports from eligible AAS-negative encounters. The older regex-based measure remains in the clinical code for historical comparison. The report-based classifier and its calibration are exploratory. Calibration on selected development cases is not independent validation or a cohort-wide accuracy estimate.

| Script | What I use it for |
| --- | --- |
| [10_report_alt_dx_prepare.R](10_report_alt_dx_prepare.R) | Prepare report-analysis packets and keep the private linkage outside the repository. |
| [11_report_alt_dx_validate.R](11_report_alt_dx_validate.R) | Check the two classification passes and their evidence, derive the preserved endpoint, and prepare clinician review. |
| [12_report_alt_dx_results.R](12_report_alt_dx_results.R) | Enforce the clinician-review gate before final prevalence or historical comparison. |
| [13_report_alt_dx_calibration.R](13_report_alt_dx_calibration.R) | Prepare selected cases and ambiguity summaries for clinician calibration. |
| [17_report_alt_dx_calibration_ingest.R](17_report_alt_dx_calibration_ingest.R) | Ingest the completed clinician calibration workbook. Previously named 20. |
| [18_report_alt_dx_v2_finalize.R](18_report_alt_dx_v2_finalize.R) | Verify frozen inputs and derive the v2 endpoints. Previously named 21. |
| [19_report_alt_dx_v2_handoff.R](19_report_alt_dx_v2_handoff.R) | Assemble report-analysis summaries and the historical-method comparison. Previously named 22. |

The original frozen manifests and checks still refer to the original names and hashes. The current runner uses the new filenames when recording a source freeze. An older frozen record still requires its original filenames and hashes. I keep that record intact rather than replacing its hashes to make newer code pass. The original source layout is preserved in the private project archive at commit `9336d82`. Consult the [team review guide](../docs/TEAM_CODE_REVIEW.md#frozen-exploratory-report-analysis) for the private artifacts and other requirements. Renaming these scripts does not establish a full historical rerun.

## Economic analysis: 14-16 and the shared engine

This is where I model what testing would cost under the different strategies and how many false-negative AAS events each strategy could produce. I need both pieces when I interpret the results.

| Script | What I use it for |
| --- | --- |
| [14_economic_option2_published_meta.R](14_economic_option2_published_meta.R) | Apply the published-accuracy base case. |
| [15_economic_option1_posterior_cost_consequence.R](15_economic_option1_posterior_cost_consequence.R) | Propagate saved posterior accuracy draws and compare the two options. Previously named 16. |
| [16_economic_option1_transportability.R](16_economic_option1_transportability.R) | Examine the new-setting sensitivity analysis. Previously named 17. |
| [economic_cost_consequence_engine.R](economic_cost_consequence_engine.R) | Calculate costs and consequences from supplied counts, test accuracy, and unit costs. |

The economic results separate modeled false negatives from costs and imaging use. They do not establish the safety of withholding CTA. The earlier net-savings calculation in 07 is retained for historical reconciliation and is not the current economic model. The Bayesian evidence module is separate, and the economic entrypoints retain their original branch and source checks. See [reproduction requirements](../docs/TEAM_CODE_REVIEW.md#reproduction-requirements).

## Before running anything

Run from a separate working copy with authorized data and the required environment. Several scripts write or replace outputs. Script 08 reruns the upstream clinical chain, while 09 reads saved derived objects. The report workflow also requires retained private inputs and calibration artifacts. Existing results and source files have their own provenance, and a change in layout does not verify a new clinical, economic, or classifier run. For ordinary inspection, start with the [local checks](../README.md#local-checks) and the [team review guide](../docs/TEAM_CODE_REVIEW.md).
