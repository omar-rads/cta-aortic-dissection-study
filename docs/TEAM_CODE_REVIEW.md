# AAS CTA analysis code for team review

This folder brings together the R code behind the current manuscript, the published aggregate inputs used for the D-dimer sensitivity analysis, and selected aggregate results. The clinical data and report-level material are not included. The scripts were copied from the current R rebuild at commit `cb609d96ccd3a839c7506eacfe941924fda65774` on branch `codex/r-rebuild-first-pass`. The analyses were not rerun while preparing this review package. The clinical scripts R00 through R09 were tightened on November 12 without changing any result, as described in the dated history below.

## Where to start

| Manuscript component | Code | Aggregate results included |
|---|---|---|
| Cohort, ADD-RS, D-dimer and provider definitions | `R/01_import.R` through `R/04_variable_derivation.R` | `results/results_handoff/01_cohort_and_analytic_units.csv` and `03_d_dimer_and_algorithm.csv` |
| November 14 descriptive Table 2 subgroup panel | `scripts/validation/reproduce_table2_subgroups.R` | Reproduced locally against the accepted audit. See [instructions and validation](TABLE2_SUBGROUP_REVIEW.md). New outputs are not included. |
| Main clinical tables and diagnostic yield | `R/05_descriptive_analysis.R` | `results/results_handoff/02_complete_table1.csv` and `04_diagnostic_yield_and_provider_positivity.csv` |
| Provider subgroup and regression analyses | `R/06_primary_models.R`, `R/07_sensitivity_analyses.R` | `results/results_handoff/05_low_acuity_subgroup.csv`, `07_primary_regression_complete.csv`, `08_regression_and_clustering_sensitivities.csv`, and `results/yield_sensitivity_results.csv` |
| Aggregate table exports and full clinical ledger | `R/08_tables_figures.R`, `R/09_complete_results_handoff.R` | Selected clinical ledgers listed above |
| Current published-accuracy economic base case | `R/14_economic_option2_published_meta.R`, `R/economic_cost_consequence_engine.R` | `results/economic_option2_published_meta_parameters.csv` and `results/economic_option2_published_meta_results.csv` |
| ADD-RS 0-1-specific Bayesian evidence synthesis | `external_dimer_evidence/option1_conditional_bivariate/R/run_option1_conditional_bivariate.R` and its two Stan models | Published aggregate source counts, reconstruction checks, posterior draws, model summaries and diagnostics under `external_dimer_evidence/` |
| Pooled economic uncertainty and new-setting sensitivity | `R/15_economic_option1_posterior_cost_consequence.R`, `R/16_economic_option1_transportability.R` | `results/economic_option1_option2_comparison.csv` and `results/economic_option1_transportability_*.csv` |
| Exploratory non-AAS report analysis | `R/10_report_alt_dx_prepare.R` through `R/13_report_alt_dx_calibration.R`, followed by `R/17_report_alt_dx_calibration_ingest.R` through `R/19_report_alt_dx_v2_handoff.R` | Selected final v2 summaries under `results/report_alt_dx_v2/` |

The manuscript table numbers have changed since the original R export filenames were assigned. The current main economic table comes from R14, R15 and R16. These were previously named R14, R16 and R17. The old net-savings calculation remains within R07 for historical reconciliation and is not the current economic model. The older regex-based alternative-diagnosis calculation is also retained in the clinical code for provenance. The current exploratory report analysis is the frozen v2 analysis.

Some older aggregate ledgers retain the original provisional D-dimer status label. The September 26 study handoff records the investigator's later confirmation of a uniform 500 ng/mL FEU threshold across study sites and years. That confirmation did not change the existing calculations. Result status labels should be interpreted using the current manuscript and handoff, not treated as a new unresolved cutoff question.

## Reproduction requirements

Run code from the project root in a separate working copy. Several scripts write or replace derived outputs. Full clinical reproduction requires authorized access to the source workbook and derived research data. Set `RESEARCH_DATA_ROOT` locally so that `R/00_setup.R` can find `radiology/cta-aortic-dissection/raw` and `radiology/cta-aortic-dissection/derived`. No personal `.Renviron`, credentials or patient data are included here.

The clinical dependency chain is sequential. R08 sources R07, which sources R06 and the earlier stages. R09 reads the derived objects created by those stages. Do not run R08 solely to inspect a table, because it reruns the upstream clinical chain.

The economic sequence is R14, the conditional Bayesian module when new posterior draws are needed, R15, then R16. Existing posterior draws are included for review and propagation. R14, R15 and R16 have original branch checks that require `codex/r-rebuild-first-pass` in a Git working checkout. The original delivery folder did not contain `.git`. This repository review branch has Git metadata, but its branch name does not satisfy those preserved guards. The entrypoints therefore still require the intended working context and authorized data access.

The shared economic engine can be inspected and used independently with published aggregate inputs. For example, the following reproduces the base-case calculation from the displayed 43 AAS-positive and 878 AAS-negative counts without reading patient data:

```r
source("R/economic_cost_consequence_engine.R")
calculate_cost_consequence(
  aas_positive = 43,
  aas_negative = 878,
  sensitivity = 0.965,
  specificity = 0.562,
  cta_unit_cost = 634.96,
  d_dimer_unit_cost = 10.18
)
```

This example is an instruction for review, not a claim that the calculation was rerun during packaging.

The `renv.lock` file records R 4.5.1 and the saved package versions. The external Bayesian module uses `cmdstanr`, `posterior` and CmdStan 2.40.0. Its own README gives the setup and execution commands. The historical clinical runtime is not established by the lockfile alone. `lme4` is optional in R06 for the attempted mixed-model check and is not in this lockfile. That attempted model was unstable and is not the primary regression.

## Frozen exploratory report analysis

The classifier configuration, prompts, schema and execution scripts are included because they define a reported research method. Model names in these files are methodological metadata. They are separate from incidental code-authoring comments.

The finalization code checks exact source and artifact hashes. The current layout includes explanatory comments and renumbered source files. New source freezes record those current bytes. Existing frozen records are unchanged and still require their original files. The shared engine has a comment-only update, and the transportability script checks its new source hash. Stan files and saved results retain their original bytes. The Python report runners now use local configuration for data and executable locations, as described in the [scripts guide](../scripts/README.md). These configuration changes do not change classifier settings. Do not launch classification as part of ordinary manuscript review.

Reproducing the reported classifier results requires the retained private input packets, saved model responses, report-evidence maps and calibration artifacts. They are not included. Saved outputs establish reproducibility of the retained calculations. A new stochastic model run is not guaranteed to generate identical classifications.

## Scope of this folder

The selected CSV files contain aggregate counts, estimates or model draws. No encounter-level rows, reports, indications, calibration workbook, patient-level model predictions, identifier maps, secrets, Git history or private session files were copied. The original R-only team package excluded the SAS scripts and optional untracked MPPR audit. This repository snapshot additionally preserves the historical SAS scripts under `scripts/` and the earlier R checkpoint under `legacy/r-pre-rebuild/`. The untracked MPPR audit remains outside this snapshot. The source repositories and historical outputs remain intact.

Use the accompanying manuscript review notes to resolve reporting questions before submission. The existence of code and saved output does not establish clinical validation of the retrospective algorithm or safety of prospective CTA deferral.

## October 30 comment cleanup

This section describes the October 30 review copy, before the November 12 cleanup described below.

Comments were simplified in R01, R03, R04, R06, R07 and R09. The R03 header now matches the implemented primary cohort, which has no timing restriction. R07 and R09 identify the older net-savings calculation as historical reconciliation. Regression headings describe the algorithm-discordance model without presenting it as the manuscript's primary analysis. Methodological model names remain intact.

At the October 30 check, all 25 copied R scripts parsed successfully and had exactly the same parsed expressions as their source versions. Noncomment lines in the 6 edited files were unchanged. The other 78 copied source and result files were byte-identical at that check, including the frozen classifier files, shared economic engine and Stan models. No analyses were rerun, and the originals remain unchanged.

## November 12 cleanup of the clinical scripts

On November 12, R00 through R09 were restructured to remove repeated logic and narration. Cohort, exposure, outcome and model definitions were not changed, and neither were the saved results. The script names are also unchanged, because the result files cite them as provenance labels. That cleanup left R10 through R22, the shared economic engine, the Stan models, the tests and every result file at their October 30 bytes. The later numbering change is described below.

The clinical chain was rerun from the source workbook with the old and the new scripts. The six derived objects, the 23 exported CSV files and both generated handoff documents were byte-identical, and the derived objects also matched the retained September 4 files. The checks were repeated under the C locale and with `lme4` unavailable. The exploratory report analysis, the Bayesian module and the economic scripts were not rerun.

One difference is intentional. Twenty-two fixed console messages that reported no data were removed, so R09 now prints nothing when it succeeds. All counts, tables and warnings still print.

[`FILE_PROVENANCE.csv`](FILE_PROVENANCE.csv) lists the current size and hash of R00 through R09. The October 30 versions remain in the repository history at commit `b6f89a61b7b7438ae103956c420264d81b8ea947`.

## November 14 subgroup audit

The standalone Table 2 audit and synthetic tests were added in commit `1a54826`, on a branch from `main`. That update left the clinical scripts, economic modules, frozen classifier, saved result files and legacy code unchanged. The added audit reproduces the retained subgroup calculations without rerunning upstream analyses. See [Table 2 subgroup reproduction](TABLE2_SUBGROUP_REVIEW.md).


## November 14 repository cleanup at commit 9336d82

This subsection records the earlier cleanup, before the subsequent 17-19 layout update.

The two economic entrypoints were renamed from 16 to 15 and from 17 to 16. Their current script labels and navigation were updated. The calculations and original branch checks are unchanged. Existing saved result files retain their original filenames, generating-script labels and hashes. See the [R script guide](../R/README.md) for the mapping.

The frozen report-analysis paths and checks remain unchanged. The copied [v2 method specification](report_alt_dx_v2_specification.md) is byte-identical to the preserved source. The frozen change log remains outside this review package because it contains small-cell development results and case-derived descriptions that need a separate sharing decision. The frozen workflow therefore still requires that original document, in addition to authorized private inputs, to reproduce its original checks. No classifier run or new freeze was performed.

Use the [local review checks](../README.md#local-checks) to check file provenance, documentation links, the Table 2 audit, the report endpoint rules and the economic engine with synthetic inputs. These checks do not establish a complete clinical, economic or classifier rerun. Do not run historical Python classifier scripts with optimization enabled because some preservation checks use assertions.

`FILE_PROVENANCE.csv` describes the current review files. Earlier versions of that manifest and the scripts are preserved in the private project archive at commit `1a54826`. This cleanup does not change the manuscript or its conclusions.


## Continuous numbering and section notes

The remaining report-analysis stages are now 17, 18 and 19, previously 20, 21 and 22. The Python freeze source list, finalizer inventory, R source references and current documentation use those names. The numbered R scripts now run from 00 through 19. Each script has a short purpose and reminder, and the [R guide](../R/README.md) explains how the sections fit together.

The three final report stages were reformatted without changing their parsed calculations. The shared economic engine received a comment-only introduction, and its expected source hash in the transportability script was updated to that reviewed version. Source guards still reject missing or altered files. The Python runner and finalizer refuse optimization modes that would disable assertion-based checks.

No existing frozen specification, result table, model draw, clinical input or manuscript was rewritten. The current source version and its checks are distinct from the earlier frozen run. Reproducing an original run requires its recorded source version and private artifacts. A new layout does not justify silently replacing an old hash, launching another classification run or treating a new source freeze as the original prespecification. The missing historical change log remains a separate sharing limitation.

The source-layout tests exercise matching, missing and altered synthetic files, rejection of the old filenames in a new layout, preservation of the supplied manifest, and refusal of optimized Python. They do not establish a full raw-to-manuscript rerun.


## Comments within scripts and portable paths

Each of the 21 current R scripts has short purpose and reminder comments beside its main sections, in addition to the overall guide. These comments explain the local step and relevant limits. Parsed R expressions match the prior reviewed version after accounting for the single expected engine-hash update. Saved results and statistical definitions are unchanged.

The historical SAS program now takes its data directory from a local environment setting instead of a personal Windows path. Its analysis statements are unchanged, and the original source remains in the retained history. The Python report utilities take data and executable locations from local configuration. Their public inventory uses repository-relative paths. No classification run or new source freeze was performed. Existing frozen runs still require their recorded source version.
