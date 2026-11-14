# CTA for suspected acute aortic syndrome

R analysis code for a retrospective study of emergency department CT angiography. The repository includes the clinical analysis, cost-consequence model, published D-dimer evidence, exploratory report-analysis method and selected aggregate results. Patient-level data and report text are stored separately.

## Start here

| Review task | Files |
| --- | --- |
| Find a script and understand its role | [R script guide](R/README.md) |
| Follow the manuscript analyses | [Code and result map](docs/TEAM_CODE_REVIEW.md#where-to-start) |
| Reproduce the descriptive Table 2 subgroup panel | [Subgroup audit instructions](docs/TABLE2_SUBGROUP_REVIEW.md) |
| Review the economic calculations and source costs | [Shared engine](R/economic_cost_consequence_engine.R) and [CMS source verification](docs/cost_sources/SOURCE_VERIFICATION_RECORD.md) |
| Check the code package without study data | [Local checks](#local-checks) |
| Review the earlier implementations | [Historical SAS scripts](scripts/README.md) and [earlier R checkpoint](legacy/r-pre-rebuild/) |

This public snapshot contains the current review code on the `main` branch. It includes the annotated R scripts, the earlier SAS implementation and selected aggregate outputs.

## Analysis modules

- **Clinical analysis:** scripts 00-09 import and clean the workbook, construct the cohort, derive variables, fit the models and export the clinical tables.
- **Economic analysis:** scripts 14-16 apply the shared cost-consequence engine to the published accuracy estimates and saved posterior draws. The earlier net-savings calculations in script 07 are retained for historical reconciliation and do not supply the current economic table.
- **Exploratory report analysis:** scripts 10-13 and stages 17-19 document report preparation, clinician calibration and final verification. See the [script guide](R/README.md) before running them.
- **Published evidence:** [external_dimer_evidence/](external_dimer_evidence/) contains aggregate literature inputs, the Bayesian model, saved posterior draws and diagnostics.
- **Saved results:** [results/](results/) contains selected aggregate outputs from the reviewed analysis. Their original provenance labels are retained.

## Local checks

Run these commands from the repository root with Python 3 and R available:

```sh
python3 scripts/validation/check_review_package.py
python3 tests/test_review_package.py
Rscript --vanilla tests/test_economic_cost_consequence_engine.R
Rscript --vanilla tests/test_report_alt_dx_v2.R
Rscript --vanilla tests/test_report_frozen_sources.R
python3 tests/test_report_source_layout.py
python3 tests/test_report_portable_paths.py
Rscript --vanilla tests/test_table2_subgroups.R
```

These checks validate the files listed in the provenance manifest and exercise synthetic examples. Link checks cover local path targets in this README, the R script guide and the team review guide. Section anchors and web links are not checked. They do not read patient data, call a model or regenerate the manuscript results. The Table 2 test creates artificial records in a temporary directory. The report-rule test requires the `digest` and `jsonlite` packages used by the preserved verifier.

## Reproduction requirements

See the [team review guide](docs/TEAM_CODE_REVIEW.md#reproduction-requirements) for input directories and execution order. Run analyses only in a separate working copy with separately authorized data access. Script 08 reruns the clinical dependency chain, and several entrypoints write derived outputs.

The economic entrypoints retain their original branch checks. The report-analysis workflow checks the current 17-19 paths and exact file hashes. Historical runs still require the source version named in their frozen records. These controls are not bypassed by the review checks above. Original frozen runs require their original source files and private input artifacts. The current report runner and finalizer explicitly reject Python optimization (`-O`, `-OO` or `PYTHONOPTIMIZE`) so their assertion-based checks cannot be disabled.

The lockfile records R 4.5.1 and saved package versions. The published-evidence model additionally requires CmdStan and its toolchain. A complete raw-to-manuscript rerun has not been established for this package.

## Version history and citation

The [review guide](docs/TEAM_CODE_REVIEW.md) records the October 30 package, November 12 clinical cleanup and November 14 subgroup audit. The [reconciliation record](docs/CODE_RECONCILIATION_2026-11-14.md) explains which older local files were retained separately. The original numbering and pre-layout snapshots are preserved in the private project archive. The current layout runs from 00 through 19, with explanatory comments and updated source references.

[CITATION.cff](CITATION.cff) identifies this review snapshot. The [Zenodo version DOI](https://doi.org/10.5281/zenodo.17228961) identifies the older SAS v1.0.2 archive. The [concept DOI](https://doi.org/10.5281/zenodo.17161677) groups versions of that record. Neither identifies a published release of this R snapshot.

The license decision remains pending. The older archive has conflicting MIT and CC BY 4.0 metadata, so no license has been assigned to this review version.
