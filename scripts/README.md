# Historical SAS and supporting scripts

I started this project in SAS, then moved the manuscript analysis to R. I keep the SAS files here so I can trace where the project started. The current R analysis also includes the rebuilt cohort, revised definitions and cost-consequence model, so this is not a claim that the two versions produce the same results line for line.

## Historical SAS files

- [New_CTA.sas](New_CTA.sas) is the earlier SAS analysis. It reads an external dataset called ASER and contains the earlier recoding and analyses. It does not generate the current manuscript results. Set `SAS_DATA_ROOT` locally to the folder containing that dataset. The original computer path has been removed, and the script stops if the setting is missing. The analysis statements are unchanged.
- [_config.sas](_config.sas) and [formats.sas](formats.sas) are the earlier setup files.
- [run_all.sas](run_all.sas) is a historical scaffold with placeholder includes. It is not a complete way to rerun the current study.

For the analysis behind the current manuscript, start with the [R script guide](../R/README.md). The [earlier R checkpoint](../legacy/r-pre-rebuild/README.md) is also preserved separately.

## Current supporting utilities

The other folders here support the current R work. They are not part of the historical SAS analysis.

- [validation/](validation/) holds the standalone Table 2 subgroup audit and review-package checks.
- [report_alt_dx/](report_alt_dx/) holds the exploratory report-classification utilities. These require authorized private inputs and the recorded review steps. Opening the code for review does not require running classification.

## Local configuration

The report utilities use `RESEARCH_DATA_ROOT` for the external research-data folder and append `radiology/cta-aortic-dissection/derived/report_alt_dx` for report artifacts. Set `CODEX_EXECUTABLE` or make the classifier executable available on `PATH`. The older runner also accepts `--codex`, which takes precedence. The finalizer uses `RSCRIPT_EXECUTABLE` or `Rscript` on `PATH`. The two runners support `--help` without local data or executables. Review the finalizer settings before running it, because it writes outputs. No personal computer path is needed in the shared source.

The source checks still matter. Changes to a runner or verifier create a new source version. They do not authorize replacing the hashes in an old frozen record. Historical reproduction requires the original files and separately authorized private artifacts.
