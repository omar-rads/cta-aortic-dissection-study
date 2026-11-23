# Software release for the acute aortic syndrome CTA study

This release records the software reviewed at Git commit `af51a873798c90bce3f36645442f26293e7b4012`. The release commit adds documentation and license metadata. Executable source and retained GitHub results are carried forward byte for byte. The accompanying software archive includes the software, validation fixtures, and evidence from published sources. The archive omits the patient-derived `results/` directory and filters the file-provenance index accordingly.

## Source of the economic base case

The base case in `R/14_economic_option2_published_meta.R` uses sensitivity 96.5% (95% credible interval, 94.8%–98.0%) and specificity 56.2% (95% credible interval, 48.3%–63.9%) from the primary 500-ng/mL analysis of 18 cohort studies reported by Essat et al. The four interval bounds match the one-way scenario inputs. The Ren reconstruction is the separate conditional ADD-RS 0–1 analysis. [Essat et al., Annals of Emergency Medicine, 2024](https://doi.org/10.1016/j.annemergmed.2024.05.001)

## What can be checked from this archive

Run the public checks from the extracted software directory. The R lockfile specifies R 4.5.1. For locked package versions, run `renv::restore()` in a disposable checkout. The Bayesian model used CmdStan 2.40.0. `Rscript --vanilla` uses the calling environment’s library paths. Install `digest` for the frozen-source test or make the locked package available to that R session.

```sh
python3 -m unittest discover -s tests -p 'test_*.py'
python3 scripts/validation/check_review_package.py
Rscript --vanilla tests/test_economic_cost_consequence_engine.R
Rscript --vanilla tests/test_report_alt_dx_v2.R
Rscript --vanilla tests/test_report_frozen_sources.R
Rscript --vanilla tests/test_table2_subgroups.R
```

These checks validate the package and exercise synthetic examples. The [team review guide](TEAM_CODE_REVIEW.md) records prior comparisons and runtime dependencies. Existing clinical and exploratory output files were generated under earlier source versions. A raw-to-manuscript rerun from this commit has not been independently verified. Historical branch checks and source digests identify specific original inputs and source bytes. The historical SAS code is included for provenance.

The report-classification model name, settings, and prompts document the historical research method. Its repeat-pass agreement measures reproducibility among classifier outputs. Independent clinical accuracy requires a separate reference standard.
