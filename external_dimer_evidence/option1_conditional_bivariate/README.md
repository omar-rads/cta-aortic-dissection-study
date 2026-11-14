# Option 1 conditional D-dimer synthesis

This module reconstructs D-dimer 2×2 tables among participants with ADD-RS 0–1 from Ren et al. 2024 and fits Bayesian bivariate random-effects models under two prior specifications. It is isolated from Option 2 and from the current economic model.

Run from the repository root after restoring the R environment and installing CmdStan. The committed lockfile records the R-package dependencies; CmdStan itself is intentionally installed under ignored working storage:

```sh
Rscript -e 'renv::restore()'
Rscript -e 'cmdstanr::install_cmdstan(dir = "work/cmdstan", version = "2.40.0")'
Rscript external_dimer_evidence/option1_conditional_bivariate/R/run_option1_conditional_bivariate.R
```

The script expects CmdStan under `work/cmdstan/cmdstan-*` or at the path supplied through `CMDSTAN`. It uses four chains, 2,000 warmup iterations and 4,000 retained iterations per chain. Raw CmdStan files and fitted R objects remain under ignored `work/`; curated aggregate outputs and compressed posterior draws are written under `output/`.

The module does not read local patient-level study data. Its only analytic inputs are published aggregate counts in `../shared/source_data/ren_2024_joint_strategy_counts.csv`.
