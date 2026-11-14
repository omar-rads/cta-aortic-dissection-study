# Option 1 conditional D-dimer results

Run identifier: `option1_2026-09-19_v1`.

## Reconciliation

All six study tables passed the source-total, nonnegative-cell, and conditional-identity checks. The derived ADD-RS 0–1 tables contain 407 true positives, 11 false negatives, 1615 false positives, and 2062 true negatives. Crude pooling gives sensitivity 97.37% and specificity 56.08% (descriptive only).

## Ren reproduction gate

The reconstructed paired-threshold model estimated 98.27% sensitivity and 51.33% specificity for ADD-RS >1 OR D-dimer >500, compared with the published 98.3% and 51.4%. Both differences were below 1 percentage point, both targets lay inside the reconstructed 95% intervals, and all MCMC gates passed.

## Conditional Bayesian estimates

| Prior strategy | Pooled sensitivity (95% CrI) | Pooled specificity (95% CrI) |
|---|---:|---:|
| Weakly informative | 96.7% (89.1%–98.7%) | 54.7% (41.1%–68.2%) |
| Ren-style heterogeneity | 97.2% (95.1%–98.6%) | 54.4% (46.1%–62.3%) |

Pooled-accuracy prior-sensitivity classification: **essentially unchanged** (absolute median differences: sensitivity 0.47 percentage points; specificity 0.22 percentage points).

That label applies only to the pooled medians. Heterogeneity and predictive uncertainty are substantially prior-sensitive: the sensitivity prediction interval is 65.6%–99.7% under weak priors versus 94.3%–98.7% under Ren-style priors; the corresponding specificity intervals are 21.8%–83.7% and 33.9%–73.4%.

Posterior predictive checks produced 0 extreme tail-area flag(s) under weak priors and 2 under Ren-style priors. The Ren-style flags are the McLatchie 2023 true-positive count (posterior predictive p=0.9994) and the overall sensitivity Pearson discrepancy (p=0.0118). These flags indicate that the informative heterogeneity prior does not fully reproduce the observed sensitivity dispersion, despite satisfactory MCMC convergence.

Full heterogeneity estimates, correlations, prediction intervals, posterior predictive checks, and diagnostics are in the machine-readable output tables. These Option 1 estimates are candidate external parameters only; they are not designated as final or base-case economic inputs.

## Limitations

Only six studies contribute. Some diseased cells are sparse, between-study heterogeneity and the sensitivity-specificity correlation are uncertain, and transportability to the local CTA-selected cohort is not established. This is a secondary reanalysis of published study-level data rather than a new systematic review. The Ren-style priors are a documented single-threshold reduction of a four-parameter paired-threshold model. Option 1 remains separate from Option 2 pending later methodological comparison.
