# Option 1 conditional D-dimer synthesis: methods and provenance

## Status and scope

This is a self-contained secondary reanalysis of published study-level data. It estimates D-dimer accuracy conditional on ADD-RS 0–1 for later comparison with Option 2. It is not designated as a final or base-case economic input, and the script does not modify the manuscript, cohort analyses, economic model, or Option 2 files.

## 1. Why the conditional tables can be reconstructed

Within each study, the positive rule `ADD-RS >1 OR D-dimer >500` contains everyone positive by `ADD-RS >1` plus the additional D-dimer-positive participants among those with ADD-RS 0–1. The published strategies use the same diseased and nondiseased totals. Therefore subtraction isolates D-dimer performance in the ADD-RS 0–1 stratum.

For every study:

- `TP_low = TP_combined - TP_ADD-RS>1`
- `FN_low = FN_combined`
- `TN_low = TN_combined`
- `FP_low = FP_combined - FP_ADD-RS>1`

The script stops unless `TP_low + FN_low = FN_ADD-RS>1`, `TN_low + FP_low = TN_ADD-RS>1`, both source denominators agree, every derived cell is nonnegative, and every conditional denominator is positive.

## 2. Published counts and derived counts

| Study | ADD-RS >1 TP/FP/FN/TN | ADD-RS >1 OR D-dimer >500 TP/FP/FN/TN | Derived ADD-RS 0–1 TP/FP/FN/TN |
|---|---:|---:|---:|
| Kotani 2017 | 24/47/92/382 | 115/275/1/154 | 91/228/1/154 |
| McLatchie 2023 | 2/38/5/579 | 5/262/2/355 | 3/224/2/355 |
| Nazerian 2014b | 81/124/152/678 | 231/560/2/242 | 150/436/2/242 |
| Morello 2021 | 19/43/30/351 | 48/186/1/208 | 29/143/1/208 |
| Nazerian 2018 | 133/208/108/1399 | 238/686/3/921 | 105/478/3/921 |
| Gorla 2017a | 54/3/31/288 | 83/109/2/182 | 29/106/2/182 |
| Total | 313/463/418/3677 | 720/2078/11/2062 | 407/1615/11/2062 |

The authoritative machine-readable source is `external_dimer_evidence/shared/source_data/ren_2024_joint_strategy_counts.csv`. It records figure panels, embedded-image hashes, URLs, and unmodified published cells. The generated audit table adds the derived cells and study-specific conditional accuracy.

## 3. Source provenance

Counts come from Ren et al. 2024, PLOS One, PMID 38905181, Supplement Figure S7-2(b) and Supplement Figure S7-4(c). Figure S7-2(a) is also retained solely for the paired-threshold reproduction check. Exact source URLs and SHA-256 hashes are in `external_dimer_evidence/shared/source_provenance/ren_2024_source_provenance.md`.

## 4. Bayesian bivariate likelihood

For study *i*:

- `TP_i ~ Binomial(TP_i + FN_i, sensitivity_i)`
- `TN_i ~ Binomial(TN_i + FP_i, specificity_i)`
- `[logit(sensitivity_i), logit(specificity_i)]' ~ BVN(mu, Sigma)`

The Stan implementation is non-centred: `eta_i = mu + diag(tau) L_Omega z_i`, with `z_i` standard bivariate normal and `L_Omega` the Cholesky factor of the correlation matrix. The summary-study accuracy is `inv_logit(mu)`. It is not the crude arithmetic proportion or the marginal mean across a mixture of studies.

The two conditional fits use the same Stan program and likelihood. Only the heterogeneity-SD prior changes.

## 5. Weakly informative priors

- `mu_sensitivity ~ Normal(0, 2.5)`
- `mu_specificity ~ Normal(0, 2.5)`
- `tau_sensitivity ~ half-Student-t(df=3, location=0, scale=1)`
- `tau_specificity ~ half-Student-t(df=3, location=0, scale=1)`
- correlation matrix `Omega ~ LKJ(2)`

The lower bound on each Stan `tau` parameter supplies the half distribution.

## 6. Ren-style prior mapping and reproduction validation

Ren S4 orders its paired-threshold random effects as FPR location, TPR location, log FPR threshold scale, and log TPR threshold scale. The four listed gamma priors are mapped positionally:

1. FPR-location SD: `Gamma(shape=13.2, rate=44.7)`
2. TPR-location SD: `Gamma(shape=7.4, rate=38.3)`
3. FPR threshold-scale SD: `Gamma(shape=9.3, rate=38.1)`
4. TPR threshold-scale SD: `Gamma(shape=4.4, rate=15.2)`

Shape/rate follows JAGS convention and is the only plausible interpretation: shape/scale would give prior means far beyond the 0–5 range used for the reference SD priors. In the single-threshold conditional model, the first two location-heterogeneity priors supply the closest documented reduction:

- `tau_sensitivity ~ Gamma(shape=7.4, rate=38.3)`
- `tau_specificity ~ Gamma(shape=13.2, rate=44.7)`

Because `logit(specificity) = -logit(FPR)`, the FPR-to-specificity conversion preserves the SD and reverses the sign of a TPR–FPR correlation. The LKJ(2) prior is symmetric, so the sign transformation does not change that prior. The two threshold-scale priors have no direct single-threshold analogue and are not assigned to the conditional model.

Before the conditional Ren-prior fit is allowed to run, an auxiliary paired-threshold model uses both S7-2 panels and all four gamma priors. S4 does not state the numeric threshold coding or the preferred informative fit's correlation structure. The reconstruction uses the natural ordinal coding `C=(1,2)`, `g(C)=log(C)`, and the independence structure explicitly preferred in the main model. These reconstruction choices are accepted only if the higher-threshold pooled sensitivity and specificity are each within 1 percentage point of Ren's published 98.3% and 51.4%, each published target lies inside the reconstructed 95% interval, and all MCMC gates pass. Failure stops the pipeline before either conditional fit is accepted.

## 7. Sampling and convergence diagnostics

Every fit uses four chains, 2,000 warmup iterations, 4,000 retained iterations per chain, `adapt_delta=0.99`, `max_treedepth=15`, and a fixed seed. The non-centred parameterization is used throughout. Acceptance gates are:

- maximum split R-hat below 1.01;
- minimum bulk ESS at least 400;
- minimum tail ESS at least 400;
- zero divergent transitions;
- zero maximum-treedepth hits;
- E-BFMI above 0.3 in every chain.

Trace plots are saved for the population logits, heterogeneity SDs, and correlation. Diagnostics are exported at parameter, fit, and chain levels.

## 8. Pooled estimates, heterogeneity, and prediction intervals

For each conditional prior specification, the pipeline reports the posterior median and equal-tail 95% credible interval for pooled sensitivity, pooled specificity, both heterogeneity SDs, and the sensitivity-specificity correlation. A latent new study is drawn from the fitted bivariate random-effects distribution in every posterior draw; its 2.5th and 97.5th percentiles form the 95% prediction interval. Binomial sampling variation is excluded from that prediction interval and handled separately in posterior predictive checks.

Crude sensitivity `sum(TP)/sum(TP+FN)` and specificity `sum(TN)/sum(TN+FP)` are retained only as descriptive verification checks.

## 9. Prior comparison

The side-by-side output includes explicit estimates and intervals under both priors plus absolute differences. For pooled sensitivity and specificity, differences are expressed in percentage points. A descriptive label is applied to the pooled sensitivity and specificity medians using predeclared thresholds: at most 1 percentage point is `essentially unchanged`, more than 1 but less than 5 is `modestly prior-sensitive`, and at least 5 is `materially prior-sensitive`. The label does not extend to heterogeneity estimates, interval widths, or model fit, which are compared separately. It does not choose between Option 1 and Option 2.

## 10. Posterior predictive checks and limitations

Posterior predictive checks compare each study's observed TP and TN with replicated count distributions, compare total false negatives and false positives, and use Pearson-type discrepancy statistics for the sensitivity and specificity components. Posterior predictive tail areas below 0.05 or above 0.95 are flagged for review. The generated results document reports every such flag; a flag is treated as evidence of model-data tension, not as an MCMC convergence failure.

Limitations include only six studies, sparse diseased cells in some studies, uncertain between-study heterogeneity and correlation, and uncertain transportability to the local CTA-selected cohort. This is a secondary analysis of published aggregate data rather than a new systematic review. The paired-threshold reproduction necessarily reconstructs two details not reported in S4, although the published pooled result provides a stringent empirical check. Option 1 remains a candidate parameterization that must be compared separately with Option 2.
