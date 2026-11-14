# Table 2 subgroup reproduction

This standalone audit reproduces the descriptive ADD-RS and D-dimer breakdown added for the November 14 manuscript revision. It reads the retained encounter-level analytic object without sourcing or rerunning the clinical pipeline. Cohort, risk-score, lab, outcome and regression definitions are unchanged.

## Run locally

Authorized access to `aas_analytic_encounters.rds` is required. Run from the repository root, using a new output directory:

```sh
Rscript --vanilla scripts/validation/reproduce_table2_subgroups.R /path/to/aas_analytic_encounters.rds work/table2-review
```

The script refuses a nonempty output directory. All checks must pass before it writes any output. The `work/` directory is ignored by Git. Do not force-add these outputs. Review small cells and combinations before sharing any new result file. Git ignore rules prevent ordinary staging but are not access controls or permission to publish data.

## What it checks

- Five mutually exclusive groups: ADD-RS 2 or 3 with any D-dimer status, ADD-RS 0 or 1 with positive D-dimer, low score with no identified D-dimer, low score with negative D-dimer, and missing ADD-RS.
- Yield denominators include known positive and negative outcomes and exclude unknown outcomes.
- Numeric outcome indicators agree with the outcome labels, including missing values.
- Algorithm classifications agree with the existing score and D-dimer rule.
- Group totals reproduce the approved retained-cohort audit.

The exported tables are `subgroup_counts.csv`, `algorithm_counts.csv`, `score_counts.csv` and `assertions.csv`, with `sessionInfo.txt`. Confidence intervals are two-sided exact binomial 95% intervals. The script intentionally omits the more granular score-by-lab-by-outcome cross-tab from the original local audit. It exports no encounter-level rows, identifiers, dates or report text.

## Verification completed November 14

The four aggregate CSVs are byte-identical to the accepted November 11 audit, and all 11 assertions pass. The input SHA-256 before and after execution was `4e602b39fea3adfc5eb87546b310b88d635b8a4f07877f0aa2a760d09303cf2d`. No source data were changed.

Run the synthetic checks without clinical data:

```sh
Rscript --vanilla tests/test_table2_subgroups.R
```

These check the expected margins, row-order and applicable lab-status invariance, inconsistent classifications and outcomes, missing fields, invalid labels, changed cohort size, and overwrite refusal. Fixture records are artificial records constructed from aggregate margins, not patient records.

These checks establish reproduction of retained calculations. They do not reabstract diagnoses, validate the risk-score reconstruction, establish when a D-dimer result became available, or measure whether testing changed the imaging decision. The script deliberately stops when the cohort differs from the retained manuscript cohort. A changed cohort requires a separate approved analysis and updated expectations.

## Interpretation

The observed yield contrast is concentrated in high-score encounters. This is a descriptive finding among encounters that received CTA. It does not establish the incremental clinical value of D-dimer, prove that a result prompted a scan, or establish safe deferral of imaging.
