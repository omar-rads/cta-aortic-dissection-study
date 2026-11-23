# CT angiography for suspected acute aortic syndrome

Analysis code for a retrospective emergency department CTA study. The repository includes the current R analysis, published D-dimer evidence, the exploratory report-classification method, and selected aggregate outputs from earlier analyses.

| Analysis | Where to start |
| --- | --- |
| Clinical cohort, outcomes, and regression | [R script guide](R/README.md) |
| ADD-RS and D-dimer subgroup yields | [Table 2 reproduction notes](docs/TABLE2_SUBGROUP_REVIEW.md) |
| Cost-consequence model | [Economic analysis engine](R/economic_cost_consequence_engine.R) and [CMS cost sources](docs/cost_sources/SOURCE_VERIFICATION_RECORD.md) |
| Exploratory analysis of non-AAS findings | [Frozen classifier specification](docs/report_alt_dx_v2_specification.md) |
| Earlier implementations | [SAS scripts](scripts/README.md) and [earlier R checkpoint](legacy/r-pre-rebuild/) |

## Reproduction and access

See the [software release notes](docs/SOFTWARE_RELEASE.md) for source versions, public checks, and the limits of reproducing retained study outputs. Patient-level data, radiology reports, and private classifier artifacts are held separately. Access requests go through the study team and applicable institutional review.

## License and citation

Original repository software and associated documentation are available under the [MIT License](LICENSE). Third-party software and source materials retain any terms stated by their rights holders. The license does not grant access to clinical records.

See [CITATION.cff](CITATION.cff) for software citation information. The earlier [SAS archive DOI](https://doi.org/10.5281/zenodo.17228961) identifies the historical SAS release.
