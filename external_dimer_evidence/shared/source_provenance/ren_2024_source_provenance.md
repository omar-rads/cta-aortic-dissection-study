# Ren et al. 2024 source provenance

This directory records the published aggregate evidence used for the Option 1 conditional D-dimer synthesis. It contains no patient-level data.

- Article: Ren S, et al. *Diagnostic accuracy of the aortic dissection detection risk score alone or with D-dimer for acute aortic syndromes: Systematic review and meta-analysis.* PLOS One. 2024;19:e0304401. PMID 38905181. DOI 10.1371/journal.pone.0304401.
- Article URL: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0304401
- S4 statistical appendix URL: https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0304401.s004
- S4 downloaded 2026-09-19; SHA-256 `6bae6558d8c857f107a645cca75a275b2e1a5653f1602d53fe9b768af8587e52`.
- S7 forest-plot appendix URL: https://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0304401.s007
- S7 downloaded 2026-09-19; SHA-256 `ed2b05f917c2fd1442605cc03905517d35b23e1521f1a497b9e889ae7a75f8c2`.

The counts in `ren_2024_joint_strategy_counts.csv` were transcribed from S7 Figure S7-2(a), S7-2(b), and S7-4(c), then independently checked against the embedded figure images. The embedded-image paths and SHA-256 hashes are recorded in that table. S7-2(a) is retained only for the Ren-model compatibility check; the requested conditional tables use S7-2(b) and S7-4(c).

S4 defines the paired-threshold random effects in the order FPR location, TPR location, log FPR threshold scale, and log TPR threshold scale. The four gamma distributions are listed in the same positional order but are not individually labelled, and the preferred informative fit's correlation structure and numeric threshold coding are not supplied. The reproducibility documentation therefore distinguishes direct statements from necessary model-reconstruction assumptions.
