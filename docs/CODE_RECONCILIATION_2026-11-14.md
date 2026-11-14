# November 14 code reconciliation at commit 1a54826

This record describes the completed subgroup-audit update at commit `1a54826`, before the later repository cleanup. Its comparisons and original filenames refer to that snapshot. See the [current R script guide](../R/README.md) and [later cleanup record](TEAM_CODE_REVIEW.md#continuous-numbering-and-section-notes) for the current names and checks.

## Review branch at that snapshot

`review/naeem-table2-2026-11-14` starts from `main` at `97c750c6d35b11a2bdf44a2170a9a12f0ca9b848`. It adds the standalone Table 2 reproduction, synthetic tests and reviewer instructions. Existing clinical scripts R00 through R09, economic modules, frozen classifier source, source configurations, tests and saved results remain unchanged. The existing citation metadata still describes the October 30 snapshot. This is a review update, not a new software release or DOI version.

The added audit reproduces the four accepted November 11 aggregate tables byte for byte from the retained analytic object. All 11 aggregate assertions pass. New audit outputs remain local. The synthetic tests cover valid margins, invariance, invalid inputs and refusal to overwrite outputs.

## Older local work

The old development branch has 12 commits beyond its own remote branch. That does not mean 12 analyses are missing from current main. The current review package already incorporated the adopted analysis source. All 44 compared shared nonclinical source and published-evidence artifacts match exactly. This includes current R10 onward, the economic engine, classifier scripts and configuration, published-evidence artifacts and existing tests.

| Material | Disposition |
| --- | --- |
| November 11 five-script cleanup | Superseded by the adopted November 12 cleanup. No demonstrated missing feature requires carrying it forward. Original candidate preserved. |
| Old development R00 through R09 | Older than current main. Not copied. |
| Historical audit notes and unpublished result files | Preserved in the original checkout. Not bulk imported or silently promoted to current results. |
| Optional R15 MPPR audit | Preserved locally. It writes encounter-level audit output and does not determine billing eligibility or alter the adopted model. Not added to this review branch. |
| Earlier CMS locality-payment audit | Historical source work with unresolved national rates at that date. Current verified cost documentation under `docs/cost_sources/` remains controlling. Not imported as an unresolved current costing issue. |
| Calibration workbook builder and modified calibration guide | Historical development tooling, separate from the frozen v2 analysis. Preserved locally. No new classifier run or calibration cycle authorized by this reconciliation. |
| Old Python environment listing | Previously removed as unused. Not restored. |
| Additional historical SAS file | Preserved in the older checkout. Existing repository legacy scripts remain unchanged. |

No files or branches were deleted. There was no blanket merge, history rewrite, force push, new clinical analysis, or change to manuscript conclusions.

## Remaining reproduction and release limits

The original economic entrypoints R14, R16 and R17 retain branch-name guards. They do not run directly on main or this review branch. Their shared aggregate economic engine can be used as documented in the team guide. The frozen classifier retains exact-hash checks and private-input requirements. This update does not bypass those controls or claim a complete raw-to-manuscript reproduction.

The repository remains private. Coauthor access, publication of a new software version, the license decision and any Zenodo update require separate action. New result files require review of small cells and combinations before sharing. Existing aggregate files were preserved without alteration.
