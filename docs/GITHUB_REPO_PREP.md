# GITHUB_REPO_PREP.md — nfiBuck
**Prepared:** 2026-06-19  
**Purpose:** Record what is and is not committed to the private GitHub repository.

---

## Files / folders intended for commit

### Core code
| Path | Size | Notes |
|------|------|-------|
| `R/helpers_paths.R` | 2.7 KB | |
| `R/helpers_products.R` | 6.5 KB | |
| `R/helpers_valuation.R` | 10.5 KB | |
| `R/helpers_audit.R` | 6.8 KB | |
| `R/helpers_taper.R` | 10.5 KB | |
| `R/helpers_candidate_generation.R` | 14.0 KB | |
| `R/helpers_dp_optimizer.R` | 8.4 KB | |

### Audits
| Path | Size | Notes |
|------|------|-------|
| `audits/audit_14_validate_first_helpers.R` | 16 KB | Verdict: ACCEPTED WITH REVIEW |
| `audits/audit_15_core_engine_helpers_regression.R` | 21 KB | Verdict: PASS |
| `audits/prototype_reference/audit_05_*.R` | 18 KB | Reference only |
| `audits/prototype_reference/audit_06_*.R` | 20 KB | Reference only |
| `audits/prototype_reference/audit_07_*.R` | 18 KB | Reference only |
| `audits/prototype_reference/audit_08_*.R` | 19 KB | Reference only |
| `audits/prototype_reference/audit_09_*.R` | 17 KB | Reference only |
| `audits/prototype_reference/audit_10_*.R` | 24 KB | Reference only |
| `audits/prototype_reference/audit_11_*.R` | 28 KB | Reference only |
| `audits/prototype_reference/audit_12_*.R` | 23 KB | Reference only |

### Scripts
| Path | Size | Notes |
|------|------|-------|
| `scripts/99_cleanup_project_structure.R` | 19 KB | |
| `scripts/prototype_reference/00_prepare_nfi_2024_data.R` | 20 KB | Requires Oracle |
| `scripts/prototype_reference/01_test_stratum_selection.R` | 17 KB | |
| `scripts/prototype_reference/02_generate_stem_profiles.R` | 18 KB | |
| `scripts/prototype_reference/03_define_products_and_volume.R` | 36 KB | |
| `scripts/prototype_reference/04_import_or_define_price_matrices.R` | 29 KB | |
| `scripts/prototype_reference/05_test_candidate_log_valuation.R` | 20 KB | |
| `scripts/prototype_reference/06_test_one_tree_bucking_optimizer.R` | 32 KB | |
| `scripts/prototype_reference/07_test_batch_bucking_optimizer.R` | 29 KB | |
| `scripts/prototype_reference/08_run_bucking_on_example_sample.R` | 27 KB | |
| `scripts/prototype_reference/09_test_pine_bucking_smoke.R` | 39 KB | |
| `scripts/prototype_reference/10_run_mixed_species_example_bucking.R` | 47 KB | |
| `scripts/prototype_reference/11_run_bucking_on_selected_stratum.R` | 51 KB | |
| `scripts/prototype_reference/12_summarise_selected_stratum_bucking.R` | 20 KB | |
| `scripts/prototype_reference/13_project_cleanup_and_app_readiness_review.R` | 17 KB | |
| `scripts/prototype_reference/14_validate_first_helpers.R` | 6 KB | |
| `scripts/prototype_reference/15_core_engine_helpers_regression_test.R` | 15 KB | |

### Inputs
| Path | Size | Notes |
|------|------|-------|
| `inputs/ProductData_spruce_pine_default.csv` | 1.3 KB | |
| `inputs/LengthClasses_spruce_pine_default.csv` | 3.2 KB | |
| `inputs/price_matrices/FlatPrices_default_2021.csv` | 1.0 KB | |
| `inputs/price_matrices/PriceMatrix_long_default_2021.csv` | 75 KB | |
| `inputs/price_matrices/PriceMatrix_long_default_2021.rds` | 2.1 KB | |
| `inputs/price_matrices/PriceMatrix_pine_sawlog_default_2021.csv` | 36 KB | |
| `inputs/price_matrices/PriceMatrix_spruce_sawlog_default_2021.csv` | 38 KB | |
| `inputs/price_matrix_sources/PriceMatrixFuru.xlsx` | 10.7 KB | Source XLSX |
| `inputs/price_matrix_sources/PriceMatrixGran.xlsx` | 12.8 KB | Source XLSX |
| `inputs/price_matrix_sources/pristabell_gran_skur.xlsx` | 12.0 KB | Source XLSX |

### Outputs (audit results and project review only)
| Path | Size | Notes |
|------|------|-------|
| `outputs/helper_validation/` | ~8 KB total | Audit 14 results |
| `outputs/core_engine_helpers_regression/` | ~153 KB total | Audit 15 results + regression reference CSVs |
| `outputs/project_review/` | ~99 KB total | File inventories and cleanup manifest |

### Docs and placeholders
| Path | Notes |
|------|-------|
| `README.md` | Top-level README |
| `docs/PROJECT_STATUS.md` | |
| `docs/CLEANUP_MANIFEST.md` | |
| `docs/CLEANUP_PLAN.md` | |
| `docs/APP_DESIGN_NOTES.md` | |
| `docs/GITHUB_REPO_PREP.md` | This file |
| `data_raw/README.md` | Placeholder explaining that raw data is not stored |

---

## Files / folders excluded by `.gitignore`

| Path | Reason |
|------|--------|
| `data_raw/*` (except README.md) | Raw NFI data from Oracle — private, not distributed |
| `data_processed/` | Processed NFI parquet/rds files; ~15 MB; private data |
| `logs/` | Local log archives |
| `outputs/_archive/` | Large archived prototype outputs; redundant after cleanup |
| `outputs/mixed_species_example_bucking/` | Prototype run artefacts from script 10 |
| `outputs/optimizer_tests/` | Large candidate log CSVs (~5.6 MB) from script 06 |
| `outputs/selected_stratum_summary/` | Large tree-level summary (~4.3 MB) from script 12 |
| `outputs/stratum_tests/` | Binary parquet of private NFI stratum sample |
| `outputs/stem_profiles/` | Derived taper profiles from private NFI data; ~1.3 MB binary |
| `inputs/*.rds` | Binary caches regeneratable from CSV sources |
| `cleanup_run_out.txt` | Temporary cleanup run log |
| `cleanup_run_err.txt` | Temporary cleanup run log |
| `.Rhistory`, `.RData`, `.Rproj.user/` | R session artefacts |
| `.Renviron`, `.Rprofile` | Local credentials / environment (if present) |
| `*.log`, `*.tmp`, `*.bak` | Temporary files |

---

## Sensitive or large files detected

| File | Size | Risk | Decision |
|------|------|------|----------|
| `data_processed/nfi_trees_2024_spruce_pine.parquet` | 2.85 MB | **Private NFI data** | Excluded — `data_processed/` in .gitignore |
| `data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet` | 2.40 MB | **Private NFI data** | Excluded |
| `data_processed/nfi_tree_plot_2024_spruce_pine.parquet` | 2.70 MB | **Private NFI data** | Excluded |
| `data_processed/nfi_plots_2024.parquet` | 0.21 MB | **Private NFI data** | Excluded |
| `outputs/selected_stratum_summary/selected_stratum_summary_by_tree.csv` | 4.25 MB | Large; derived from private NFI data | Excluded — folder in .gitignore |
| `outputs/_archive/…/selected_stratum_selected_logs.csv` | 4.08 MB | Large archive artefact | Excluded — `_archive/` in .gitignore |
| `outputs/optimizer_tests/one_tree_optimizer_priced_candidates.csv` | 2.83 MB | Large prototype output | Excluded — folder in .gitignore |
| `outputs/optimizer_tests/one_tree_optimizer_generated_candidates.csv` | 2.83 MB | Large prototype output | Excluded |
| `outputs/stem_profiles/stem_profiles_example_500trees.parquet` | 0.72 MB | Derived from private NFI data | Excluded — folder in .gitignore |
| No Oracle wallet, `.Renviron`, or credential files found in project folder | — | ✓ Clean | — |

---

## Uncertainty — `data_processed/`

The `data_processed/` directory contains the cleaned NFI tree and plot data
(8 files, ~15 MB total) derived from the Oracle database via
`scripts/prototype_reference/00_prepare_nfi_2024_data.R`.

**Decision: exclude.** These are private NFI data. The repository README notes
that these files must be generated locally from the Oracle database before
running any prototype scripts.

## Uncertainty — `outputs/stem_profiles/`

`outputs/stem_profiles/stem_profiles_example_500trees.parquet` (0.72 MB) is
required as input data by audit 15. However:
- It is derived from private NFI tree data
- It is a binary file (parquet)
- It can be regenerated by running `scripts/prototype_reference/02_generate_stem_profiles.R`

**Decision: exclude.** Audit 15 will not run out-of-the-box from a fresh clone
without this file. The README documents the prerequisite.

---

## Notes on `inputs/price_matrices/PriceMatrix_long_default_2021.rds`

This 2.1 KB binary file was committed (not excluded) because it is tiny and
is the direct serialised form of the canonical long-format price matrix used
by multiple helpers. Re-generation requires running
`scripts/prototype_reference/04_import_or_define_price_matrices.R`.

If preferred, it can be added to `inputs/*.rds` exclusion — but at 2.1 KB it
poses no size concern.
