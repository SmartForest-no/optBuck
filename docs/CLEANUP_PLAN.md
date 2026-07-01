# nfiBuck — Cleanup Plan

**Date:** 2026-06-19  
**Phase:** Planning only — no files deleted or refactored yet

---

## Proposed helper file structure

The prototype scripts each contain the full pipeline inline. Before building a Shiny app, repeated logic should be extracted into helper files under `R/`. Scripts become thin wrappers that source helpers and set parameters.

### Proposed helpers

| Helper file | Content | Currently duplicated in |
|-------------|---------|------------------------|
| `R/helpers_paths.R` | Project root, `in_dir`, `out_dir` construction; `ensure_dir()` wrapper | All scripts (hardcoded paths) |
| `R/helpers_taper.R` | `generate_profile()` wrapper around `taperNOR::taperNOR()`; species-string lookup; profile validation | 02, 07, 08, 09, 10, 11 |
| `R/helpers_products.R` | `load_products()`, `load_length_classes()`; validate product/species combinations | 03, 05, 06, 07, 08, 09, 10, 11 |
| `R/helpers_candidate_generation.R` | `generate_candidates()` — positions, log lengths, top diameters, volume (Huber); species filtering | 05, 06, 07, 08, 09, 10, 11 |
| `R/helpers_valuation.R` | `load_price_matrix()`, `value_candidates()` — price join with diameter capping; `MAX_PRICE_DIAMETER_CLASS_CM`; unpriced tracking | 05, 06, 07, 08, 09, 10, 11, 12 |
| `R/helpers_dp_optimizer.R` | `optimize_one_tree()` — backward-induction DP over StartPos states; overlap check; returns selected logs | 06, 07, 08, 09, 10, 11 |
| `R/helpers_summary.R` | `summarise_tree_results()`, `summarise_plot_results()`, `summarise_products()` | 08, 09, 10, 11, 12 |
| `R/helpers_audit.R` | `record()`, `alog()`, `diag_val()`, `%||%` operator; verdict logic | All audit scripts (fully duplicated) |

---

## Scripts: proposed role after refactor

| Script | Proposed role | Action |
|--------|--------------|--------|
| 00 | Keep as-is (data preparation, DB connection specific) | No change |
| 01 | Thin wrapper: source helpers_paths; run stratum selection | Minor cleanup |
| 02 | Thin wrapper: source helpers_taper; run profile generation | Minor cleanup |
| 03 | Thin wrapper: source helpers_products; define and save products | Minor cleanup |
| 04 | Thin wrapper: source helpers_valuation; import and validate price matrices | Minor cleanup |
| 05 | Keep as test/reference script | No change |
| 06 | Keep as test/reference script | No change |
| 07 | Keep as test/reference script | No change |
| 08 | Keep as accepted reference run | No change |
| 09 | Keep as accepted reference run | No change |
| 10 | Keep as accepted reference run | No change |
| 11 | Thin wrapper: source all helpers; set stratum path and parameters | After helpers extracted |
| 12 | Thin wrapper: source helpers_summary; set input/output dirs | After helpers extracted |
| 13 | Keep as project review and inventory script | No change |

Audits (05–12): keep as-is. After `helpers_audit.R` is created, extract `record()`, `alog()`, `diag_val()`, `%||%` and source from there.

---

## Output folder inventory (proposed classification)

| Folder | Contents | Proposed status |
|--------|----------|----------------|
| `outputs/selected_stratum_bucking/` | Final accepted stratum run (script 11) | **Keep — final output** |
| `outputs/selected_stratum_summary/` | Final accepted summary (script 12) | **Keep — final output** |
| `outputs/stratum_tests/` | Example stratum files used in scripts 10/11 | **Keep — required inputs for scripts 10/11** |
| `outputs/stem_profiles/` | Intermediate profiles from script 02 | Keep as reference; could be regenerated |
| `outputs/example_sample_bucking/` | Accepted script 08 outputs | Keep as reference run |
| `outputs/pine_smoke_bucking/` | Accepted script 09 outputs | Keep as reference run |
| `outputs/mixed_species_example_bucking/` | Accepted script 10 outputs | Keep as reference run |
| `outputs/batch_optimizer_tests/` | Script 07 test outputs | Low priority; could archive |
| `outputs/optimizer_tests/` | Script 06 single-tree test | Low priority; could archive |
| `outputs/valuation_tests/` | Script 05 valuation test | Low priority; could archive |
| `outputs/price_matrix_tests/` | Script 04 price matrix outputs | Low priority; could archive |
| `outputs/product_volume_tests/` | Script 03 product/volume tests | Low priority; could archive |
| `outputs/project_review/` | Script 13 inventories and review | Keep — planning artifacts |

### Proposed archive list (do not delete yet)

These can be moved to `outputs/_archive/` when confirmed no longer needed:

- `outputs/batch_optimizer_tests/`
- `outputs/optimizer_tests/`
- `outputs/valuation_tests/`
- `outputs/price_matrix_tests/`
- `outputs/product_volume_tests/`

---

## Input files (keep all)

| File | Status |
|------|--------|
| `inputs/ProductData_spruce_pine_default.csv/.rds` | Required by all bucking scripts |
| `inputs/LengthClasses_spruce_pine_default.csv/.rds` | Required by all bucking scripts |
| `inputs/price_matrices/PriceMatrix_long_default_2021.csv/.rds` | Required by all bucking scripts |
| `inputs/price_matrices/FlatPrices_default_2021.csv` | Required |
| `inputs/price_matrices/PriceMatrix_spruce_sawlog_default_2021.csv` | Source/reference |
| `inputs/price_matrices/PriceMatrix_pine_sawlog_default_2021.csv` | Source/reference |
| `inputs/price_matrix_sources/` | Source files for price matrices; keep for provenance |

---

## Data files (keep all)

| File | Status |
|------|--------|
| `data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet/.rds` | Primary analysis dataset; keep |
| `data_processed/nfi_tree_plot_2024_spruce_pine.parquet/.rds` | Intermediate join; keep |
| `data_processed/nfi_trees_2024_spruce_pine.parquet/.rds` | Raw tree data; keep |
| `data_processed/nfi_plots_2024.parquet/.rds` | Plot data; keep |

---

## Notes

- No files are deleted in this plan. All proposed actions are advisory.
- Refactoring (extracting helpers) should happen before the Shiny app is built.
- Audits should be run again after any refactoring to confirm no regressions.
- The `data_raw/` directory is currently empty; raw data was loaded directly from the Oracle DB in script 00.
