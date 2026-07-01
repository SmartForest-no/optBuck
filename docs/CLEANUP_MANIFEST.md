# nfiBuck — Cleanup Manifest

**Date:** 2026-06-19  
**Phase:** Post-prototype cleanup — prototype reference archived, helpers visible  
**Script:** `scripts/99_cleanup_project_structure.R` (run with `DRY_RUN = TRUE` first)

---

## What was kept visible

### `R/` — helpers only

All numbered prototype scripts were moved to `scripts/prototype_reference/`.
Only these files remain in `R/`:

| File | Purpose |
|------|---------|
| `helpers_paths.R` | Project root, path construction, `ensure_dir()` |
| `helpers_products.R` | Load ProductData and LengthClasses, validate assumptions |
| `helpers_valuation.R` | Load price matrix, diameter capping, price-join key preparation |
| `helpers_audit.R` | Shared audit utilities: `record_check()`, `alog()`, `diag_val()`, `%||%`, `audit_verdict()` |
| `helpers_taper.R` | Taper profile generation via taperNOR, profile vector conversion |
| `helpers_candidate_generation.R` | Vectorised candidate log generation (expand.grid + cumulative volume) |
| `helpers_dp_optimizer.R` | Backward-induction DP optimiser, overlap check, result summary |

### `audits/` — current audits only (at top level)

| File | Purpose |
|------|---------|
| `audit_14_validate_first_helpers.R` | 20-check audit of helpers_paths/products/valuation/audit |
| `audit_15_core_engine_helpers_regression.R` | 23-check audit of helpers_taper/candidate_generation/dp_optimizer |

### `outputs/` — current/final outputs kept at top level

| Folder | Contents | Why kept |
|--------|----------|----------|
| `outputs/project_review/` | File inventories, script dependency overview, cleanup manifest CSV | Planning and provenance |
| `outputs/helper_validation/` | Script 14 helper check outputs | Current audit 14 outputs |
| `outputs/core_engine_helpers_regression/` | Script 15 regression outputs | Current audit 15 outputs |
| `outputs/selected_stratum_summary/` | Final accepted stratum summary (product/tree/plot, 7 diagnostic PNGs, price-gap/cap CSVs) | Final accepted output from script 12 |

### `docs/` — all documents kept

| File | Purpose |
|------|---------|
| `PROJECT_STATUS.md` | Accepted scripts, outputs, assumptions, known limitations |
| `CLEANUP_PLAN.md` | Proposed helper structure and output classification |
| `APP_DESIGN_NOTES.md` | Shiny app v1 design notes |
| `CLEANUP_MANIFEST.md` | This file |

### `inputs/` — unchanged (all files kept)

ProductData, LengthClasses, price matrices, price matrix sources.

### `data_processed/` — unchanged

NFI 2024 parquet and rds files.

### `data_raw/` — empty folder, README added

The folder is empty. Raw data are read from Oracle by script 00.
A `README.md` was added explaining this.

---

## What was moved to archive

### Prototype R scripts → `scripts/prototype_reference/`

All files previously in `R/` matching `[0-9]+_*.R` pattern:

| Script | Purpose |
|--------|---------|
| `00_prepare_nfi_2024_data.R` | Oracle data extract |
| `01_test_stratum_selection.R` | Stratum filter tests |
| `02_generate_stem_profiles.R` | Taper profile generation |
| `03_define_products_and_volume.R` | ProductData and volume |
| `04_import_or_define_price_matrices.R` | Price matrix import |
| `05_test_candidate_log_valuation.R` | Candidate valuation test |
| `06_test_one_tree_bucking_optimizer.R` | One-tree optimizer test |
| `07_test_batch_bucking_optimizer.R` | Batch optimizer test |
| `08_run_bucking_on_example_sample.R` | Example sample run |
| `09_test_pine_bucking_smoke.R` | Pine smoke test |
| `10_run_mixed_species_example_bucking.R` | Mixed-species run |
| `11_run_bucking_on_selected_stratum.R` | Full-stratum run |
| `12_summarise_selected_stratum_bucking.R` | Stratum summary |
| `13_project_cleanup_and_app_readiness_review.R` | Project review |
| `14_validate_first_helpers.R` | First helper regression |
| `15_core_engine_helpers_regression_test.R` | Core engine regression |

### Prototype audits → `audits/prototype_reference/`

| Audit | Corresponds to |
|-------|---------------|
| `audit_05_candidate_log_valuation.R` | Script 05 |
| `audit_06_one_tree_bucking_optimizer.R` | Script 06 |
| `audit_07_batch_bucking_optimizer.R` | Script 07 |
| `audit_08_example_sample_bucking.R` | Script 08 |
| `audit_09_pine_bucking_smoke.R` | Script 09 |
| `audit_10_mixed_species_example_bucking.R` | Script 10 |
| `audit_11_selected_stratum_bucking.R` | Script 11 |
| `audit_12_selected_stratum_summary.R` | Script 12 |

### Old prototype outputs → `outputs/_archive/prototype_YYYYMMDD/`

| Folder | Archived because |
|--------|-----------------|
| `batch_optimizer_tests/` | Script 07 test outputs; no current script references these |
| `example_sample_bucking/` | Script 08 outputs; no current script references these |
| `pine_smoke_bucking/` | Script 09 outputs; no current script references these |
| `price_matrix_tests/` | Script 04 outputs; no current script references these |
| `product_volume_tests/` | Script 03 outputs; no current script references these |
| `valuation_tests/` | Script 05 outputs; no current script references these |
| `selected_stratum_bucking/` | Script 11 raw outputs; summarised version kept in `selected_stratum_summary/` |

### Old log → `logs/_archive/`

| File | Reason |
|------|--------|
| `nfi_2024_data_summary.txt` | Generated by script 00; retained but not actively used |

---

## What was intentionally NOT moved (uncertain)

The following output folders are **kept in place** at `outputs/` because they are
actively referenced by the current regression test (`scripts/prototype_reference/15_core_engine_helpers_regression_test.R`
reads them as reference inputs) or by the archived prototype scripts.
Moving them would silently break reproducibility checks.

| Folder | Used by | Reason to keep |
|--------|---------|---------------|
| `outputs/optimizer_tests/` | Script 15 / audit 15 | Contains `one_tree_optimizer_selected_logs.csv` — accepted reference for regression test A |
| `outputs/mixed_species_example_bucking/` | Script 15 / audit 15 | Contains `mixed_species_selected_trees.csv`, `mixed_species_selected_logs.csv`, `mixed_species_tree_summary.csv` — reference inputs for regression test B |
| `outputs/stem_profiles/` | Script 15 / audit 15 | Contains `stem_profiles_example_500trees.parquet` — source of tree 242251 DBH/height for regression test A |
| `outputs/stratum_tests/` | Scripts 10/11 (archived) | Contains example stratum parquet; needed to re-run scripts 10/11 from prototype_reference |

To move these, update the path references in `scripts/prototype_reference/15_core_engine_helpers_regression_test.R`
and `audits/audit_15_core_engine_helpers_regression.R` first, then archive.

---

## How to restore archived files

All archived files were moved (not deleted). To restore any file:

1. Locate it under `outputs/_archive/prototype_YYYYMMDD/` or `scripts/prototype_reference/` or `audits/prototype_reference/` or `logs/_archive/`.
2. Copy or move the file/folder back to its original location.

The `outputs/project_review/cleanup_manifest.csv` records the exact `from_rel → to_rel` mapping for every planned/executed move.

---

## Which scripts/audits are current

| File | Location | Status |
|------|----------|--------|
| `helpers_paths.R` | `R/` | Current — required by all |
| `helpers_products.R` | `R/` | Current — required by all |
| `helpers_valuation.R` | `R/` | Current — required by all |
| `helpers_audit.R` | `R/` | Current — required by all |
| `helpers_taper.R` | `R/` | Current — required by taper/candidate/optimizer |
| `helpers_candidate_generation.R` | `R/` | Current — core candidate engine |
| `helpers_dp_optimizer.R` | `R/` | Current — core DP engine |
| `audit_14_validate_first_helpers.R` | `audits/` | Current — validates helpers 1-4 |
| `audit_15_core_engine_helpers_regression.R` | `audits/` | Current — validates core engine (23 PASS) |

**Coming next:** `scripts/app_backend/01_run_sampled_plot_bucking_backend.R`
(not yet created — see `docs/APP_DESIGN_NOTES.md`)

---

## Notes on accepted prototype scripts

Scripts 00–12 are **accepted** (contents must not be modified).
They are in `scripts/prototype_reference/` for reference and reproducibility.
They are **not** active in the cleaned-up structure.
To re-run a prototype script, call it from its `prototype_reference/` location with
all required input files present (paths are hardcoded in the scripts).

Scripts 13, 14, 15 are moved to `scripts/prototype_reference/` as well.
The active regression logic from scripts 14/15 is represented by:
- `R/helpers_*.R` (the extracted helpers)
- `audits/audit_14_*.R` and `audits/audit_15_*.R` (the current audits)
