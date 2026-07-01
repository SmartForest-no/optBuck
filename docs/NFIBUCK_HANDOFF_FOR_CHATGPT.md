# nfiBuck — Project Handoff Document

**Date:** 2026-07-01  
**For:** New AI assistant session / collaborator  
**Project:** SFI SmartForest — project 52106  
**Repository:** Private GitHub (not yet pushed; `git init` done locally)  
**Language/runtime:** R 4.4.3 on Windows

---

## 1. What the project is trying to do

`nfiBuck` simulates optimal timber bucking (log cross-cutting) on trees from the
Norwegian National Forest Inventory (NFI). Given a tree's taper profile and a set
of product and price definitions, it finds the cut pattern that **maximises total
timber value** using a backward-induction dynamic-programming (DP) optimizer.

The end-goal is a **local Shiny app** (v1) that lets a researcher:
1. Select a stratum (species × site class × maturity class, optionally filtered by
   county or elevation)
2. Upload or choose a price matrix
3. Sample a subset of complete NFI plots from that stratum
4. Run the full taper → candidate generation → DP optimizer pipeline on the sample
5. View approximate timber-value estimates, product breakdowns, and diagnostics

The v1 app is deliberately **sampled-plot only** (not full-stratum interactive) to
keep runtime under a browser-session limit. Full-stratum runs are offline.

---

## 2. Current folder structure

```
nfiBuck/
├── R/                                   ← ACTIVE helpers (7 files, commit these)
│   ├── helpers_paths.R
│   ├── helpers_products.R
│   ├── helpers_valuation.R
│   ├── helpers_audit.R
│   ├── helpers_taper.R
│   ├── helpers_candidate_generation.R
│   └── helpers_dp_optimizer.R
│
├── audits/                              ← ACTIVE audits (2 files, commit these)
│   ├── audit_14_validate_first_helpers.R
│   ├── audit_15_core_engine_helpers_regression.R
│   └── prototype_reference/             ← archived audits 05–12 (read-only)
│       ├── audit_05_candidate_log_valuation.R
│       ├── audit_06_one_tree_bucking_optimizer.R
│       ├── audit_07_batch_bucking_optimizer.R
│       ├── audit_08_example_sample_bucking.R
│       ├── audit_09_pine_bucking_smoke.R
│       ├── audit_10_mixed_species_example_bucking.R
│       ├── audit_11_selected_stratum_bucking.R
│       └── audit_12_selected_stratum_summary.R
│
├── scripts/
│   ├── 99_cleanup_project_structure.R   ← cleanup script (commit)
│   └── prototype_reference/             ← archived scripts 00–15 (read-only)
│       ├── 00_prepare_nfi_2024_data.R
│       ├── 01_test_stratum_selection.R
│       ├── ...
│       └── 15_core_engine_helpers_regression_test.R
│
├── inputs/                              ← product/price definitions (commit all)
│   ├── ProductData_spruce_pine_default.csv
│   ├── LengthClasses_spruce_pine_default.csv
│   ├── price_matrices/
│   │   ├── PriceMatrix_long_default_2021.csv   (75 KB, canonical long-format)
│   │   ├── PriceMatrix_long_default_2021.rds   (2 KB binary cache)
│   │   ├── PriceMatrix_pine_sawlog_default_2021.csv
│   │   ├── PriceMatrix_spruce_sawlog_default_2021.csv
│   │   └── FlatPrices_default_2021.csv
│   └── price_matrix_sources/            ← source XLSX files (commit)
│       ├── PriceMatrixFuru.xlsx
│       ├── PriceMatrixGran.xlsx
│       └── pristabell_gran_skur.xlsx
│
├── outputs/                             ← mixed: some commit, most excluded
│   ├── helper_validation/               ← audit 14 results (COMMIT)
│   ├── core_engine_helpers_regression/  ← audit 15 results + regression data (COMMIT)
│   ├── project_review/                  ← inventories, cleanup manifest (COMMIT)
│   ├── selected_stratum_summary/        ← final stratum outputs — LOCAL ONLY (large)
│   ├── stem_profiles/                   ← taper profiles from NFI data — LOCAL ONLY
│   ├── optimizer_tests/                 ← large candidate CSVs — LOCAL ONLY
│   ├── mixed_species_example_bucking/   ← prototype run — LOCAL ONLY
│   ├── stratum_tests/                   ← parquet of private NFI stratum — LOCAL ONLY
│   └── _archive/prototype_20260619/     ← archived prototype outputs — EXCLUDED
│
├── data_processed/                      ← PRIVATE NFI data, LOCAL ONLY, never commit
│   ├── nfi_plots_2024.parquet
│   ├── nfi_trees_2024_spruce_pine.parquet
│   ├── nfi_tree_plot_2024_spruce_pine.parquet
│   └── nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet  (+ .rds variants)
│
├── data_raw/                            ← empty folder; raw data is from Oracle only
│   └── README.md                        ← placeholder explaining absence of data
│
├── logs/                                ← LOCAL ONLY (excluded)
│
├── docs/
│   ├── PROJECT_STATUS.md
│   ├── CLEANUP_PLAN.md
│   ├── CLEANUP_MANIFEST.md
│   ├── APP_DESIGN_NOTES.md
│   ├── GITHUB_REPO_PREP.md
│   └── NFIBUCK_HANDOFF_FOR_CHATGPT.md   ← this file
│
├── README.md
└── .gitignore
```

---

## 3. Current and important files

### Active helpers — `R/`

These seven files are the **current production code**. Do not modify without
running audit 14 and audit 15 afterward.

| File | Key exported functions | Purpose |
|------|----------------------|---------|
| `helpers_paths.R` | `get_root_dir()`, `path_input()`, `path_output()`, `path_data()`, `ensure_dir()` | Resolve all project paths; used by every other helper |
| `helpers_products.R` | `load_products()`, `load_length_classes()`, `check_product_assumptions()` | Load ProductData and LengthClasses; verify 6-key active set and assumptions |
| `helpers_valuation.R` | `load_price_matrix_long()`, `cap_price_diameter_class()`, `prepare_price_join_keys()`, `validate_price_matrix_against_products()`, `get_supported_sawlog_lengths()` | Load and validate price matrices; apply 50 cm diameter cap for lookup |
| `helpers_audit.R` | `init_audit_log()`, `record_check()`, `alog()`, `diag_val()`, `audit_verdict()`, `write_audit_outputs()`, `` `%\|\|%` `` | Shared audit/logging framework used by all audits |
| `helpers_taper.R` | `generate_taper_profile_one_tree()`, `generate_taper_profiles_for_trees()`, `profiles_to_profile_vectors()`, `normalise_tree_inputs_for_taper()` | Wrap `taperNOR::taperNOR()` to produce per-tree taper profiles at 10 cm height steps |
| `helpers_candidate_generation.R` | `generate_candidate_logs_one_tree()`, `products_for_species()` | Generate all StartPos × StopPos × ProductKey candidate log combinations; compute Huber volume; assign PriceDiameterClass |
| `helpers_dp_optimizer.R` | `optimize_one_tree_dp()`, `check_selected_logs_no_overlap()`, `summarize_optimizer_result()` | Backward-induction DP over position grid; returns selected logs with zero overlap guarantee |

### Active audits — `audits/`

| File | Verdict | Checks | Notes |
|------|---------|--------|-------|
| `audit_14_validate_first_helpers.R` | **ACCEPTED WITH REVIEW** | 25 PASS, 3 REVIEW | 3 REVIEWs = expected 31 dm sawlog gap; not a defect |
| `audit_15_core_engine_helpers_regression.R` | **PASS** | 23/23 PASS | Full regression of all 7 helpers |

### Active input files — `inputs/`

| File | Contents |
|------|----------|
| `ProductData_spruce_pine_default.csv` | 6 rows defining spruce/pine sawlog, pulpwood, waste products; DiameterUnderBark=FALSE, DiameterSource=taperNOR_over_bark |
| `LengthClasses_spruce_pine_default.csv` | 190 rows; all eligible length × diameter combinations per product |
| `price_matrices/PriceMatrix_long_default_2021.csv` | 628 rows; canonical long-format price matrix (default 2021 scenario) |
| `price_matrices/PriceMatrix_long_default_2021.rds` | Binary cache of above |
| `price_matrices/PriceMatrix_pine_sawlog_default_2021.csv` | Wide-format pine sawlog prices |
| `price_matrices/PriceMatrix_spruce_sawlog_default_2021.csv` | Wide-format spruce sawlog prices |
| `price_matrices/FlatPrices_default_2021.csv` | Flat-rate prices for pulpwood and waste |

---

## 4. Archived / prototype-reference files

These files are **read-only reference**. Do not edit or re-run them without a
specific reason. They document the accepted prototype work.

| Location | Contents | Rule |
|----------|----------|------|
| `scripts/prototype_reference/00_*.R` – `15_*.R` | All 16 prototype scripts in original form | Read-only; source of truth for what was validated |
| `audits/prototype_reference/audit_05_*.R` – `audit_12_*.R` | 8 prototype audits | Read-only |
| `outputs/_archive/prototype_20260619/` | Prototype run outputs (CSVs, PNGs, audit TXTs) | Excluded from git; local archive only |
| `outputs/selected_stratum_summary/` | Final stratum summary (script 12 output) | Local only; too large and derived from private NFI data |
| `outputs/stem_profiles/` | Taper profiles for 500-tree NFI sample | Local only; required by audit 15 — regenerate with script 02 |

---

## 5. Accepted assumptions (hard-coded / verified by audits)

| Parameter | Value | Where enforced |
|-----------|-------|---------------|
| `STUMP_HEIGHT_CM` | **10 cm** | `helpers_candidate_generation.R` — first valid StartPos |
| `START_POS_STEP_CM` | **10 cm** | `helpers_candidate_generation.R` and `helpers_dp_optimizer.R` — DP grid resolution |
| Taper function | **`taperNOR::taperNOR()` with `with_bark = TRUE`** | `helpers_taper.R`; profile_step_m = 0.10 m |
| `DiameterUnderBark` | **`FALSE`** for all 6 active products | `helpers_products.R`; verified by audit 14 check 7 |
| `DiameterSource` | **`"taperNOR_over_bark"`** for all 6 active products | `helpers_products.R`; verified by audit 14 check 8 |
| Price diameter cap | **`MAX_PRICE_DIAMETER_CLASS_CM = 50L`** | `helpers_valuation.R`; `cap_price_diameter_class()`; physical diameter unaffected; original stored in `PriceDiameterClass_cm_uncapped`; capped value in `PriceDiameterClass_cm_used` |
| Sawlog volume formula | **Huber formula using top diameter floored to cm** | `helpers_candidate_generation.R`; `floor(TopDiameter/10)*10` mm before cylinder-volume; matches product column `"Measured diameter rounded downwards to cm"` |
| Active product keys | `spruce_sawlog`, `spruce_pulpwood`, `spruce_waste`, `pine_sawlog`, `pine_pulpwood`, `pine_waste` | `helpers_products.R` constant `ACTIVE_PRODUCT_KEYS` |
| Supported sawlog lengths | **{34, 37, 40, 43, 46, 49, 52, 55} dm** for both species | `helpers_valuation.R`; verified by audit 14 checks 9–10 |
| 31 dm sawlog | Present in `LengthClasses` but **unpriced** — no row in price matrix | Known gap; candidates generated but excluded from price join; reported as REVIEW |

---

## 6. Status of scripts 00–15 and audits

### Prototype scripts (now in `scripts/prototype_reference/`)

| Script | Purpose | Status |
|--------|---------|--------|
| 00 | Read NFI 2020–2024 from Oracle; export parquet/rds | Accepted — requires Oracle connection |
| 01 | Stratum filter tests (species, hogstkl, abonitet, fylnr, hoh) | Accepted |
| 02 | Generate taper profiles (taperNOR, 500-tree sample) | Accepted |
| 03 | Define ProductData + LengthClasses; Huber volume tests | Accepted |
| 04 | Import price matrices; produce `PriceMatrix_long_default_2021.*` | Accepted |
| 05 | One-tree candidate valuation test | Accepted — audit_05 PASS |
| 06 | One-tree DP optimizer test | Accepted — audit_06 PASS |
| 07 | Batch optimizer test (small sample) | Accepted — audit_07 PASS |
| 08 | Spruce example-sample run | Accepted — audit_08 ACCEPTED WITH REVIEW |
| 09 | Pine smoke test (25 trees) | Accepted — audit_09 ACCEPTED WITH REVIEW |
| 10 | Mixed spruce/pine 500-tree run; introduces diameter-cap columns | Accepted — audit_10 ACCEPTED WITH REVIEW |
| 11 | Full-stratum run: 21,238 spruce trees, 1,239 plots | Accepted — audit_11 ACCEPTED WITH REVIEW |
| 12 | Stratum summary: product/tree/plot tables, 7 diagnostic PNGs | Accepted — audit_12 ACCEPTED WITH REVIEW |
| 13 | Project review and file inventory | Accepted |
| 14 | First-helpers validation (prototype version, in prototype_reference) | Accepted |
| 15 | Core-engine helpers regression test (prototype version) | Accepted |

### Current audits (in `audits/`)

| Audit | What it tests | Verdict | Detail |
|-------|--------------|---------|--------|
| `audit_14_validate_first_helpers.R` | `helpers_paths`, `helpers_products`, `helpers_valuation`, `helpers_audit` — 20 checks | **ACCEPTED WITH REVIEW** | 25 PASS, 3 REVIEW (all 31 dm gap — expected) |
| `audit_15_core_engine_helpers_regression.R` | All 7 helpers; one-tree and mixed-species regression; overlap, diameter-cap, zero-price checks — 23 checks | **PASS** | 23/23 PASS |

---

## 7. What was proven by regression tests (audits 14 and 15)

### Audit 14 proves

- All 4 first helpers load without error from their canonical paths
- All 6 `ACTIVE_PRODUCT_KEYS` are present in `ProductData`
- `DiameterUnderBark = FALSE` and `DiameterSource = "taperNOR_over_bark"` for every active product
- Supported sawlog lengths = {34, 37, 40, 43, 46, 49, 52, 55} dm for both species
- 31 dm gap is present and flagged correctly as REVIEW (not FAIL)
- `cap_price_diameter_class()` maps values > 50 to 50 and preserves values ≤ 50
- `prepare_price_join_keys()` produces `PriceDiameterClass_cm_uncapped` (original) and `PriceDiameterClass_cm_used` (= min(original, 50))
- `validate_price_matrix_against_products()` returns 0 FAIL items and only the expected 2 REVIEWs (31 dm + zero-price waste)
- All 13 prototype scripts 00–12 exist in `scripts/prototype_reference/` and were not modified after the helpers were frozen

### Audit 15 proves

- All 7 helper files exist and source without error
- `generate_taper_profile_one_tree()` produces a correctly-structured profile (253 rows for test tree; columns `treid`, `z_m`, `diameter_cm`, `diameterPosition`, `DiameterValue`, `DBH`)
- `profiles_to_profile_vectors()` produces named list with all 7 fields including correct `DBH = 566.0 mm` for test tree (height 25.30 m)
- `products_for_species()` returns exactly `{spruce_sawlog, spruce_pulpwood, spruce_waste}` and `{pine_sawlog, pine_pulpwood, pine_waste}`
- `generate_candidate_logs_one_tree()` produces 20,713 candidates for a test spruce tree
- **One-tree regression (deterministic):**
  - Selected logs: exactly 5 (matches reference)
  - StartPos/StopPos set: exact match to reference
  - Total value: 946.2783 NOK (diff from reference = 0.000000)
  - Total volume: 1.786110 m³ (diff from reference = 0.000000)
- **Mixed-species regression (500 trees):**
  - All 458 reference trees with logs appear in output (0 missing)
  - Per-tree StartPos/StopPos: exact match for all 458 trees
  - Total value: 42,210.6668 NOK (diff from reference = 0.000000)
- No overlapping logs in either test (zero overlap guarantee)
- `PriceDiameterClass_cm_uncapped` column present; `PriceDiameterClass_cm_used ≤ 50` for all selected logs
- 31 dm sawlog unpriced entries confirmed (2 entries in mixed-species run)
- Waste zero prices treated as REVIEW (not FAIL) by validation

---

## 8. Known limitations

| Limitation | Detail |
|-----------|--------|
| **No bark correction** | All volume and diameter computations use over-bark values from `taperNOR`. Under-bark volume or net timber volume is not computed. |
| **No HPR / StanForD import** | All trees come from NFI data. No CTL/harvester head production records. |
| **No NFI expansion factors** | Plot-level per-ha values use simple `value / plot_area_m2 × 10000`. No plot weights, expansion factors, or design-based estimators are applied. All estimates from the sampled-plot app must be labelled "sampled estimate — not ha-expanded — no NFI factors applied." |
| **Sampled-plot estimates only (app v1)** | Full-stratum interactive runs (21,000+ trees) are too slow for a web session. The Shiny app v1 samples n_plots (10/25/50) complete plots and runs the optimizer only on those trees. |
| **User-uploaded price matrices need validation** | External matrices must have columns `ProductKey`, `LengthClass_dm`, `PriceDiameterClass_cm`, `PriceNOKperM3`, `scenario`. The `validate_price_matrix_against_products()` function in `helpers_valuation.R` performs the checks. No automatic format conversion. Negative prices are rejected; zero prices require explicit user confirmation. |
| **31 dm sawlog gap** | Sawlog price matrices start at 34 dm. Candidates at 31 dm are generated (they are technically eligible) but remain unpriced and therefore cannot be selected by the optimizer. This is a data gap in the 2021 price table, not a code defect. |
| **Single-stem model** | Optimization is per-stem. No stand-level volume routing or market allocation. |
| **No exact timber measurement conventions** | Production-harvesting measurement rules (mid-point measurement, exact bark reduction factor) are not modelled. |
| **10 cm DP grid** | `START_POS_STEP_CM = 10` means log start positions are on a 10 cm grid. Optimal cut positions that fall between grid points are not explored. This is a deliberate performance/accuracy trade-off. |

---

## 9. Current GitHub status

### What has been done
- `git init` was run locally (2026-06-19).
- `.gitignore` was created.
- No `git add` or `git commit` has been run yet.
- No remote has been configured.

### `git status --short` output (as of 2026-06-19)
```
?? .gitignore
?? R/
?? README.md
?? audits/
?? data_raw/
?? docs/
?? inputs/
?? outputs/
?? scripts/
```
All files are untracked. Nothing has been staged.

### Files intended for commit

```
.gitignore
README.md
R/helpers_paths.R
R/helpers_products.R
R/helpers_valuation.R
R/helpers_audit.R
R/helpers_taper.R
R/helpers_candidate_generation.R
R/helpers_dp_optimizer.R
audits/audit_14_validate_first_helpers.R
audits/audit_15_core_engine_helpers_regression.R
audits/prototype_reference/audit_05_*.R  (×8 files)
scripts/99_cleanup_project_structure.R
scripts/prototype_reference/00_*.R – 15_*.R  (×16 files)
inputs/ProductData_spruce_pine_default.csv
inputs/LengthClasses_spruce_pine_default.csv
inputs/price_matrices/FlatPrices_default_2021.csv
inputs/price_matrices/PriceMatrix_long_default_2021.csv
inputs/price_matrices/PriceMatrix_long_default_2021.rds   (2.1 KB, tiny binary)
inputs/price_matrices/PriceMatrix_pine_sawlog_default_2021.csv
inputs/price_matrices/PriceMatrix_spruce_sawlog_default_2021.csv
inputs/price_matrix_sources/PriceMatrixFuru.xlsx
inputs/price_matrix_sources/PriceMatrixGran.xlsx
inputs/price_matrix_sources/pristabell_gran_skur.xlsx
outputs/helper_validation/          (6 files — audit 14 results)
outputs/core_engine_helpers_regression/  (6 files — audit 15 results + regression CSVs)
outputs/project_review/             (7 files — inventories, manifest)
data_raw/README.md
docs/PROJECT_STATUS.md
docs/CLEANUP_PLAN.md
docs/CLEANUP_MANIFEST.md
docs/APP_DESIGN_NOTES.md
docs/GITHUB_REPO_PREP.md
docs/NFIBUCK_HANDOFF_FOR_CHATGPT.md
```

### Files excluded by `.gitignore`

| Pattern | What is excluded | Reason |
|---------|-----------------|--------|
| `data_raw/*` (except README.md) | Raw NFI data (accessed from Oracle) | Private |
| `data_processed/` | 8 NFI parquet/rds files, ~15 MB total | Private NFI data |
| `logs/` | Log archives | Local only |
| `outputs/_archive/` | Archived prototype outputs | Redundant; large |
| `outputs/mixed_species_example_bucking/` | Prototype run artefacts | Large; regeneratable |
| `outputs/optimizer_tests/` | Large candidate CSVs (~5.6 MB) | Large; regeneratable |
| `outputs/selected_stratum_summary/` | Tree-level summary (~4.3 MB) | Large; private NFI data derived |
| `outputs/stratum_tests/` | NFI stratum parquet | Private NFI data derived |
| `outputs/stem_profiles/` | Taper profiles (~1.3 MB binary) | Derived from private NFI data |
| `inputs/*.rds` | `ProductData_*.rds`, `LengthClasses_*.rds` (root-level only) | Regeneratable binary caches |
| `cleanup_run_out.txt`, `cleanup_run_err.txt` | Cleanup log artefacts | Temporary |
| `.Rhistory`, `.RData`, `.Rproj.user/` | R session artefacts | Local only |
| `.Renviron`, `.Rprofile` | Oracle credentials / env vars | Sensitive |
| `*.tmp`, `*.bak`, `*.log` | Temp files | Local only |

### Sensitive / large files that must NOT be committed

| File / folder | Size | Risk |
|---------------|------|------|
| `data_processed/nfi_trees_2024_spruce_pine.parquet` | 2.85 MB | **Private NFI data** |
| `data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet` | 2.40 MB | **Private NFI data** |
| `data_processed/nfi_tree_plot_2024_spruce_pine.parquet` | 2.70 MB | **Private NFI data** |
| `data_processed/nfi_plots_2024.parquet` | 0.21 MB | **Private NFI data** |
| `outputs/selected_stratum_summary/selected_stratum_summary_by_tree.csv` | 4.25 MB | Contains tree-level NFI-derived data |
| `outputs/optimizer_tests/one_tree_optimizer_*_candidates.csv` | 2.83 MB × 2 | Large; regeneratable |
| `outputs/stem_profiles/stem_profiles_example_500trees.parquet` | 0.72 MB | Derived from private NFI trees |
| Oracle credentials / `.Renviron` | — | **No credential files were found** in the project folder ✓ |

---

## 10. Recommended next steps

### Immediate (before any new code)

1. **Verify `.gitignore` is complete**
   Run `git ls-files --others --exclude-standard` and confirm `data_processed/`
   and `outputs/selected_stratum_summary/` do not appear. See section 9 for the
   expected output.

2. **Create initial commit and push to private GitHub**
   ```bash
   git add .
   git commit -m "Initial commit: 7 core engine helpers, audits 14/15, prototype reference"
   git remote add origin https://github.com/NIBIO/<repo-name>.git
   git push -u origin main
   ```

3. **Confirm audit 15 still passes on a fresh clone**
   After cloning, run audit 15 after generating `outputs/stem_profiles/`:
   ```r
   # Step 1 — requires Oracle access and data_processed/
   source("scripts/prototype_reference/02_generate_stem_profiles.R")
   # Step 2 — runs without Oracle
   source("audits/audit_15_core_engine_helpers_regression.R")
   ```

### Next development task

4. **Create the sampled-plot backend script**
   File: `scripts/app_backend/01_run_sampled_plot_bucking_backend.R`
   
   This script is the backend for the Shiny app v1. It should:
   - Accept parameters: `species_group`, `hogstkl`, `abonitet`, `n_plots`,
     `price_matrix_path`, `stump_height_cm = 10`, `start_pos_step_cm = 10`
   - Load `data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet`
   - Filter to the requested stratum
   - Sample `n_plots` complete plots (all trees per plot)
   - Source all 7 helpers in `R/`
   - Run the full pipeline: taper → candidates → DP optimizer
   - Return a structured list: `selected_logs`, `tree_summary`, `plot_summary`,
     `product_summary`, `unpriced_summary`, `diagnostics`
   - All outputs labelled as sampled estimates; no NFI expansion factors

5. **Create the Shiny app prototype**
   Files: `app/app.R`, `app/ui.R`, `app/server.R`
   
   Minimum viable v1 UI:
   - Stratum selector (species, hogstkl, abonitet)
   - n_plots selector (10 / 25 / 50)
   - Price matrix: use default or upload CSV/XLSX
   - Run button → calls backend script
   - Results tabs: product summary, plot-level histogram, price-gap report,
     zero-log tree table, diameter-cap report
   - All estimate labels: "Sampled-plot estimate — not ha-expanded — no NFI factors applied"

6. **Stratum availability table**
   Query `data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet`
   for `n_plots` per `(species_group, hogstkl, abonitet)` combination.
   Use `outputs/project_review/stratum_availability_by_species_hogstkl_abonitet.csv`
   as the starting point — this file already exists and can be committed.
   The app should only offer strata with ≥ 10 complete plots.

7. **Price matrix validation module**
   Formalise `validate_price_matrix_against_products()` for interactive use:
   return user-readable messages for the app UI, not just the audit framework.
   This is already in `helpers_valuation.R` but the output format targets the
   audit log, not a Shiny notification panel.

### Longer term (not v1)

- Bark correction module
- NFI expansion factor integration
- Full-stratum offline batch runner with progress reporting
- HPR / StanForD file import
- Sensitivity analysis (`STUMP_HEIGHT_CM`, `START_POS_STEP_CM`)

---

## Appendix A — How to run the two current audits

```r
# From the project root (S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck)

# Audit 14 — no data dependency, runs standalone
source("audits/audit_14_validate_first_helpers.R")
# Output: outputs/helper_validation/audit_14_validate_first_helpers.{csv,txt}
# Expected verdict: ACCEPTED WITH REVIEW (3 REVIEW items = 31 dm gap)

# Audit 15 — requires outputs/stem_profiles/stem_profiles_example_500trees.parquet
source("audits/audit_15_core_engine_helpers_regression.R")
# Output: outputs/core_engine_helpers_regression/audit_15_core_engine_helpers.{csv,txt}
# Expected verdict: PASS (23/23)
```

## Appendix B — R packages required

```r
install.packages(c("arrow", "readr", "dplyr"))
# taperNOR — internal package; install from source or NIBIO registry
```

## Appendix C — Key data pipeline for context

```
Oracle DB
  └─ 00_prepare_nfi_2024_data.R
       └─ data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet
            └─ [stratum filter + plot sample]
                 └─ helpers_taper.R  →  taper profiles (10 cm steps)
                      └─ helpers_candidate_generation.R  →  candidate logs
                           └─ helpers_valuation.R  →  priced candidates
                                └─ helpers_dp_optimizer.R  →  selected logs
                                     └─ [summary / Shiny output]
```
