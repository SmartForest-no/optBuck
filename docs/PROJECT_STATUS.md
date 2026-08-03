# nfiBuck — Project Status

**Date:** 2026-06-19  
**Phase:** Prototype complete → cleanup and app planning

---

## Accepted scripts and audits

| Script | Purpose | Audit | Status |
|--------|---------|-------|--------|
| 00_prepare_nfi_2024_data.R | Read and clean NFI 2020–2024 tree/plot data from Oracle; export to parquet/rds. | — | Accepted |
| 01_test_stratum_selection.R | Test stratum filters (species, hogstkl, abonitet, fylnr, hoh) on the prepared dataset; export example stratum files. | — | Accepted |
| 02_generate_stem_profiles.R | Generate taper profiles for a sample of trees using taperNOR (over-bark); produce stem_profiles output. | — | Accepted |
| 03_define_products_and_volume.R | Define ProductData and LengthClasses for spruce/pine sawlog, pulpwood, waste; compute candidate log volumes using Huber formula. | — | Accepted |
| 04_import_or_define_price_matrices.R | Import and validate price matrices (PriceMatrix_long, FlatPrices); produce canonical price input files. | — | Accepted |
| 05_test_candidate_log_valuation.R | Test candidate log generation and price join for one tree; verify PriceDiameterClass assignment and capping logic. | audit_05 | Accepted |
| 06_test_one_tree_bucking_optimizer.R | Generate all candidate logs for one tree, value them, and run the DP bucking optimizer; verify selected-log sequence. | audit_06 | Accepted |
| 07_test_batch_bucking_optimizer.R | Batch optimizer test on a small sample of trees; verify batch results, timing, and overlap checks. | audit_07 | Accepted |
| 08_run_bucking_on_example_sample.R | Run the optimizer on the spruce example stratum sample; produce tree_summary, selected_logs, candidate diagnostics. | audit_08 | Accepted with REVIEW |
| 09_test_pine_bucking_smoke.R | Pine-only smoke test (25 trees by DBH quantile); verify optimizer produces results for pine price matrices. | audit_09 | Accepted with REVIEW |
| 10_run_mixed_species_example_bucking.R | Mixed spruce/pine 500-tree integration test; introduces MAX_PRICE_DIAMETER_CLASS_CM parameter and PriceDiameterClass_cm_uncapped/_used columns. | audit_10 | Accepted with REVIEW |
| 11_run_bucking_on_selected_stratum.R | Full-stratum run on 21,238 spruce trees (1,239 plots); produces all bucking outputs including checkpoint, unpriced summary, and plot summary. | audit_11 | Accepted with REVIEW |
| 12_summarise_selected_stratum_bucking.R | Summarise script 11 outputs at product, tree, plot, and stratum levels; produce diagnostic plots and price-gap/cap summaries. | audit_12 | Accepted with REVIEW |

---

## Main outputs (scripts 04–12)

| Script | Key outputs |
|--------|-------------|
| 04 | `inputs/price_matrices/PriceMatrix_long_default_2021.csv/.rds`, `FlatPrices_default_2021.csv` |
| 05 | `outputs/valuation_tests/` — candidate logs with prices for one test tree |
| 06 | `outputs/optimizer_tests/` — one-tree optimizer results |
| 07 | `outputs/batch_optimizer_tests/` — batch optimizer results |
| 08 | `outputs/example_sample_bucking/` — spruce example outputs |
| 09 | `outputs/pine_smoke_bucking/` — pine smoke test outputs |
| 10 | `outputs/mixed_species_example_bucking/` — mixed-species outputs |
| 11 | `outputs/selected_stratum_bucking/` — selected_logs, tree_summary, plot_summary, diagnostics, checkpoint |
| 12 | `outputs/selected_stratum_summary/` — product/tree/plot summaries, zero-log list, price-gap/cap CSVs, 7 diagnostic PNGs |

---

## Accepted assumptions

| Parameter | Value | Note |
|-----------|-------|-------|
| `STUMP_HEIGHT_CM` | 10 cm | First cut position above terrain |
| `START_POS_STEP_CM` | 10 cm | DP grid step; controls position resolution |
| Taper function | `taperNOR::taperNOR()`, over-bark (`with_bark = TRUE`) | Kozak-type Norwegian taper equation |
| `DiameterUnderBark` | `FALSE` | All diameter-based pricing uses over-bark diameter |
| `MAX_PRICE_DIAMETER_CLASS_CM` | 50 cm | Logs with PriceDiameterClass > 50 are capped to 50 for price lookup only; physical diameter unaffected; original stored in `PriceDiameterClass_cm_uncapped` |

---

## Known limitations

| Item | Status |
|------|--------|
| No bark correction | Not implemented. Pricing uses over-bark volume and diameter. |
| No HPR / StanForD import | Not implemented. All trees come from NFI data. |
| No NFI expansion factors | Not applied. Plot-level per-ha values are simple unexpanded plot-scaling only. |
| 31 dm sawlog price | Resolved 2026-08-03 by interpolation (`scripts/97_fill_31dm_sawlog_price_gap.R`, Price = 2×34dm − 37dm). Rows flagged `PriceSource = "Interpolated"`, not from the original source spreadsheet. |
| No exact timber-measurement rules | Precise production harvesting measurement conventions (e.g., measurement at log mid-point with exact bark reduction) are not yet modelled. |
| User-defined price matrices | External matrices must conform to the `PriceMatrix_long` format validated in script 04. No automatic format conversion is provided. |
| Single-stem model | Optimization is per-stem. No stand-level volume-to-market routing. |

---

## Recommended next steps

1. **Refactor shared logic into helper files** — see `docs/CLEANUP_PLAN.md`.
2. **Build a simple Shiny app (v1)** — see `docs/APP_DESIGN_NOTES.md`. First version: sampled-plot mode only, spruce/pine, uploaded price matrices.
3. **Stratum availability review** — use script 13 output to identify which species/hogstkl/abonitet combinations have enough plots for sampled-plot estimates.
4. **Price matrix validation module** — formalize the validation logic from script 04 into a reusable function that the Shiny app can call.
5. **Sensitivity analysis** — test `STUMP_HEIGHT_CM` = 5, 20, 30 cm; `START_POS_STEP_CM` = 5, 20 cm.
