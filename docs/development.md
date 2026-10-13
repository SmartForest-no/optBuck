# Apter development notes

These notes are for developers working on Apter. The partner-facing overview
is in [README.md](../README.md). AI-assistant context (domain terms,
load-bearing assumptions, data-privacy rules) is in [CLAUDE.md](../CLAUDE.md).
The development roadmap is in [docs/plans/PLAN.md](plans/PLAN.md).

Apter was called **nfiBuck** until the 2026-10 rename. The GitHub repository
was renamed to `lennartnoordermeer/Apter`; old URLs redirect.

---

## Status

| Component | Status |
|-----------|--------|
| Prototype scripts 00–15 | Accepted and frozen in `scripts/prototype_reference/` |
| Core engine helpers (`R/`) | Regression-tested: audit 15 PASS (23/23) |
| First-helper validation | Audit 14 PASS (28/28); zero-priced waste accepted 2026-10-12 |
| App backend | Audit 16 PASS (15/15), including fast vs. reference optimizer equivalence |
| Bark model and NOK/m³ denominator | Audit 18 PASS (13/13): barkNOR wrapper, B ≥ 0 floor, swappable model, synthetic stems, price independence |
| Tie-breaking and price scenarios | Audit 19 PASS (26/26): synthetic ties, order independence, matrix helpers, long and wide uploads (class gaps rejected), pattern comparison |
| Custom assortments and volume methods (Phase 4b) | Audit 20 PASS (40/40): volume methods by hand, custom assortments winning and losing, default results identical to the pre-Phase-4b baseline (with the pre-Phase-4c limits), validation, upload round trips, per-product price cap, prices kept when an assortment is edited |
| Tree lists and height imputation (Phase 5) | Audit 21 PASS (28/28): FinnHoyde curves and calibration by hand, fieldPlot wrapper (single species, time limit), validation of broken files, NFI trees as a tree list give identical results, end-to-end runs |
| Shiny app (Phase 4c layout) | Audit 17 PASS (28/28), headless shinytest2 smoke test with screenshots: two tabs, A editable, B states, rows and columns, automatic comparison when B differs, no "default"/"scenario" text, a custom assortment through the form, an uploaded tree list, Run disabled with progress while a run task runs |
| Assortment table and price sets A/B (Phase 4c) | Audit 23 PASS (10/10): base products through the table and back, default limits, derived Priority, lengths from the matrix columns, row/column helpers, class gaps, B states, xlsx round trip, B with its own lengths in the backend |
| Performance (vectorised core, comparisons, workers, ExtendedTask) | Audit 22 PASS (8/8): 1 vs 15 workers and chunk sizes identical, a comparison equals two separate runs, all cases equal main before the phase (with the pre-Phase-4c limits), the app's background job equals the backend |

The 31 dm sawlog price gap was filled by interpolation on 2026-08-03
(`scripts/97_fill_31dm_sawlog_price_gap.R`). Rows are flagged
`PriceSource = "Interpolated"`.

---

## Repository layout

```
app/                        Shiny app, files sourced explicitly by app.R
  app.R                     Entry point: resolves the root, sources the backend
                            and the files below, calls shinyApp(ui, server)
  helpers_root.R            Project root resolution (APTER_ROOT / parent of app/)
  setup.R                   Data, default assortments and prices (APP_DEFS, APP_PM_DEFAULT),
                            HKL/bonitet/county choices, fixed plots/seed, APP_VERSION
  theme.R                   Design tokens, chart colours, bslib (Bootstrap 5) theme
  helpers_ui.R              Stripe, cards, read-only fields, reactable tables,
                            base-R plot styling, grid renderers
  helpers_validation.R      Inputs -> backend params; stratum plot counts
  helpers_upload.R          Price files for the runs; uploads merged into price set A or B
  helpers_jobs.R            Runs as background jobs: promise chain over the workers
                            (shiny::ExtendedTask), progress per chunk
  app_ui.R                  `ui`: page_navbar with Assortments and prices, Simulator
                            (not ui.R: Shiny treats ui.R + app.R as two app formats)
  app_server.R              `server`
  www/
    apter.css               Component styles (ForestSens / Redwood look)
    apter-icon.svg          App icon and favicon (original artwork)
    fonts/                  Inter and Source Serif 4 (.woff2, SIL OFL 1.1; licences alongside)

R/                          Core engine helpers
  helpers_paths.R           Project root and path utilities (get_root_dir())
  helpers_products.R        Product definitions, assumption checks, the assortment table
                            (defs_from_products(), products_from_defs()), validate_products()
  helpers_volume_methods.R  Price-volume methods (Top cylinder, Top frustum, solid volume, Norwegian mid)
  helpers_valuation.R       Price matrix loading, validation, price lookup with per-product class cap
  helpers_audit.R           Audit logging framework
  helpers_taper.R           Taper profiles (wraps taperNOR)
  helpers_candidate_generation.R  Candidate log geometry, eligibility and volume
  helpers_dp_optimizer.R    Reference dynamic-programming bucking optimizer
  helpers_bucking_core.R    Backend per-tree core: candidates, pricing and DP on vectors,
                            one candidate pass for the runs of a comparison
  helpers_parallel.R        Workers (apter.workers), chunks, futures; the core shipped to workers
  helpers_bark.R            Swappable bark model (default taperNOR::barkNOR, B >= 0 floor)
  helpers_commercial_volume.R  NOK/m3 denominator and per-m3 summaries
  helpers_price_matrix.R    Price matrices: grids, diff, rows/columns, upload (long/wide), export;
                            price set B (states, b_set(), b_differs()), check_price_set(),
                            assortment xlsx I/O; Phase 4b definition format (performance cases)
  helpers_comparison.R      A vs. B: pattern changes, NOK/m3 comparison
  helpers_heights.R         Height imputation for tree lists (fieldPlot wrapper, FinnHoyde curves)
  helpers_tree_list.R       Tree lists: read, validate, impute heights, taperNOR range, template

audits/
  audit_14_validate_first_helpers.R          Products, valuation, price-matrix validation
  audit_15_core_engine_helpers_regression.R  Regression of all engine helpers
  audit_16_sampled_plot_backend_smoke.R      Backend smoke + fast/reference optimizer equivalence
  audit_17_app_smoke.R                       Headless app smoke test (shinytest2)
  audit_18_bark_and_commercial_volume.R      Bark model and commercial-volume denominator
  audit_19_ties_and_scenarios.R              Tie-breaking, price-matrix scenarios, pattern comparison
  audit_20_assortments_and_volume_methods.R  Volume methods, custom assortments, per-product price cap
  audit_21_tree_lists_and_heights.R          Tree lists, height imputation, tree-list runs
  audit_22_performance_equivalence.R         Workers, chunking, comparisons, main baseline, app jobs
  audit_23_assortment_table_and_price_sets.R Assortment table, default limits, rows/columns, price set B
  prototype_reference/      Frozen audits 05–12

scripts/
  97_fill_31dm_sawlog_price_gap.R         Fills the 31 dm sawlog price gap by interpolation
  98_create_github_safe_processed_data.R  Sanitizes data_processed/ into data_processed_github/
  99_cleanup_project_structure.R          One-off post-prototype cleanup (already run)
  run_prototype_00_prepare_nfi_data.R     Runs the frozen Oracle extraction under the current root
  phase4b_save_default_baseline.R         Saved the pre-Phase-4b default results (run once on e8b1f05; do not rerun)
  performance_save_main_baseline.R        Saved main's results before the Performance phase (run once on 3fa0a9d; do not rerun)
  performance_tree_lists.R                Seeded synthetic tree lists (100, 1000, 5000 trees)
  performance_cases.R                     Run cases shared by the baseline, check, benchmark and audit 22
  performance_check.R                     Quick check of every case against the main baseline
  performance_profile.R                   Per-tree stage profile (step 1 of the phase)
  performance_benchmark.R                 Benchmark timings (outputs/performance/benchmarks.csv)
  phase4c_limits_change_report.R          What the Phase 4c default diameter limits change against the baselines
  app_backend/
    00_precompute_taper_profiles_github.R     Precomputes the taper profile cache for the app
    01_run_sampled_plot_bucking.R             Backend used by the app (fast DP optimizer): NFI
                                              sample runs and tree-list runs (run_tree_list_bucking())
    02_benchmark_dp_optimizer_alternatives.R  DP optimizer benchmark (v3 adopted)
  prototype_reference/      Frozen prototype scripts 00–15

inputs/
  ProductData_spruce_pine_default.csv     Product definitions
  LengthClasses_spruce_pine_default.csv   Allowed log lengths per product
  price_matrices/           PriceMatrix_long and per-product matrices (default 2021)
  price_matrix_sources/     Source spreadsheets for the price matrices

data_processed_github/      Sanitized NFI sample used by the app (committed)
  nfi_plots_github.parquet
  nfi_tree_plot_stratum_ready_github.parquet
  sanitization_manifest.txt Record of what was removed or replaced

outputs/
  helper_validation/        Audit 14 results
  core_engine_helpers_regression/  Audit 15 results and regression reference data
  sampled_plot_backend_smoke/      Audit 16 results
  app_smoke/                Audit 17 results
  commercial_volume/        Audit 18 results
  scenario_comparison/      Audit 19 results
  assortments/              Audits 20 and 23 results and the pre-Phase-4b default-results baseline
  tree_lists/               Audit 21 results
  performance/              Audit 22 results, main baseline, profile, benchmarks, Phase 4c limits report
  app_cache/                Taper profile cache (local, regenerated by app_backend/00)
  stratum_availability/     Species/hogstkl/abonitet/fylnr combinations with enough plots

VERSION                     Release number (internal since Phase 4c; bump by hand)

docs/
  development.md            This file
  plans/                    Roadmap (PLAN.md)
```

### Not in the repository

| Item | Reason |
|------|--------|
| `data_raw/` | Raw NFI data are read directly from Oracle |
| `data_processed/` | Processed private NFI data (~15 MB); local only |
| Oracle credentials, `.Renviron` | Never committed |
| `outputs/_archive/` | Archived prototype outputs |
| `outputs/stem_profiles/`, `optimizer_tests/`, `selected_stratum_summary/`, `mixed_species_example_bucking/`, `stratum_tests/` | Derived from private NFI trees or large; regenerate locally |
| `inputs/*.rds` | Binary caches of the CSV inputs |
| `logs/`, `figures/presentation_2026-10/` | Local logs and presentation material |

---

## Project root

No path is hardcoded. The app, backend, scripts and audits resolve the
project root in this order:

1. `APTER_ROOT` environment variable (`get_root_dir()` then also reads the
   option `apter.root`)
2. Fallback: the app uses the parent of `app/`; `get_root_dir()`
   (`R/helpers_paths.R`), the backend, scripts and audits walk up from the
   working directory to the first folder containing `R/helpers_paths.R`

If the resolved folder does not contain `R/helpers_paths.R`, they stop with a
clear error. The app exports the resolved root as `APTER_ROOT` so the backend
agrees. Scripts that run before `R/helpers_paths.R` is sourced carry an
inline copy of the same logic.

The legacy `NFIBUCK_ROOT` variable and `nfibuck.root` option are no longer
read (removed in the 2026-10 rename).

---

## App appearance

The look follows NIBIO's ForestSens (Oracle APEX Redwood style; reference
screenshots in `docs/design_reference/`) with the "Forest & sand" palette of
Phase 4c: forest green `#3F6E58` is the only strong colour (logo, primary
buttons, active tab, status icons), secondary buttons are sand `#EAE5DC`.
Everything is our own: colours
are the tokens in `app/theme.R` (`APTER_TOKENS`, `APTER_CHART`), component
styles are in `app/www/apter.css`, and building blocks are in
`app/helpers_ui.R`. No inline colours in `app_ui.R` / `app_server.R`, and no
Oracle or ForestSens assets.

- Fonts are self-hosted from `app/www/fonts/` (no Google Fonts request at
  runtime): Inter for the UI (Source Serif 4 is still in `fonts/` but unused
  since the Home page went). Both are SIL OFL 1.1; keep the licence files
  next to the font files.
- Chart colours were checked with the dataviz palette validator (headless
  Chrome, all pairs, white surface; 2026-10-13). Sawlog green `#3F6E58`,
  pulpwood sand `#B38A52`, waste grey `#C4BFB8`, custom assortments charcoal
  `#3A3836`: worst CVD dE 13.3, worst normal-vision dE 16.6. The proposed
  `#C9A97A` / `#A39E98` failed (normal-vision dE 8.0 between them). A is
  green, B sand; the delta chart uses a neutral grey because the sign is
  shown by bar direction.
- Audit 17 saves screenshots of both tabs, the results and the comparison
  to `outputs/app_smoke/screenshots/` for review.

## Running the audits

Run from the project root, or set `APTER_ROOT`. Each audit writes a CSV and a
text log under `outputs/` and ends with a verdict: PASS, ACCEPTED WITH
REVIEW, or FAIL. Re-run all audits (14–23) at the end of every development phase.

**Prerequisites**

```r
install.packages(c("arrow", "readr", "dplyr", "shiny", "bslib", "reactable",
                   "rhandsontable", "readxl", "writexl", "future", "promises", "later"))
devtools::install_git("https://github.com/SmartForest-no/taperNOR")
remotes::install_github("lennartnoordermeer/fieldPlot")   # tree-list heights (nlme)
# Audit 17 only:
install.packages(c("shinytest2", "chromote"))   # plus Chrome
```

`chromote::find_chrome()` must return a path for audit 17; otherwise install
Chrome or set `CHROMOTE_CHROME` to a Chromium-based browser.

| Audit | Command | Needs |
|-------|---------|-------|
| 14 — first helpers | `Rscript audits/audit_14_validate_first_helpers.R` | Nothing beyond the repo |
| 15 — engine regression | `Rscript audits/audit_15_core_engine_helpers_regression.R` | `outputs/stem_profiles/stem_profiles_example_500trees.parquet` (from frozen prototype script 02, which needs `data_processed/`) |
| 16 — backend smoke | `Rscript audits/audit_16_sampled_plot_backend_smoke.R` | Taper profile cache (`scripts/app_backend/00_…`) |
| 17 — app smoke | `Rscript audits/audit_17_app_smoke.R` | shinytest2, chromote, Chrome; use `Rscript`, not `source()`; runs the app with `APTER_N_PLOTS=5` |
| 18 — bark and commercial volume | `Rscript audits/audit_18_bark_and_commercial_volume.R` | taperNOR; taper profile cache for its backend runs |
| 19 — ties and scenarios | `Rscript audits/audit_19_ties_and_scenarios.R` | readxl, writexl; taper profile cache |
| 20 — assortments and volume methods | `Rscript audits/audit_20_assortments_and_volume_methods.R` | readxl, writexl, taperNOR; taper profile cache; `outputs/assortments/baseline_phase4b_default.rds` |
| 21 — tree lists and heights | `Rscript audits/audit_21_tree_lists_and_heights.R` | fieldPlot, nlme, taperNOR, readxl; taper profile cache (check 6) |
| 22 — performance equivalence | `Rscript audits/audit_22_performance_equivalence.R` | future, promises, later, taperNOR; taper profile cache; `outputs/performance/baseline_main_3fa0a9d.rds`; uses up to 15 workers |
| 23 — assortment table and price sets | `Rscript audits/audit_23_assortment_table_and_price_sets.R` | readxl, writexl, taperNOR |

Audit 16 checks 14–15 rebuild every tree's priced input with the per-tree
data.frame pipeline (`audit_dp_inputs()` in `R/helpers_audit.R`) and check
that `.optimize_dp_fast()` and the backend's own result agree with the
reference `optimize_one_tree_dp()` on every tree of three backend runs.

---

## Performance

The Performance phase (2026-10-13) changed how the backend computes, not
what: every result equals main before the phase (audit 22 check 5; baseline
`outputs/performance/baseline_main_3fa0a9d.rds`, never regenerated).

**Where the time went (main, 1000 trees, one core).** 47.5 s in the per-tree
loop: candidate generation 40 % (19 ms/tree), DP 32 % (15 ms/tree), data.frame
subsetting for the eligibility filter and DP input 16 %, pricing 8 %.
taperNOR took 1.7 s and profile conversion 3.8 s (quadratic: one scan of the
profile table per tree). About 12,800 candidates per tree, 11,700 of them
eligible, for 3.3 selected logs. Profile: `scripts/performance_profile.R`,
summary in `outputs/performance/profile_summary.txt`.

**What changed.**

- `profiles_to_profile_vectors()` splits the table once (3.5 s → 0.06 s).
- Bucking core on plain vectors (`R/helpers_bucking_core.R`):
  - product parameters per species built once per run;
  - candidates generated with `rep()`, diameters read from the 10 cm grid, only usable candidates kept, with five columns;
  - pricing through `.price_vectors()`, the engine of `lookup_prices()`;
  - the DP on vectors (`.dp_select()`; `.optimize_dp_fast()` wraps it);
  - result rows built only for the selected logs.
- Comparisons: the runners take `scenario = list(price_matrix_path, products, length_classes)`. Both runs are priced from one candidate pass for the union of their products, and the scenario's result is returned as `$scenario`. Runs whose shared products are defined differently get separate passes. There is no cache between runs.
- Runners are prepare → buck → finish jobs (`prepare_*_job()`, `buck_job()`, `finish_job()`). Trees are bucked in `treid` order.
- Workers (`R/helpers_parallel.R`):
  - `future` multisession (PSOCK, no forking: Windows and Linux).
  - Trees are split into contiguous chunks and combined in chunk order, so results do not depend on the number of workers or the chunking.
  - Each chunk gets the bucking core as one environment (helper files, `helpers_utils.R` first, sourced with parent `package:stats`), so workers need no setup.
  - taperNOR for tree lists is chunked the same way.
- App (`app/helpers_jobs.R`):
  - a run is a promise chain run by a `shiny::ExtendedTask`, with progress per chunk;
  - Run is disabled while a task runs;
  - futures are queued and polled with `later`, because `promises::future_promise()` 1.3.2 calls the plan function directly, which future ≥ 1.40 rejects.

**Options.**

| Option | Default | Meaning |
|--------|---------|---------|
| `apter.workers` | `parallel::detectCores() - 1` | Parallel workers. 1 runs in the calling process without `future`. |
| `apter.chunk_size` | `max(25, ceiling(n / (4 × workers)))` | Trees per chunk |

The `cores − 1` default assumes **one** R process on the machine. On a
multi-user server with several R processes (e.g. several app processes, or
an app plus batch jobs), set `apter.workers` so that processes × workers ≤
cores. One app process starts its workers once (`app/setup.R`) and shares
them between its sessions.

**Benchmarks** (`scripts/performance_benchmark.R`, remote desktop: Windows
Server 2022, 16 cores, R 4.5.1, 2026-10-13). Synthetic tree lists, wall-clock
seconds of `run_tree_list_bucking()` including taperNOR, warm workers; single
run at the default 2021 prices, comparison with a changed scenario matrix.
Starting the pool takes 1.3–1.7 s, once per R process.

| Trees | Mode | 1 worker | 4 workers | 15 workers |
|------:|------|---------:|----------:|-----------:|
| 100 | single run | 1.6 | 0.8 | 0.8 |
| 100 | comparison | 2.5 | 1.1 | 1.1 |
| 1000 | single run | 14.9 | 5.3 | 3.4 |
| 1000 | comparison | 22.5 | 8.2 | 4.4 |
| 5000 | single run | 73.5 | 25.4 | 11.2 |
| 5000 | comparison | 111.3 | 38.4 | 15.8 |

For reference, main before the phase: 1000 trees about 51–56 s for a run
and about 110 s for a comparison (two runs), on one core.

---

## Prototype history and frozen files

`scripts/prototype_reference/` (scripts 00–15) and
`audits/prototype_reference/` (audits 05–12) are the original, validated
prototype and its audits. They are a matched, frozen pair: audit 14 checks
19–20 assert the scripts still exist and were not modified after script 14.
Do not edit them, even to fix the old name or paths in them.

Script 00 (the Oracle extraction) hardcodes the old
`S:/…/optBuck/nfiBuck` root. Run it only through the wrapper, which
substitutes the current root in memory and leaves the file untouched:

```sh
Rscript scripts/run_prototype_00_prepare_nfi_data.R --dry-run   # check the substitution
Rscript scripts/run_prototype_00_prepare_nfi_data.R             # run (needs Oracle)
```

---

## Data pipeline

```
Oracle (SKOG.NORGE_TRE_2024_V, SKOG.NORGE_FLA_2024_V)
  └─ scripts/run_prototype_00_prepare_nfi_data.R  -> data_processed/   (private, local)
       └─ scripts/98_create_github_safe_processed_data.R
            -> data_processed_github/   (sanitized, committed; read by the app)
                 └─ scripts/app_backend/00_precompute_taper_profiles_github.R
                      -> outputs/app_cache/taper_profiles_github/   (taper cache)
```

- Oracle access uses the keyring entry `REDACTED41_medium` / `REDACTED42_pluss`.
- Sanitization replaces the original `plot_id` with a synthetic
  `plot_id_github`; the mapping is never written to disk. See
  `data_processed_github/sanitization_manifest.txt`.
- Re-running script 98 rewrites the committed sanitized data. Do it only
  deliberately, and re-run the audits afterwards.
- Data-privacy rules, including the one authorised coordinate-export
  exception, are in [CLAUDE.md](../CLAUDE.md#data-privacy--read-before-touching-plotcoordinate-data).

---

## Files that still say "nfiBuck"

These keep the old name on purpose:

| What | Why |
|------|-----|
| `scripts/prototype_reference/`, `audits/prototype_reference/` | Frozen (audit 14 checks 19–20) |
| `data_processed_github/sanitization_manifest.txt` | Record of the sanitization run; regenerating it rewrites the committed data |
| `outputs/helper_validation/helper_price_matrix_validation_summary.txt` | Written by frozen prototype script 14 |
| `outputs/_archive/`, `logs/_archive/`, prototype output folders, the taper cache manifest | Records of past runs |
| `docs/plans/phase0.md`, `docs/plans/PLAN.md` | Historical plan; "formerly nfiBuck" |
| `scripts/99_cleanup_project_structure.R` header, `scripts/run_prototype_00_prepare_nfi_data.R` | Describe the old name and the old root that the wrapper replaces |
| This file, CLAUDE.md | "formerly nfiBuck" notes |

---

## Phase 4c: interface and default limits

- **Assortment table.** The app keeps all assortments, base and custom, in one
  table (`ASSORTMENT_TABLE_COLS`, cm, one row per ProductKey); `products_from_defs()`
  turns it into ProductData and LengthClasses for each run. Priority is derived
  (matrix 1, flat 2, waste 99); a matrix-priced product's lengths are the
  columns of the run's matrix, so B can have its own lengths. The Phase 4b
  definition format (`custom_assortment_rows()`, `products_for_run()`, `UseIn`,
  explicit Priority) stays for `scripts/performance_cases.R` and audit 20, whose
  baselines depend on it.
- **A and B.** B holds its own prices for every product and a state per product
  (`same`, `own`, `none`). A run compares A and B when `b_differs()`. With the
  default table and 2021 prices, A runs through the default price file and
  products, as before; audit 23 check 10 shows the explicit path gives the same.
- **Fixed sampling.** NFI runs use 200 plots and seed 42 (`app/setup.R`);
  `APTER_N_PLOTS` overrides the number (audit 17 uses 5).
- **Default diameter limits** (optBuck's example.hpr, under bark): spruce sawlog
  max top 60 / butt 65 cm, pine sawlog 50 / 60, pulpwood 70 / 70. The baselines
  predate them, so audits 20 (check 8), 22 (check 5) and
  `scripts/performance_check.R` run with `pre_phase4c_limits()`. What the new
  limits change is in `outputs/performance/phase4c_limits_change.txt`
  (`scripts/phase4c_limits_change_report.R`): only the NFI spruce stratum, 6 of
  339 trees and 22 logs; value 27 268.08 -> 27 150.33 NOK, commercial volume
  70.30 -> 69.90 m³, NOK/m³ 387.90 -> 388.41 (value falls less than the
  commercial volume, so NOK/m³ rises slightly).
- `inputs/ProductData_spruce_pine_default.rds` is a stale prototype artifact
  (no PriceType column; read only by frozen prototype script 03) and was not
  updated.

