# optBuck (the R package optBuck) development notes

These notes are for developers working on optBuck. The partner-facing overview
is in [README.md](../README.md). AI-assistant context (domain terms,
load-bearing assumptions, data-privacy rules) is in [CLAUDE.md](../CLAUDE.md).
The development roadmap is in [docs/plans/PLAN.md](plans/PLAN.md).

optBuck was called **nfiBuck** until the 2026-10 rename, and had another working
name until 2026-11-06 (renamed in code, files and docs). The GitHub
repository is `SmartForest-no/optBuck` (since 2026-11-10; its history joins the history of the
earlier optBuck package, see PLAN.md B8); the local folder can be renamed freely.

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
| Custom assortments and volume methods (Phase 4b) | Audit 20 PASS (43/43): volume methods by hand (Mid included), custom assortments winning and losing, default results identical to the pre-Phase-4b baseline (with the pre-Phase-4c limits), validation, upload round trips, per-product price cap, prices kept when an assortment is edited |
| Tree lists and height imputation (Phase 5) | Audit 21 PASS (28/28): FinnHoyde curves and calibration by hand, fieldPlot wrapper (single species, time limit), validation of broken files, NFI trees as a tree list give identical results, end-to-end runs |
| Shiny app (Phase 4d layout) | Audit 17 (headless shinytest2 smoke test with screenshots): two tabs, Scenarios A and B side by side, "optBuck" name, B states, rows and columns, automatic comparison when B differs, paged result tables, simplified assortment form without a blurred backdrop, a custom assortment through the form, an uploaded tree list, B's own assortments (rule change, B-only, removed from B only, comparison, A downloaded and uploaded to B), Run disabled with progress while a run task runs |
| Assortment table and price sets A/B (Phase 4c) | Audit 23 PASS (10/10): base products through the table and back, default limits, derived Priority, lengths from the matrix columns, row/column helpers, class gaps, B states, xlsx round trip, B with its own lengths in the backend |
| Scenarios A and B (Phase 4d) | Audit 24 PASS (11/11): derived rounding/step/type, assortment only in B, a B rule change alone makes the scenarios differ, removed from B only, make equal (one assortment, whole scenario), backend comparison with B's own rules and a matrix-priced pulpwood in B, renamed Waste unchanged, scenario file round trip, demo files, price type per scenario and copy_prices() |
| Import from .hpr (hpr Phase A) | Audit 25 PASS (16/16, incl. an imported Mid product): reading the synthetic fixture (only the part before the first stem; .gz, .zip, whole-file fallback), product definitions, species guess, best-grade filter, mapping onto the assortment table, lengths rounded up, forbidden cells empty, a fully forbidden inner class as empty rows, price volume on the class lower limit, file errors and partial-problem warnings, a tree-list run with the imported scenario, scenario file round trip and comparison |
| .hpr stems (hpr Phase B) | Audit 26 PASS (checks 1-13; 13, 2026-10-28: several files, unique keys, doubled counts, same per-stem results as one file, failing files, no file names): reading the fixture's 13 stems (chunk size and extractor independent), unusual XML, DBH at 1.3 m above ground from the profile (not the file), the measured-only 10 cm grid, stump 0, nothing above the last measured point, barkNOR, species mapping and dropped stems, .gz/.zip, files without profiles or stems, no coordinates or names in any output, an A/B run, workers; audit 17 check 31 |
| Result tables (log tally, DBH classes) | Audit 27 PASS (10/10): tally totals equal the run's per assortment and in total (waste not tallied), the price-grid layout with unpriced cells, logs above the cap in the top class, flat-priced per length, B − A (common axes, one-sided, matrix vs flat), DBH class limits and totals, xlsx/CSV downloads, B − A in the xlsx only for shared assortments, type groups (Sawlog, Pulpwood, Other) for scenarios with different assortments, the Products table (`assortment_summary()`: sums, contributions add up to NOK/m³, "–" for a missing assortment); audit 17 checks 14–17, 27 and 33 (four result tabs since 2026-10-23) |
| Norwegian language option (roadmap Phase 1) | Audit 28 PASS (7/7): the translation table (ids, placeholders, conflicts), every id in the code, no English string literals left in the app, every message template round-trips, 68 real backend messages (tree list, .hpr import, .hpr stems, prices) translate without a miss, downloads per language, tree counts; audit 17 checks 34–39 drive a Norsk session |
| Bulk editing of price matrices (roadmap Phase 4) | Audit 29 PASS (5/5): range + amount, range %, row price + length correction (2 decimals, never below 0), forbidden cells stay empty in all three and the scenario stays valid; the app (shinytest2): a range +10 NOK/m³ in A changes those cells only and not B, the fill dialog; screenshots in outputs/app_smoke/review/audit29/ (gitignored, cleared by each run) |
| Performance (vectorised core, comparisons, workers, ExtendedTask) | Audit 22 PASS (8/8): 1 vs 15 workers and chunk sizes identical, a comparison equals two separate runs, all cases equal main before the phase (with the pre-Phase-4c limits), the app's background job equals the backend |

The 31 dm sawlog price gap was filled by interpolation on 2026-08-03
(a one-off script removed in the 2026-10 cleanup, see git history). Rows are flagged
`PriceSource = "Interpolated"`.

---

## Repository layout

The repository is the R package `optBuck` (roadmap 8, step 8): `DESCRIPTION`, `NAMESPACE` (roxygen2 from `R/api.R`),
`LICENSE` (MIT, NIBIO), `NEWS.md`, `README.md`, `CITATION.cff`, `inst/CITATION`, `man/`, `tests/testthat/`, `.Rbuildignore`
(the folders below that are not part of the package: `audits/`, `scripts/`, `docs/`, `outputs/`, `figures/`, `logs/`,
`data_raw/`, `data_processed/`). Check it with `R CMD build .` and `R CMD check --no-manual optBuck_*.tar.gz`; run the
audits with `Rscript scripts/run_audits.R` (they use `pkgload::load_all()`).

```
inst/app/                    Shiny app, files sourced explicitly by app.R
  app.R                     Entry point: loads the package (load_all() in development), attaches its internals,
                            sources the files below, calls shinyApp(ui, server)
  setup.R                   Data, default assortments and prices (APP_DEFS, APP_PM_DEFAULT),
                            HKL/bonitet/county choices, all plots (OPTBUCK_N_PLOTS for tests),
                            tree counts for the large-run note, APP_VERSION
  theme.R                   Design tokens, chart colours, bslib (Bootstrap 5) theme
  helpers_ui.R              Cards, summary boxes, reactable tables (columns sized
                            to content), server-side paged tables, base-R plots (A/B bars),
                            grid renderers; APP_DISPLAY_NAME ("optBuck")
  helpers_validation.R      Inputs -> backend params; stratum plot and tree counts; run-time estimate
  helpers_upload.R          Price files for the runs; a price upload merged into one scenario
  helpers_jobs.R            Runs as background jobs: promise chain over the workers
                            (shiny::ExtendedTask), progress per chunk
  app_ui.R                  `ui`: page_navbar with Assortment specifications and prices, Simulator
                            (not ui.R: Shiny treats ui.R + app.R as two app formats)
  server_scenarios.R        scenario_server(): the Assortment specifications and prices tab (Scenarios A and B)
  app_server.R              `server`: Simulator (tree sources, runs, results)
  www/
    optbuck.css               Component styles (option 3 "Combination" look)
    optbuck.js                Shows the progress bar as soon as Run is clicked; the language
                            switch (static texts by data-i18n id, remembered per browser)
    optbuck-icon.svg          App icon and favicon (original artwork)
    fonts/                  Inter and Source Serif 4 (.woff2, SIL OFL 1.1; licences alongside);
                            Inter .ttf (400/600) for the ggplot2 figures (registered with systemfonts)

R/                          Core engine helpers
  helpers_paths.R           Project root and path utilities (get_root_dir())
  helpers_i18n.R            Norsk / English: tr(), tr_msg(), tr_value(), number formats and
                            result CSVs per language (inst/extdata/inputs/i18n/translations.csv)
  helpers_products.R        Product definitions, assumption checks, the assortment table
                            (defs_from_products(), products_from_defs()), validate_products()
  helpers_volume_methods.R  Price-volume methods (Top cylinder, Top frustum, solid volume, Norwegian mid, Mid)
  helpers_valuation.R       Price matrix loading, validation, price lookup with per-product class cap
  helpers_audit.R           Audit logging framework
  helpers_taper.R           Taper profiles (wraps taperNOR)
  helpers_candidate_generation.R  Candidate log geometry, eligibility and volume
  helpers_dp_optimizer.R    Reference dynamic-programming bucking optimizer
  helpers_bucking_core.R    Backend per-tree core: candidates, pricing and DP on vectors,
                            one candidate pass for the runs of a comparison
  helpers_parallel.R        Workers (optbuck.workers), chunks, futures; the core shipped to workers
  helpers_bark.R            Swappable bark model (default taperNOR::barkNOR, B >= 0 floor)
  helpers_commercial_volume.R  NOK/m3 denominator and per-m3 summaries
  helpers_price_matrix.R    Price matrices: grids, diff, rows/columns, upload (long/wide), export;
                            two scenarios (assortment_diff(), scenarios_differ(),
                            copy_assortment(), copy_prices()), check_price_set(), scenario xlsx I/O
                            (scenario_export()/_import()); Phase 4b definition format (performance cases)
  helpers_comparison.R      A vs. B: pattern changes, NOK/m3 comparison
  helpers_heights.R         Height imputation for tree lists (fieldPlot wrapper, FinnHoyde curves)
  helpers_tree_list.R       Tree lists: read, validate, impute heights, taperNOR range, template
  helpers_hpr.R             StanForD 2010 .hpr: species groups, products, price matrices -> a scenario (Import from .hpr)
  helpers_hpr_stems.R       .hpr stems (hpr Phase B): streamed stem reader, checks, measured profiles (stump 0)
  helpers_hpr_export.R      .hpr export (Phase 5a): a scenario's result as a StanForD 2010 .hpr, copied from the source, no coordinates
  helpers_pin_export.R      .pin export (Phase 5b): a scenario as a StanForD 2010 .pin (fixed minimal header, no coordinates)
  helpers_hpr_logs.R        Recorded logs of an .hpr file (bucking check, roadmap 8): chunked whitelist reader, permitted grades
  helpers_check.R           Bucking check (tab 5): recorded logs valued like candidates, best possible bucking, the tables; bucking_check(stage = )
  helpers_nfi_cache.R       NFI stem profiles built on first use (progress callback), same profiles as the precompute script
  helpers_result_tables.R   Log tally per price matrix (stocknota) and value by DBH class
  helpers_price_tuning.R    Price-matrix tuning (roadmap 7, no UI yet): grid, target, FG, price update, tune_prices()
  helpers_tree_selection.R  NFI trees of a run (stratum, plots, dbh/height filter): select_nfi_trees(),
                            used by the backend and the app's live tree counts

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
  audit_24_scenarios.R                       Scenarios A and B with their own assortments; demo files
  audit_25_hpr_import.R                      Import from .hpr (parser, mapping, price lengths, errors, warnings, run)
  audit_26_hpr_stems.R                       .hpr stems (reader, profiles, stump 0, drops, privacy, A/B run)
  audit_27_result_tables.R                   Log tally and value by DBH class (totals, grid layout, cap, flat, B − A, downloads, type groups)
  audit_28_translations.R                    Translation table, ids in the code, no English left, backend messages, downloads per language
  audit_31_hpr_export.R                      .hpr export (round trip, stems copied unchanged, no coordinates, product keys, two files, re-bucking)
  audit_32_pin_import_export.R               .pin import and export (both layouts, key fallbacks, BuckingCriteria, species guess, round trip, no coordinates, errors)
  audit_29_price_bulk_edit.R                 Bulk editing of price matrices (range +/- NOK or %, row price + length fill, empty cells, app)
  audit_36_bucking_check.R                   Bucking check on the synthetic fixture (reader, valuation, waste zones, broken top, used products only, stages, NFI profiles on demand)
  audit_37_check_vs_old.R                    Reader, profiles and permitted grades against the saved old optBuck output, private files (REVIEW when absent)
  audit_38_check_texts.R                     Texts of tab 5 and of the NFI profile build in both languages
  audit_39_name_and_paths.R               The tool name (no old name left but Norwegian words and the remote's address) and the path rules (read-only: system.file(), writable: R_user_dir())
  audit_40_install_gate.R                    The install gate: fresh library and working directory, no root variables, run_app(), six tabs, tab 5 three times, NFI run, PDF in both languages
  audit_41_example_hpr.R                     The anonymised example file inst/extdata/example.hpr: size, no coordinates, neutral identity fields, readers, bucking check, schema
  audit_30_stem_grades.R                     .hpr stem grades and cut at grade changes (grade grid, StartGrade fallback, permitted grades, forced cuts, workers)
  prototype_reference/      Frozen audits 05–12

scripts/
  98_create_github_safe_processed_data.R  Sanitizes data_processed/ into inst/extdata/nfi/
  run_prototype_00_prepare_nfi_data.R     Runs the frozen Oracle extraction under the current root
  phase4b_save_default_baseline.R         Saved the pre-Phase-4b default results (run once on e8b1f05; do not rerun)
  performance_save_main_baseline.R        Saved main's results before the Performance phase (run once on 3fa0a9d; do not rerun)
  performance_tree_lists.R                Seeded synthetic tree lists (100, 1000, 5000 trees)
  performance_cases.R                     Run cases shared by the baseline, check, benchmark and audit 22
  performance_check.R                     Quick check of every case against the main baseline
  run_audits.R                            Runs a set of audits in parallel processes (PASS/FAIL per audit, exit status)
  performance_profile.R                   Per-tree stage profile (step 1 of the phase)
  performance_benchmark.R                 Benchmark timings (outputs/performance/benchmarks.csv)
  phase4c_limits_change_report.R          What the Phase 4c default diameter limits change against the baselines
  make_demo_files.R                       Writes demo/ (scenario and price files, example tree list)
  make_hpr_grades_fixture.R               Builds inst/extdata/inputs/test_fixtures/hpr_grades_synthetic.hpr (audit 30) from the synthetic fixture: invented stems with stem grades
  make_hpr_test_fixture.R                 Builds inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr from optBuck's example.hpr (whitelist, no personal data; 13 invented stems)
  make_pin_test_fixture.R                 Builds inst/extdata/inputs/test_fixtures/pin_minimal_synthetic.pin and pin_full_synthetic.pin from literals (refuses coordinates and contacts)
  hpr_stems_benchmark.R                   Time and memory of reading and an A/B run on real .hpr files (local only; output never committed)
  app_backend/
    00_precompute_taper_profiles_github.R     Precomputes the taper profile cache for the app
    (01_run_sampled_plot_bucking.R is now R/run_bucking.R)  Backend used by the app (fast DP optimizer): NFI
                                              runs (all plots, or a sample) and tree-list runs
                                              (run_tree_list_bucking())
    02_benchmark_dp_optimizer_alternatives.R  DP optimizer benchmark (v3 adopted)
  prototype_reference/      Frozen prototype scripts 00–15

inst/extdata/inputs/
  ProductData_spruce_pine_default.csv     Product definitions
  LengthClasses_spruce_pine_default.csv   Allowed log lengths per product
  price_matrices/           PriceMatrix_long and per-product matrices (default 2021)
  price_matrix_sources/     Source spreadsheets for the price matrices
  i18n/translations.csv     The one translation table (Norsk / English) of the app
  test_fixtures/            hpr_example_synthetic.hpr: audits 25 and 26's .hpr file (definitions, prices, invented stems)

inst/extdata/nfi/      Sanitized NFI sample used by the app (committed)
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

demo/                       Demo files for the app (scripts/make_demo_files.R; audit 24)

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
| `outputs/stem_profiles/`, `optimizer_tests/`, `selected_stratum_summary/`, `mixed_species_example_bucking/`, `stratum_tests/` | Derived from private NFI trees or large; regenerate locally |
| `inst/extdata/inputs/*.rds` | Binary caches of the CSV inputs |
| `logs/`, `figures/presentation_2026-10/` | Local logs and presentation material |

---

## Project root

No path is hardcoded. The app, backend, scripts and audits resolve the
project root in this order:

1. `OPTBUCK_ROOT` environment variable (`get_root_dir()` then also reads the
   option `optbuck.root`)
2. Fallback: the app uses the parent of `inst/app/`; `get_root_dir()`
   (`R/helpers_paths.R`), the backend, scripts and audits walk up from the
   working directory to the first folder containing `R/helpers_paths.R`

If the resolved folder does not contain `R/helpers_paths.R`, they stop with a
clear error. The app exports the resolved root as `OPTBUCK_ROOT` so the backend
agrees. Scripts that run before `R/helpers_paths.R` is sourced carry an
inline copy of the same logic.

The legacy `NFIBUCK_ROOT` variable and `nfibuck.root` option are no longer
read (removed in the 2026-10 rename).

---

## App appearance

The look is option 3 "Combination" of
`docs/design_reference/optbuck_design_directions.html` (restyle,
2026-10-16; it replaced the ForestSens-like "Forest & sand" look and its
stripe). Since 2026-10-19 the page is light grey `#F3F4F3` (`--optbuck-page`)
behind white cards: Scenario A and B, the Simulator sidebar (a card of its
own) and the results card (`--optbuck-surface`; bs_theme `bg` stays white,
because Bootstrap's cards take their background from it). The navbar is
white with a hairline `#E6E6E6` rule, the
navbar tabs are grey with a black active tab, and cards have a 1 px border
and no shadow. Section labels are small uppercase grey, and forest green
`#00564b` (`OPTBUCK_GREEN`, one constant; was `#3F6E58` until 2026-11-09) is the only strong colour (primary buttons, focus, progress, the
result tabs' underline). Secondary buttons are `#EEF0EF`, inputs `#F3F4F3`.
Table headers are grey-green (`#6A726E` on `#F7F8F7`), A/B differences
a pale green derived from the green, and the summary boxes `#F7F8F7` without a border.
Audit 17 check 6 reads these colours back from the browser.
Everything is our own: colours
are the tokens in `inst/app/theme.R` (`OPTBUCK_TOKENS`, `OPTBUCK_CHART`), component
styles are in `inst/app/www/optbuck.css`, and building blocks are in
`inst/app/helpers_ui.R`. No inline colours in `app_ui.R` / `app_server.R`, and no
Oracle or ForestSens assets.

- Fonts are self-hosted from `inst/app/www/fonts/` (no Google Fonts request at
  runtime): Inter for the UI (Source Serif 4 is still in `fonts/` but unused
  since the Home page went). Both are SIL OFL 1.1; keep the licence files
  next to the font files. The ggplot2 figures use Inter's .ttf files
  (fontsource latin subset, 400 and 600), registered with
  `systemfonts::register_font()` in `inst/app/theme.R`; Shiny draws plots with
  ragg, which picks them up (falls back to sans without them).
- Chart colours were checked with the dataviz palette validator (headless
  Chrome, all pairs, white surface; 2026-10-16). The requested pulpwood
  `#C9A97A` and waste `#B9BFBC` failed (normal-vision dE 8.7, floor 15), so
  they were shifted slightly. The result is sawlog the green (`#00564b` since 2026-11-09), pulpwood
  sand `#C09A66` (since 2026-11-09 the pine colour `#c0704a`, `OPTBUCK_PINE`), waste grey `#CBD0CD` and custom assortments charcoal
  `#3A3836`: worst CVD dE 15.5, worst normal-vision dE 16.5. Scenario A is
  green and B sand `#C09A66` (since 2026-11-09 the pine colour; CVD dE 17.9 was for the sand). The delta chart uses a neutral
  grey because the sign is shown by bar direction. Log labels are dark on
  sand and grey.
- Figures (2026-10-19): ggplot2 with `theme_optbuck()` (Inter, white
  background, light grid). Height density per species in the Trees card
  (spruce `#00564b`, pine `#c0704a` since 2026-11-09; before `#3F6E58` and `#C9A97A` at CVD dE 22.2, normal dE 27.1). Since
  2026-10-22 the DBH density and the two Selected-logs bar figures are
  replaced by the result tables (below): the DBH-class figure (trees and
  NOK/m³ per class, same species colours, A solid / B dashed) and the log
  tally table.
- Result tables (2026-10-22): `R/helpers_result_tables.R` (pure functions
  on a run's selected logs and trees; audit 27): `log_tally()`,
  `log_tally_grid()`, `log_tally_diff_grid()`, `log_tally_long()`,
  `log_tally_xlsx()`, `dbh_class()`, `dbh_class_summary()`. The comparison
  tab lost `tbl_cmp_overview` (= the Summary's A/B grid) and `bar_cmp_value`
  (= the Products tab's A/B value figure).
- Audit 17 saves its screenshots to `outputs/app_smoke/review/audit17/` (see Conventions
  under "Running the audits").

## Languages (Norsk / English; roadmap Phase 1, 2026-10-27)

The app starts in Norsk (bokmål); the navbar switch changes to English
without a reload, and the browser remembers the choice (localStorage
`optbuck.lang`; `?lang=en` in the URL forces a language). Every text is in
one table, `inst/extdata/inputs/i18n/translations.csv` (`id, kind, en, no, note`):

- **ui**: app text, looked up by id. Static UI uses `t_(id, ...)`
  (`inst/app/helpers_ui.R`): a span carrying the id, swapped by `www/optbuck.js`
  from the JSON of both languages in the page head. The server uses
  `tr(id, ..., lang = lang())`, so its outputs re-render on a switch.
- **msg**: backend message templates, matched by their English text (the
  `sprintf` format as in the code). The backend stays English, so scripts,
  audits and baselines see exactly the text they saw before; the app
  translates when it shows a message (`tr_msg()`): exact match, then
  templates (most specific first), with captured arguments translated in
  turn (reasons, lists, "name: message" pairs) and numbers in the
  language's format. Unmatched text stays English and is recorded
  (`i18n_misses()`; audit 28 check 5 requires none).
- **value**: data values shown to users ("spruce", "Sawlog", "Top
  cylinder", "Total", stage names), translated exactly by `tr_value()`.

To add a text: add a row (both languages, same placeholders in the same
order; `%1$s` is allowed in `no`), use its id in the code, and run audit 28.
To add a backend message that users see: write it in English as usual and
add a `msg` row with the same format string. Audit 28 check 3 fails on a
prose-like string literal in `app/*.R` that is not in the table.

Downloads: result tables follow the language (file names, column names,
values; Norsk CSVs with `;`, a decimal comma and a UTF-8 BOM,
`write_download_csv()`; the tally xlsx through `log_tally_xlsx(lang =)`).
Exchange files keep their machine format in both languages: the scenario
xlsx, price files and the tree-list template (only their file names are
translated), so a file saved in one language loads in the other. Numbers
on screen: Norsk shows a decimal comma and a space as thousands separator
(`fmt_num()`; reactable cells drawn from the numeric value, so sorting
stays numeric; the price grid with numbro's nb-NO culture, which also reads
a typed "430,5"; the altitude slider via ion.rangeSlider).

Not translated: names (optBuck, taperNOR, fieldPlot, FinnHoyde, StanForD),
units, user data (assortment and product names, county names), and
unexpected R errors passed through. Base assortment names (Sawlog,
Pulpwood, Waste) are shown translated; saving the edit form with the shown
name unchanged keeps the stored name, so the key stays.

## Running the audits

Run from the project root, or set `OPTBUCK_ROOT`. Each audit writes a CSV and a
text log under `outputs/` and ends with a verdict: PASS, ACCEPTED WITH
REVIEW, or FAIL. Re-run all audits (14–28) at the end of every development phase.

**Conventions (cleanup phase, 2026-10-28)**

- **Review screenshots** go to one gitignored folder, `outputs/app_smoke/review/`.
  Audit 17 writes its screenshots to `review/audit17/` (English ones as numbered
  files, the Norsk session with the prefix `no_`, phase 2 with `phase2_`) and clears
  only that subfolder at the start of each run. Ad-hoc screenshots taken for a
  phase go directly in `review/` and are emptied by hand at the start of the next
  phase. Never create `review_<phase>/` folders.
- **Audit outputs and timestamps.** The audits' `.txt`/`.csv` files are tracked as
  evidence, so a rerun must not change them without a reason:
  `write_audit_outputs()` (`R/helpers_audit.R`) writes the previous bytes back when
  the new log differs only in the `Run: <timestamp>` line. Logs that contain
  timings (audit 21 check 4, audit 17's runtimes) still change and are
  committed as before.

**`scripts/run_audits.R`** runs a set of audits (and `scripts/performance_check.R`)
as separate Rscript processes and prints one PASS / REVIEW / FAIL line per item,
then the total time; exit status 1 if anything FAILs.

    Rscript scripts/run_audits.R                 # everything: every audit from 14 up and perf
    Rscript scripts/run_audits.R 16 20 perf      # a few (numbers, ranges 14-20, names)
    Rscript scripts/run_audits.R --jobs 4        # processes at a time (default 3)
    Rscript scripts/run_audits.R --17-last       # audit 17 alone at the end

Audit 17 (browser, about 4 min) starts first and takes one of the slots; the
others follow longest first. Each process gets `OPTBUCK_WORKERS` = `max(2, (cores - 1)
%/% jobs)` workers (`optbuck_workers()` reads it when the option `optbuck.workers` is
not set), so the processes do not oversubscribe the cores. Logs: `outputs/audit_runs/`
(gitignored). During a phase run only the affected audits; run the full suite once
before asking for a merge.

**Browser audits (17 and 29): own Chrome, always closed.** Each audit process
starts its own headless Chrome with a private, throw-away profile
(`audit_browser_start()` in `R/helpers_audit.R`; chromote's random debugging port)
instead of the default Chrome profile, which the user's own browser, leftover
headless Chromes and a parallel audit or session would share and lock. Starting
the app retries once on a fresh Chrome (`audit_app_driver()`). The Chrome and the
profile are closed on every exit path (`audit_browser_stop()`: `finally`, and at
R exit for a halted script): only the Chrome process this audit started, through
the handle chromote holds, never by process name, command line or port. An
uncaught driver error (for example a dropped Chrome connection) fails the checks
not yet run, with the error text, and the audit still writes its outputs. Audit
17 takes every screenshot through one function (`shoot()`), probes one right after
the start, and **fails** (check 22, and 22b for the Norsk session) if any
screenshot is missing, with the first error message. `run_audits.R` reports
orphaned headless Chromes from earlier runs (PIDs and a `taskkill` hint) but never
kills them, and kills the process trees of its own children if interrupted.

**Prerequisites**

```r
install.packages(c("arrow", "readr", "dplyr", "shiny", "bslib", "reactable",
                   "rhandsontable", "readxl", "writexl", "future", "promises", "later",
                   "xml2"))   # xml2: .hpr import
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
| 17 — app smoke | `Rscript audits/audit_17_app_smoke.R` | shinytest2, chromote, Chrome; use `Rscript`, not `source()`; runs the app with `OPTBUCK_N_PLOTS=5` |
| 18 — bark and commercial volume | `Rscript audits/audit_18_bark_and_commercial_volume.R` | taperNOR; taper profile cache for its backend runs |
| 19 — ties and scenarios | `Rscript audits/audit_19_ties_and_scenarios.R` | readxl, writexl; taper profile cache |
| 20 — assortments and volume methods | `Rscript audits/audit_20_assortments_and_volume_methods.R` | readxl, writexl, taperNOR; taper profile cache; `outputs/assortments/baseline_phase4b_default.rds` |
| 21 — tree lists and heights | `Rscript audits/audit_21_tree_lists_and_heights.R` | fieldPlot, nlme, taperNOR, readxl; taper profile cache (check 6) |
| 22 — performance equivalence | `Rscript audits/audit_22_performance_equivalence.R` | future, promises, later, taperNOR; taper profile cache; `outputs/performance/baseline_main_3fa0a9d.rds`; uses up to 15 workers |
| 23 — assortment table and price sets | `Rscript audits/audit_23_assortment_table_and_price_sets.R` | readxl, writexl, taperNOR |
| 24 — scenarios | `Rscript audits/audit_24_scenarios.R` | readxl, writexl, taperNOR, fieldPlot; `demo/` |
| 25 — hpr import | `Rscript audits/audit_25_hpr_import.R` | xml2, zip, writexl, readxl, taperNOR; `inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr` |
| 26 — hpr stems | `Rscript audits/audit_26_hpr_stems.R` | xml2, zip, taperNOR, future; `inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr` |
| 27 — result tables | `Rscript audits/audit_27_result_tables.R` | writexl, readxl, taperNOR |
| 28 — translations | `Rscript audits/audit_28_translations.R` | xml2, zip, writexl, readxl, taperNOR; `inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr` |
| 30 — stem grades | `Rscript audits/audit_30_stem_grades.R` | xml2, writexl, readxl, taperNOR, future; `inst/extdata/inputs/test_fixtures/hpr_grades_synthetic.hpr` |
| 31 — hpr export | `Rscript audits/audit_31_hpr_export.R` | xml2, zip, taperNOR, future; `inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr` (variants in tempfiles) |
| 36 — bucking check | `Rscript audits/audit_36_bucking_check.R` | xml2, arrow, future, taperNOR; `inst/extdata/inputs/test_fixtures/hpr_check_synthetic.hpr` (scripts/make_hpr_check_fixture.R) |
| 37 — check vs old optBuck | `Rscript audits/audit_37_check_vs_old.R` | private files in `C:/optbuck_private/` (REVIEW without them) |
| 38 — check texts | `Rscript audits/audit_38_check_texts.R` | none |
| 39 — name and paths | `Rscript audits/audit_39_name_and_paths.R` | git (falls back to every file) |
| 40 — install gate | `Rscript audits/audit_40_install_gate.R` | Chrome, shinytest2, processx; the owner's file 1 in `C:/optbuck_private/hpr/` (REVIEW without it); about 10 minutes |
| 41 — example file | `Rscript audits/audit_41_example_hpr.R` | xml2; the XSD and the build record in `C:/optbuck_private/` (REVIEW without them) |
| 32 — pin import and export | `Rscript audits/audit_32_pin_import_export.R` | xml2; `inst/extdata/inputs/test_fixtures/pin_minimal_synthetic.pin`, `pin_full_synthetic.pin` |

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
- App (`inst/app/helpers_jobs.R`):
  - a run is a promise chain run by a `shiny::ExtendedTask`, with progress per chunk;
  - Run is disabled while a task runs;
  - futures are queued and polled with `later`, because `promises::future_promise()` 1.3.2 calls the plan function directly, which future ≥ 1.40 rejects.

**Options.**

| Option | Default | Meaning |
|--------|---------|---------|
| `optbuck.workers` | `parallel::detectCores() - 1` | Parallel workers. 1 runs in the calling process without `future`. |
| `optbuck.chunk_size` | `max(25, ceiling(n / (4 × workers)))` | Trees per chunk |

The `cores − 1` default assumes **one** R process on the machine. On a
multi-user server with several R processes (e.g. several app processes, or
an app plus batch jobs), set `optbuck.workers` so that processes × workers ≤
cores. One app process starts its workers once (`inst/app/setup.R`) and shares
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
            -> inst/extdata/nfi/   (sanitized, committed; read by the app)
                 └─ scripts/app_backend/00_precompute_taper_profiles_github.R
                      -> outputs/app_cache/taper_profiles_github/   (taper cache)
```

- Oracle access uses a keyring entry (service and user names are placeholders `ORACLE_SERVICE` / `ORACLE_USER` in the frozen script; set the real ones locally, never commit them).
- Sanitization replaces the original `plot_id` with a synthetic
  `plot_id_github`; the mapping is never written to disk. See
  `inst/extdata/nfi/sanitization_manifest.txt`.
- Re-running script 98 rewrites the committed sanitized data. Do it only
  deliberately, and re-run the audits afterwards.
- Data-privacy rules, including the one authorised coordinate-export
  exception, are in [CLAUDE.md](../CLAUDE.md#data-privacy--read-before-touching-plotcoordinate-data).

### Which NFI trees a run bucks

One function decides it, `select_nfi_trees()` in
`R/helpers_tree_selection.R`. `prepare_sampled_job()` calls it, and so do
the app's live tree counts, so the sidebar box always counts exactly the
trees the run will buck (audit 17 check 4e). Don't reimplement it.

- **Only spruce and pine exist in the data.** Data preparation
  (`scripts/prototype_reference/00_prepare_nfi_2024_data.R`) maps `treslag`
  to `species_group` and keeps only spruce and pine; broadleaves and other
  species are dropped before sanitization. They are never counted or bucked.
- **Plots are selected by tree species, not by dominant species.** The
  stratum filter (maturity class, site class, county, altitude) is applied to the
  trees of the chosen species group; a plot is eligible if it has at least
  one such tree. There is no dominant-species rule.
- **Only trees of the chosen species are bucked.** With "Spruce", the pine
  on the same plots is left out; "Spruce and pine" bucks both.
- **Trees need DBH ≥ 5 cm and height ≥ 4.5 m** (`NFI_MIN_DBH_CM`,
  `NFI_MIN_HEIGHT_M`; height = `beregnet_hoyde` / 10), the taperNOR
  validity filter. It is applied after plot selection, so a plot stays
  counted even if all its trees fail it.
- The app uses all eligible plots (`all_plots = TRUE`); scripts and audits
  can sample `n_plots` complete plots instead (seed 42).
- **Altitude (`hoh`, m a.s.l.)** is a plot attribute (one value per plot,
  integer, recorded to 5 m), already in the sanitized data; no Oracle
  extraction or preparation script was changed for it. The sidebar slider
  covers 0–1,100 m (`HOH_RANGE` in `inst/app/setup.R`: all HKL 3–5 plots, 0–1,080
  m, rounded out to the 50 m step). At the full range `build_run_params()`
  sends no bounds (`selected_hoh()`), so the params are identical to those
  without the filter; a narrowed range sends inclusive `hoh_min`/`hoh_max`,
  and `select_nfi_trees()` then also excludes plots with missing altitude.
  No plot has missing altitude (0 of 6,348 sanitized plots, 0 of 10,495 in
  the raw extract; script 00 would drop them), so that rule is defensive.

Default stratum (HKL 4, bonitet 11/14/17, all counties):

| Species group | Plots | Trees before the DBH/height filter | Trees bucked |
|---|---|---|---|
| Spruce | 655 | 12,508 | 12,427 |
| Pine | 477 | 4,197 | 4,166 |
| Spruce and pine | 801 | 16,705 | 16,593 (12,427 spruce, 4,166 pine) |

655 plots have spruce, 477 have pine and 331 have both, so "Spruce and
pine" adds 146 pine-only plots to the spruce plots.

Altitude of the default stratum's plots (m a.s.l.; 2026-10-20):

| Species group | Min | 5% | 25% | Median | 75% | 95% | Max |
|---|---|---|---|---|---|---|---|
| Spruce (655 plots) | 10 | 54 | 188 | 320 | 473 | 680 | 910 |
| Spruce and pine (801 plots) | 5 | 50 | 170 | 305 | 465 | 690 | 910 |

Spruce plots per 100 m band from 0 m: 63, 109, 117, 128, 103, 68, 38, 16,
11, 2. The band 200–400 m keeps 301 of the 801 "Spruce and pine" plots
(6,571 trees; audit 17 check 4f).

---

## Files that still say "nfiBuck"

These keep the old name on purpose:

| What | Why |
|------|-----|
| `scripts/prototype_reference/`, `audits/prototype_reference/` | Frozen (audit 14 checks 19–20) |
| `inst/extdata/nfi/sanitization_manifest.txt` | Record of the sanitization run; regenerating it rewrites the committed data |
| `outputs/helper_validation/helper_price_matrix_validation_summary.txt` | Written by frozen prototype script 14 |
| `logs/_archive/`, prototype output folders, the taper cache manifest | Records of past runs |
| `docs/plans/phase0.md`, `docs/plans/PLAN.md` | Historical plan; "formerly nfiBuck" |
| `scripts/run_prototype_00_prepare_nfi_data.R` | Describe the old name and the old root that the wrapper replaces |
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
- **A and B.** B held its own prices for every product and a state per product
  (`same`, `own`, `none`; replaced in Phase 4d by two independent scenarios and
  `scenarios_differ()`). With the
  default table and 2021 prices, A runs through the default price file and
  products, as before; audit 23 check 10 shows the explicit path gives the same.
- **Fixed sampling.** NFI runs used 200 plots and seed 42 (`inst/app/setup.R`);
  replaced in Phase 4d by all plots (below).
- **Default diameter limits** (optBuck's example.hpr, under bark): spruce sawlog
  max top 60 / butt 65 cm, pine sawlog 50 / 60, pulpwood 70 / 70. The baselines
  predate them, so audits 20 (check 8), 22 (check 5) and
  `scripts/performance_check.R` run with `pre_phase4c_limits()`. What the new
  limits change is in `outputs/performance/phase4c_limits_change.txt`
  (`scripts/phase4c_limits_change_report.R`): only the NFI spruce stratum, 6 of
  339 trees and 22 logs; value 27 268.08 -> 27 150.33 NOK, commercial volume
  70.30 -> 69.90 m³, NOK/m³ 387.90 -> 388.41 (value falls less than the
  commercial volume, so NOK/m³ rises slightly).
- `inst/extdata/inputs/ProductData_spruce_pine_default.rds` is a stale prototype artifact
  (no PriceType column; read only by frozen prototype script 03) and was not
  updated.

## Phase 4d: scenarios and polish

- **Scenarios.** `inst/app/server_scenarios.R` (`scenario_server()`) holds two
  independent scenarios, `sc$A` and `sc$B` (assortment table and prices
  each). An earlier draft linked B to A per assortment; it was replaced
  before merging. Now B starts as a copy, and "Make assortment/matrix equal to
  A/B" copy one item from the other side (a whole scenario: download and
  upload) (`copy_assortment()`,
  `copy_prices()`). The helpers are in `R/helpers_price_matrix.R`, and audit
  24 tests them without Shiny. The backend needed no change: `.prepare_runs()`
  already gives a key defined differently in A and B (other rules or price
  type) its own candidate pass.
- **Hidden type.** `Type` stays in `ASSORTMENT_TABLE_COLS` and in the scenario
  xlsx, but it is never shown. Edits keep a row's type. New rows get
  `custom_type()`. `validate_products()` no longer reserves "waste" for the
  base keys, so a renamed base waste row is valid. The Phase 4b format keeps
  that rule in `validate_assortment_defs()` (audit 20 check 10).
- **All plots.** `build_run_params()` sends `all_plots = TRUE` (when
  `OPTBUCK_N_PLOTS` is unset), and `prepare_sampled_job()` then takes every
  eligible plot without RNG. The old path (`n_plots`, default 10, capped at 50,
  seed 42) is unchanged, so the baselines and audits 16, 20 and 22 are
  unaffected. The app used to ask for 200 plots, but the cap meant it got 50.
  A typical stratum (spruce, HKL 4, bonitet 11/14/17, all counties) has about
  800 plots and 12,500 trees.
- **Large tables.** reactable 0.4.5 sends every row to the browser and runs R
  `details` functions for every row when it renders. The old Trees table did
  that for every tree, which made the Per m³ tab slow with many trees.
  `paged_table_server()` now sends one page at a time and sorts on the server.
  A tree's logs render only when the tree is selected. With all plots of the
  typical stratum (26,857 logs, comparison) every results tab renders in about
  1 s.
- **Run time.** NFI runs never call taperNOR: all 93,607 bucked trees of
  HKL 3–5 have a cached profile, and a missing one stops the run. Default-
  stratum comparison (12,427 trees, 15 workers): loading 5.8 s, bucking
  26.9 s, result tables 1.3 s, and a 2.5 s worker start once per app
  process. The app's earlier ~2 min was mostly `compare_cutting_patterns()`
  (61.7 s: a per-tree loop with a quadratic lookup). It is vectorised now
  (0.8 s, identical output on all 12,427 trees).
- **Progress.** `www/optbuck.js` draws an indeterminate bar on click. The server
  then reports stages: loading NFI data and taper profiles (indeterminate),
  taperNOR chunks (tree lists, 0–40 %), bucking, and summarising.

## Import from .hpr (hpr Phase A, 2026-10-20)

- **Parser.** `R/helpers_hpr.R`, on xml2, ported from optBuck's getters
  (read, not vendored). `read_hpr()` reads the species groups and classified
  products (limits, classes, volume rules, permitted grades, price cells
  with their BuckingCriteria) from the part of the file before the first
  stem record (`.hpr_prefix()`, 4 MB chunks, fast fixed search for
  `<StemKey>`; `.hpr_close_tags()` closes the open elements), or from the
  whole file when that part lacks species groups or products. .gz via
  gzfile(), .zip streamed with unz(). Stems are not read (Phase B). It never reads
  coordinates, dates, operators, object names or contacts.
- **Mapping.** `hpr_to_scenario()` builds an assortment table and prices
  (one matrix-priced row per product and species, plus the base Waste row
  per species) and checks them with `check_price_set()`.
  `hpr_product_table()` gives the dialog's product rows and default ticks.
  The decisions (rounding up, forbidden cells, class-limit price length,
  species guess, best-grade filter) are recorded in CLAUDE.md.
- **Price length.** The hidden assortment field `PriceLengths_cm` holds the
  class lower limits of a product priced on "Length as defined in
  LengthClasses"; `products_from_defs()` turns it into
  `LengthClasses$PriceLength_cm` only when some row has it (the base tables
  are unchanged), the candidate plan carries it (`price_len_of()`), and
  `.prepare_runs()` treats a different price length as a different product
  definition.
- **Test file.** The real example has exact stem coordinates and names, so
  it is not in the repository. `inst/extdata/inputs/test_fixtures/hpr_example_synthetic.hpr`
  holds its species groups (with DBHHeight since Phase B; FURU set to 120),
  products and prices plus 13 invented stems
  (`scripts/make_hpr_test_fixture.R`, run with the path to optBuck's
  `inst/extdata/example.hpr`).
- **Example file contents** (optBuck @ 33ec887): species groups 445 FURU,
  446 GRAN, 447 LAUV, 448 BIOVIRKE (grades 1–16, best 1, StartGrade 2);
  8 classified products (240 Sagtømmer furu, 200 Massevirke for FURU;
  197 Massevirke, 100 Energi, 154 Kubb, 140 Sagtømmer gran for GRAN;
  345 Bjørk; 987 Biovirke), all under bark with the top diameter 10 cm below
  the cut; 35 forbidden cells in 240 Sagtømmer furu; 2 GRAN stems over bark
  every 10 cm from the felling cut (25.0 and 22.2 m measured).

## .hpr stems (hpr Phase B, 2026-10-21)

- **Reader.** `read_hpr_stems()` in `R/helpers_hpr_stems.R` streams 2 MB
  chunks (`.hpr_stem_chunks()`): a stem record runs from one
  `<Stem…><StemKey>` to the next, the last (incomplete) one carries over.
  Each chunk's records go through `.hs_extract_regex()`, or
  `.hs_extract_xml2()` when the chunk has `&` or CDATA. Work on bytes
  (`Encoding = "bytes"`, `useBytes = TRUE`): R's char/byte offset conversion
  is quadratic on long non-ASCII strings, and `substring()`'s default end is
  1,000,000 characters (a bug lost one stem per chunk before it was fixed).
  The definitions come from `read_hpr()` (which now also returns
  `dbh_height` per species group).
- **Checks and profiles.** `hpr_stem_checks()` decides which stems are
  bucked (one warning per drop reason) without building profiles, so the app
  can re-check as the species map changes; `hpr_stem_profiles()` builds the
  10 cm profiles and DBH (at 1.3 m above ground via
  `hpr_height_above_ground()`), in the long layout of
  `profiles_to_profile_vectors()`.
- **Cleaning (Phase 3, 2026-10-28).** After the file-problem drops,
  `hpr_stem_checks()` calls the pure `hpr_clean_flags()` for the remaining
  stems: broken top (last diameter > 200 mm, counted only: the stem is
  kept), too flat (a 30 dm window
  falls < 1 mm) and too steep (> 30 mm over 1 dm or > 60 mm over 10 dm),
  from 200 cm above the felling cut to the first measured diameter
  ≤ 100 mm, on the stem's own measured points. A too flat or too steep
  stem is excluded (once, under its first failing check); each check has
  one warning line;
  counts are independent per check and summed over files
  (`clean_counts`). Rules and thresholds: CLAUDE.md, "hpr cleaning"; test:
  audit 26 check 14.
- **Stem grades (Phase 6, 2026-10-28).** `read_hpr_stems()` also reads
  `StemGrade/GradeValue@gradeStartPosition` (`gidx`, `gpos`, `gval`, also
  through `combine_hpr_stems()`). `hpr_stem_profiles(grades = TRUE)` puts the
  grade of each 10 cm cell on the profile (`hpr_grade_grid()`; StartGrade for
  stems and cells without a grade), and `profiles_to_profile_vectors()` carries
  it as `pv$grade`. The two per-scenario options (`GRADE_OPTS_OFF`,
  `normalize_grade_opts()` in `R/helpers_candidate_generation.R`) become
  candidate filters in `.candidate_columns()`; the hidden assortment column
  `PermittedGrades` and `ReferenceGrades` come from the .hpr import (a product that does not
  permit its group's reference grade is left out for stems without grade data). Rules and decisions: CLAUDE.md,
  "hpr stem grades"; tests: audit 30 (fixture
  `inst/extdata/inputs/test_fixtures/hpr_grades_synthetic.hpr`, rebuilt by
  `scripts/make_hpr_grades_fixture.R`) and audit 17 check 41.
- **Backend.** `prepare_hpr_job()` / `run_hpr_bucking()` (stump forced to
  0), `prepare_hpr_stems_job()` for the app's worker; `.result_tree_list()`
  with `kind = "hpr"`.
- **App.** Third radio choice; `hs_*` outputs; reading on a worker as an
  ExtendedTask; the imported file is kept in the session's `imported/`
  folder, uploads in `upload/`.
- **Owner's files** (local, `C:/optbuck_private/hpr/`; 2026-10-21, 15 workers):

  | File | Stems (about) | Read | A/B run |
  |---|---|---|---|
  | 46 MB | 7,000, none with a profile (an error) | 5.5 s | – |
  | 52 MB | 3,000 | 6.1 s | 9.8 s |
  | 124 MB | 7,500 | 12.5 s | 18.7 s (app, click to results: 22 s) |
  | 142 MB | 11,650 | 15.9 s | 23.4 s |
  | 151 MB | 7,500 | 14.9 s | 20.9 s |

  Reading adds about 100 MB to the process; the 15 workers peak at about
  2.3 GB together. No cap on the number of stems.
