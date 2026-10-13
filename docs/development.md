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
| Tie-breaking and price scenarios | Audit 19 PASS (25/25): synthetic ties, order independence, matrix helpers, long and wide uploads, pattern comparison |
| Shiny app (v1, Release 0.2.0) | Audit 17 PASS (24/24), headless shinytest2 smoke test with screenshots |

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
  setup.R                   Data loading, HKL/bonitet/county choices, label, APP_VERSION
  theme.R                   Design tokens, chart colours, bslib (Bootstrap 5) theme
  helpers_ui.R              Cards, read-only fields, status tiles, step list,
                            reactable tables, base-R plot styling
  helpers_validation.R      Inputs -> backend params; stratum plot counts
  helpers_upload.R          Writes the scenario price matrix for the comparison run
  app_ui.R                  `ui`: page_navbar with Home, Simulator, Prices and assortments
                            (not ui.R: Shiny treats ui.R + app.R as two app formats)
  app_server.R              `server`
  www/
    apter.css               Component styles (ForestSens / Redwood look)
    apter-icon.svg          App icon and favicon (original artwork)
    fonts/                  Inter and Source Serif 4 (.woff2, SIL OFL 1.1; licences alongside)

R/                          Core engine helpers
  helpers_paths.R           Project root and path utilities (get_root_dir())
  helpers_products.R        Product definitions and assumption checks
  helpers_valuation.R       Price matrix loading, validation, diameter-class capping
  helpers_audit.R           Audit logging framework
  helpers_taper.R           Taper profiles (wraps taperNOR)
  helpers_candidate_generation.R  Candidate log geometry, eligibility and volume
  helpers_dp_optimizer.R    Reference dynamic-programming bucking optimizer
  helpers_bark.R            Swappable bark model (default taperNOR::barkNOR, B >= 0 floor)
  helpers_commercial_volume.R  NOK/m3 denominator and per-m3 summaries
  helpers_price_matrix.R    Price-matrix scenarios: grids, diff, reset, upload (long/wide), templates
  helpers_comparison.R      Default vs. scenario: pattern changes, NOK/m3 comparison

audits/
  audit_14_validate_first_helpers.R          Products, valuation, price-matrix validation
  audit_15_core_engine_helpers_regression.R  Regression of all engine helpers
  audit_16_sampled_plot_backend_smoke.R      Backend smoke + fast/reference optimizer equivalence
  audit_17_app_smoke.R                       Headless app smoke test (shinytest2)
  audit_18_bark_and_commercial_volume.R      Bark model and commercial-volume denominator
  audit_19_ties_and_scenarios.R              Tie-breaking, price-matrix scenarios, pattern comparison
  prototype_reference/      Frozen audits 05–12

scripts/
  97_fill_31dm_sawlog_price_gap.R         Fills the 31 dm sawlog price gap by interpolation
  98_create_github_safe_processed_data.R  Sanitizes data_processed/ into data_processed_github/
  99_cleanup_project_structure.R          One-off post-prototype cleanup (already run)
  run_prototype_00_prepare_nfi_data.R     Runs the frozen Oracle extraction under the current root
  app_backend/
    00_precompute_taper_profiles_github.R     Precomputes the taper profile cache for the app
    01_run_sampled_plot_bucking.R             Backend used by the app (fast DP optimizer)
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
  app_cache/                Taper profile cache (local, regenerated by app_backend/00)
  stratum_availability/     Species/hogstkl/abonitet/fylnr combinations with enough plots

VERSION                     Release number shown in the app footer (bump by hand)

docs/
  development.md            This file
  plans/                    Roadmap (PLAN.md) and phase plans
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

The look follows NIBIO's ForestSens (Oracle APEX Redwood style); reference
screenshots are in `docs/design_reference/`. Everything is our own: colours
are the tokens in `app/theme.R` (`APTER_TOKENS`, `APTER_CHART`), component
styles are in `app/www/apter.css`, and building blocks are in
`app/helpers_ui.R`. No inline colours in `app_ui.R` / `app_server.R`, and no
Oracle or ForestSens assets.

- Fonts are self-hosted from `app/www/fonts/` (no Google Fonts request at
  runtime): Inter for the UI, Source Serif 4 for the landing title only.
  Both are SIL OFL 1.1; keep the licence files next to the font files.
- Chart colours were checked with a colour-vision-deficiency validator
  against the white card surface. Single-series charts use one colour;
  Scenario 1/2 use blue/orange; the delta chart uses a neutral colour
  because the sign is shown by bar direction.
- Audit 17 saves screenshots of every page to
  `outputs/app_smoke/screenshots/` for review against the references.

## Running the audits

Run from the project root, or set `APTER_ROOT`. Each audit writes a CSV and a
text log under `outputs/` and ends with a verdict: PASS, ACCEPTED WITH
REVIEW, or FAIL. Re-run audits 14–17 at the end of every development phase.

**Prerequisites**

```r
install.packages(c("arrow", "readr", "dplyr", "shiny", "bslib", "reactable",
                   "rhandsontable", "readxl", "writexl"))
devtools::install_git("https://github.com/SmartForest-no/taperNOR")
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
| 17 — app smoke | `Rscript audits/audit_17_app_smoke.R` | shinytest2, chromote, Chrome; use `Rscript`, not `source()` |
| 18 — bark and commercial volume | `Rscript audits/audit_18_bark_and_commercial_volume.R` | taperNOR; taper profile cache for its backend runs |
| 19 — ties and scenarios | `Rscript audits/audit_19_ties_and_scenarios.R` | readxl, writexl; taper profile cache |

Audit 16 checks 14–15 compare the app's `.optimize_dp_fast()` against the
reference `optimize_one_tree_dp()` on every tree of three backend runs.

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
