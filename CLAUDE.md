# nfiBuck — context for Claude and other AI assistants

This file is for AI collaborators. For a human-facing overview (what the
project does, how to run it, current status), see [README.md](README.md).

## What this is

`nfiBuck` simulates optimal log bucking (cross-cutting) on trees from the
Norwegian National Forest Inventory (NFI): given a tree's taper profile
(`taperNOR`) and a set of product/price definitions, a backward-induction DP
optimizer finds the cutting pattern that maximizes total timber value. A
Shiny app (`app/app.R`) exposes a sampled-plot version of this pipeline.

## Domain terms (Norwegian NFI)

- **hogstkl** — maturity/harvest class (1–5; the app only offers 3–5,
  production/mature forest — 1–2 are young/establishment, excluded by design)
- **abonitet** — site productivity class (bonitet; lower number = poorer site)
- **fylnr** — county code. Norway's counties were reorganized in 2024; the
  app's `FYLNR_CHOICES`/`.FYLNR_NAMES` mapping in `app/app.R` handles both
  current and historical codes.
- **hoh** — elevation (m)
- **bucking** — cutting a stem into logs (sawlog/pulpwood/waste) to maximize
  value; **taper** — the stem's diameter-vs-height profile

## Accepted assumptions (load-bearing, not guesses)

| Parameter | Value | Where |
|-----------|-------|-------|
| Stump height | 10 cm | first cut position above terrain |
| DP grid step | 10 cm | position resolution in `helpers_dp_optimizer.R` |
| Taper function | `taperNOR::taperNOR()`, over-bark (`with_bark = TRUE`) | `helpers_taper.R` |
| `DiameterUnderBark` | `FALSE` | all pricing uses over-bark diameter |
| Max price-diameter class | 50 cm | `DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM` in `helpers_valuation.R`; logs above this are capped for price lookup only — physical diameter is never changed |
| Waste price | 0 NOK/m³, intentional (accepted 2026-10-12) | price matrix; audit 14 check 18 |

## Known out-of-scope items (don't assume these are bugs)

- No bark correction (pricing uses over-bark volume/diameter throughout)
- No HPR/StanForD import — all trees come from NFI data
- No NFI expansion factors applied — plot-level values are simple unexpanded
  plot-scaling, never treat app output as ha-expanded/representative
- No stand-level routing — optimization is strictly per-stem
- Uploaded price matrices must conform to the `PriceMatrix_long` schema
  validated by `validate_price_matrix_against_products()` in
  `helpers_valuation.R` — no format auto-conversion

## Data privacy — read before touching plot/coordinate data

**No exact plot coordinates exist anywhere in this pipeline or repository**,
not even in the private local `data_processed/` pulled from Oracle (`SKOG.NORGE_FLA_2024_V`
is queried with `SELECT *`, but the extraction script whitelists only a
specific column set that never included coordinates — verified directly
against the raw Oracle columns, not assumed). The only location signal
available is `fylnr` (county, coarse) and `hoh` (elevation). If asked to add
a map or location feature, this is a hard constraint, not a data gap to
"just go find" — see `data_processed_github/sanitization_manifest.txt` for
the full sanitization record (original `plot_id` is replaced with a synthetic
`plot_id_github`, and that mapping is never written to disk).

**Authorised exception (presentation only).**
`scripts/local_only/export_plot_coordinates_rounded.R` queries Oracle for
plot coordinates and exports them rounded to 1 km to
`C:/nfibuck_private/plot_coords_km.csv`, outside the repo. The project owner
authorised it for the 2026-10 status presentation only. It needs Oracle
access, writes nothing into the repository, and its output rows carry no
plot IDs of any kind. It is not used by the app or the pipeline, and it does
not make coordinates available for features: a map or location feature is
still out of bounds.

## Architecture notes

- The app is split into explicitly sourced files under `app/` (no
  `R/` autoload, no `global.R`, no `modules/`): `app.R` (entry point: root
  resolution, sources the backend and the files below, `shinyApp(ui,
  server)`), `helpers_root.R`, `setup.R` (data, choice mappings, label,
  CSS), `helpers_validation.R` (inputs -> backend params, stratum counts),
  `helpers_upload.R` (Scenario 2 price matrix), `app_ui.R` and
  `app_server.R`. Never name them `ui.R`/`server.R`: next to `app.R` Shiny
  treats those as a second app format, and shinytest2 refuses to start.
- **Project root:** no hardcoded paths. Order is `APTER_ROOT` ->
  `NFIBUCK_ROOT` (legacy, until the Phase 1 rename) -> fallback (app: parent
  of `app/`; `get_root_dir()`, backend and audits: walk up from `getwd()` to
  the folder containing `R/helpers_paths.R`) -> `stop()`. The app exports the
  resolved root as `APTER_ROOT` so the backend agrees. Scripts that run
  before `R/helpers_paths.R` is sourced carry an inline copy of this logic.
- The app runs `.optimize_dp_fast()` (backend), not the reference
  `optimize_one_tree_dp()`; audit 16 checks 14-15 assert they agree.
  `audits/audit_17_app_smoke.R` drives the app headlessly (shinytest2).
- The app only reads `data_processed_github/` (sanitized, committed, no
  Oracle needed). `data_processed/` and Oracle access are only needed to
  *regenerate* that sanitized data via `scripts/prototype_reference/00_prepare_nfi_2024_data.R`
  and `scripts/98_create_github_safe_processed_data.R`.
- `scripts/prototype_reference/` and `audits/prototype_reference/` are a
  frozen, matched pair — the original validated prototype scripts (00–15)
  and their audits (05–12). `audit_14`'s checks 19–20 assert these scripts
  still exist and are unmodified; don't delete or edit them.
- See README's "Repository contents" section for the full file layout.
