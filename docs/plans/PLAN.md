# Apter (formerly nfiBuck) development plan

Plan dated 2026-10-12. Work through one phase per session. Start each phase in plan mode, show the plan to Lennart before editing, and re-run audits 14, 15, 16 (and new audits) at the end of each phase. Do not edit `scripts/prototype_reference/` or `audits/prototype_reference/` (audit 14 checks 19–20).

## Scope decisions (2026-10-12)

- Apter is a tool for improving bucking and price matrices. It is not an inventory or estimation tool.
- Remove everything related to stratum estimation, per-ha values, plot scaling, ha-expansion and NFI expansion factors. This was never in scope. Remove it from the code, UI, README and CLAUDE.md, including the "Recommended next steps" section of the README.
- NFI trees remain one possible tree source: a sample of trees selected by species, hogstkl, abonitet and fylnr. Results describe the selected trees only.
- Species: Norway spruce and Scots pine only.
- Audit 14 REVIEW item (zero-priced waste) is accepted as intentional. Record this in the audit output and documentation.
- Non-monotonic stem profiles are valid input and must not trigger warnings or errors.
- Deployment target: NIBIO server with many cores. Testing happens on the remote desktop, which also has many cores. Per-tree optimisation must run in parallel.

## Value denominator (NOK/m³)

All values are reported per m³ of commercial solid volume under bark:

- **Commercial stem:** the parts of the stem allocated to non-waste products. This excludes bark, the top above the last merchantable log, stump, and removed waste sections.
- **Volume:** the sum of 10 cm cylinders along the commercial stem. Each cylinder uses the under-bark diameter at the top of its 10 cm section.
- The denominator is independent of how individual products are priced or measured. For example, a sawlog volume based on its top diameter does not change the denominator.
- Report NOK/m³ in total and by assortment (assortment value divided by the same total commercial volume), plus the volume share by assortment.

Implemented in Phase 3 (2026-10-12): under-bark diameters from `taperNOR::barkNOR()` (`R/helpers_bark.R`), the denominator in `R/helpers_commercial_volume.R`. Each species uses its own denominator.

## Phase 0: housekeeping

1. List all TODO/FIXME/XXX comments and unfinished items in the repo. Report the status of `scripts/app_backend/02_benchmark_dp_optimizer_alternatives.R`: whether it was concluded, and which optimizer variant the app uses. Present the list to Lennart for triage. Do not act on it.
2. Replace the hardcoded root-path fallback in `app/app.R`. Use `NFIBUCK_ROOT` → `APTER_ROOT`, otherwise the app directory's parent.
3. Split `app/app.R` into sourced files: `app/ui.R`, `app/server.R`, plus helpers for upload and validation. Keep `shiny::runApp("app")` working. Update the CLAUDE.md architecture note.
4. Add a headless app smoke test (`shinytest2`) as a new audit.

## Phase 1: rename to Apter

- Rename the repo, R files, environment variables, app title, README, CLAUDE.md, audit labels and output folder names wherever they say nfiBuck.
- Exceptions: frozen prototype scripts and audits keep their content. Old output files that audits compare against may keep their names if renaming would break regression checks. List any such exceptions.
- Give Lennart a short checklist for his local PC: renaming the GitHub repo, `git remote set-url`, local folder name, `.Renviron` variable names, and Oracle-dependent scripts.

## Phase 2: appearance

Model the look on NIBIO's ForestSens (Oracle APEX Redwood style). Screenshots are kept in `docs/design_reference/`; ask Lennart to add them.

- Use `bslib` (Bootstrap 5) with a custom theme. Use no default Shiny styling.
- Warm off-white page background, white cards with subtle shadow and rounded corners, and a slim light-grey top bar with the app name on the left.
- A home/landing view with an app icon, the "Apter" title and a decorative banner strip.
- Card headers with plain dark text. Read-only fields shown as labelled boxes with dashed borders, small grey labels and larger values.
- Clean tables with light row separators and pagination text at the bottom right, like "1 - 2".
- Run status displayed as step lists with check icons, DONE status and timestamps.
- Font: a free substitute such as Inter, since Oracle Sans is not freely licensed.
- Restyle selectize inputs, buttons, file inputs and tabs explicitly. Avoid sliders where numeric inputs or selects work.
- Footer showing the release version.

## Phase 3: per m³ outputs

- Implement under-bark diameters (see open questions) and the commercial-volume denominator defined above, as a tested helper function.
- Replace all per-ha and plot-level outputs with NOK/m³ outputs: total, by assortment, and by species. Show tree-level detail on demand.
- Add audit checks:
  - Manual verification of the denominator on a few synthetic stems.
  - Waste and top sections excluded from the denominator.
  - Denominator unchanged when prices change.

## Phase 4: price matrices and assortments

Split into two parts (decided 2026-10-12): 4a first, 4b later.

## Phase 4a: matrix comparison

Replace the current price-scenario options (base / multiplier / uploaded). The multiplier inputs are removed; the side-by-side comparison replaces them.

1. **Deterministic tie-breaking in the optimizer (first).** Treat totals within a tolerance as equal and apply a documented rule, such as preferring fewer cuts. The fast and reference optimizers must stay identical (audit 16 checks 14–15). Phase 3's audit 18 found 34 of 137 spruce trees switching between equal-value patterns when all prices were scaled ×1.5 (e.g. different splits of the same flat-priced pulpwood stretch; exact ties since pulpwood volume is the unrounded solid-volume rule).
2. **Side-by-side price matrix comparison.** For each species × assortment, show the default matrix (read-only) and the scenario matrix side by side. The scenario matrix can be edited in place (`rhandsontable`).
   - Cells that differ from the default are highlighted.
   - "Reset to default" works per matrix.
   - Drag-and-drop upload replaces the scenario matrix. Uploads are validated against the `PriceMatrix_long` schema, and an import template can be downloaded.
   - Scenarios can be downloaded as CSV.
3. **Default vs. scenario results.** NOK/m³ total and by assortment, volume shares by assortment, and the change in cutting patterns. Example stems show the bucking under both matrices.
   - **Cutting-pattern changes count only differences that change value or volume**, so equal-value ties are not shown as changes.

## Phase 4b: assortments and volume methods (later)

- **Custom assortments.** Users can add assortments (e.g. poles, large-diameter sawlogs, small sawlogs, special timber) with:
  - Species
  - Minimum and maximum top diameter
  - Allowed lengths
  - Quality restrictions, if supported
  - A price matrix

  These are added to the product definitions and validated with `validate_price_matrix_against_products()`.
- **Volume calculation method per product.** Make the price-volume method selectable per product, following the StanForD volume categories as implemented in optBuck (`PriceVolumeCalc()`): e.g. top cylinder (current default for sawlogs), frustum, all diameters (solid volume), calculated Norwegian mid, with the matching volume-diameter and volume-length rules. **Frustum taper is 1 cm per metre** (decided 2026-10-12). optBuck's `PriceVolumeCalc.R` currently uses `LogLength * 0.01`, which is a bug (should be `0.1`, since diameters are in mm and LogLength in cm); Lennart is fixing it in optBuck. Use 1 cm/m here regardless. The commercial-volume denominator does not depend on this choice.
  - **Reconcile "solid volume".** Apter's solid volume (pulpwood/waste price volume and the denominator) is 10 cm cylinders at each section's under-bark **top** diameter, not rounded, via `section_volume_fun()` (decided 2026-10-12). optBuck's "All diameters (solid volume)" is a different variant: it sums cylinders over every diameter from start to stop inclusive (one section too many), and rounds diameters down to cm before subtracting bark. Reconcile the two when adding per-product volume methods; keep the denominator on Apter's rule.
- **Proposed extension (confirm with Lennart before building).** A sensitivity tool that shows how cutting patterns and NOK/m³ respond to changing a price cell or assortment by ±x %. It would also show which cells actually affect the bucking. Building a full price-matrix optimiser would need an explicit objective (e.g. target volume shares per assortment), which Lennart must define first.

## Phase 5: tree list import

- Add a data-source switch with two options: NFI tree sample or uploaded tree list.
- Required columns: `tree_id`, `species` (spruce/pine), `dbh` in cm. Optional: `height` in m, `plot_id`.
- Apply taperNOR, then run the existing candidate and DP pipeline.
- **Height imputation.** If no heights or too few heights are given, use Norwegian height models. If some heights are given, use Lennart's own R package. Lennart will provide references and code. Until then, require heights and leave a clear placeholder function.
- Provide a downloadable import template.

## Phase 6: stem profile import

- Long format: `tree_id`, `species`, `height` in m along the stem, `diameter` in cm.
- Bark status is declared by the user (see open questions).
- Interpolate the profile to the 10 cm DP grid and feed it directly into candidate generation, bypassing taperNOR.
- Allow non-monotonic profiles.
- Provide a downloadable import template.

## Validation layer (shared by phases 4–6)

Validation results are shown in a panel before the user runs the model. Errors block the run; warnings do not.

- **Errors:** missing required columns, non-numeric values, missing IDs, unknown species, negative or zero dimensions, duplicated heights within a tree, a price matrix that does not match the product definitions.
- **Warnings:** likely unit errors (state the suspected unit, e.g. a dbh median above 100 suggests mm, a height median above 60 suggests dm), implausible h/d ratios, profiles with very few points or large height gaps, trees outside the taper model's data range, and price matrices with gaps or implausible values. Do not convert units silently.
- Add an audit with deliberately broken test files for each check.

## Performance

- Parallelise the per-tree optimisation (`future` / `future.apply` or `mirai`, with a configurable number of workers).
- Run long jobs with `shiny::ExtendedTask`, so the UI stays responsive and shows progress.
- Benchmark on the remote desktop and record timings.

## Documentation

After each phase, update the README and CLAUDE.md, including the accepted assumptions, out-of-scope items and data formats.

## Open questions for Lennart

1. **Bark (decided 2026-10-12, implemented in Phase 3).** All stem profiles are assumed over bark: taperNOR output, uploaded tree lists, and imported stem profiles (e.g. from harvester data). Default bark model: `taperNOR::barkNOR()` along the whole stem, as a swappable helper (`R/helpers_bark.R`). Price lookup uses the top diameter under bark; product limits and price volume are under bark too (`DiameterUnderBark = TRUE`, as optBuck's `buckStem`).
2. **barkNOR negative pine bark (open; ask the taperNOR authors).** barkNOR's pine coefficients give negative double bark near the top of larger pines (d below about 0.33·dbh − 2.4 cm). Apter floors B at 0 as a provisional fix. Ask the authors whether this is intended or whether the 2023 paper's coefficients differ.
3. **Height imputation.** References and code for the Norwegian height models, and the function from Lennart's package for partial heights.
4. **Sensitivity tool.** Is the price sensitivity tool in phase 4b wanted? If a matrix optimiser is wanted instead, what should it optimise towards?
