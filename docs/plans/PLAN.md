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

This requires under-bark diameters. Currently everything is over-bark (`with_bark = TRUE`, `DiameterUnderBark = FALSE`). See open questions.

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

Replace the current price-scenario options (base / multiplier / uploaded). Remove the multiplier if it is not needed by the new design.

- **Comparison view.** For each species × assortment, show the default matrix (read-only) and the scenario matrix side by side. The scenario matrix can be edited in place (`rhandsontable`).
  - Cells that differ from the default are highlighted.
  - "Reset to default" works per matrix.
  - Drag-and-drop upload replaces the scenario matrix. Uploads are validated against the `PriceMatrix_long` schema, and an import template can be downloaded.
  - Scenarios can be downloaded as CSV.
- **Results.** Show default vs. scenario: NOK/m³ total and by assortment, volume shares by assortment, and the change in cutting patterns. Example stems show the bucking under both matrices.
- **Custom assortments.** Users can add assortments (e.g. poles, large-diameter sawlogs, small sawlogs, special timber) with:
  - Species
  - Minimum and maximum top diameter
  - Allowed lengths
  - Quality restrictions, if supported
  - A price matrix

  These are added to the product definitions and validated with `validate_price_matrix_against_products()`.
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

1. **Bark (decided 2026-10-12).** All stem profiles are assumed over bark: taperNOR output, uploaded tree lists, and imported stem profiles (e.g. from harvester data). Under-bark diameters are derived with bark functions, which Lennart will supply from published sources. Implement bark functions as a separate, swappable helper. Open: should price lookup use top diameter under bark?
2. **Bark function source.** Lennart to provide the bark functions and references.
3. **Height imputation.** References and code for the Norwegian height models, and the function from Lennart's package for partial heights.
4. **Sensitivity tool.** Is the price sensitivity tool in phase 4 wanted? If a matrix optimiser is wanted instead, what should it optimise towards?
