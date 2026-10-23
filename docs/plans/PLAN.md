# Apter development plan

Updated 2026-10-15. Replaces the previous PLAN.md and supersedes docs/plans/phase0.md (and any other files in docs/plans/). Detailed conventions live in CLAUDE.md; developer details, audits and benchmarks in docs/development.md.

Working rules: one phase per session, plan mode first, show the plan to Lennart before editing, work on a branch off main, re-run all audits at the end of a phase, and don't merge or push until Lennart says so. Never edit `scripts/prototype_reference/` or `audits/prototype_reference/`. When a convention is undecided, follow optBuck (SmartForest-no/optBuck) unless it conflicts with a decision in this file or CLAUDE.md.

## Scope

- Apter is a general bucking simulator for Norway spruce and Scots pine, for industry partners: compare and improve price matrices and plan bucking. It is not an inventory or estimation tool, and NFI is not emphasised.
- Tree sources: NFI data (all plots of a stratum), uploaded tree lists, stems from .hpr files (hpr Phase B) and (Phase 6) uploaded stem profiles. Results describe the selected trees only. No per-ha, plot-level or stratum outputs.
- Assortments and prices: the base set, user-added assortments, scenario and price files, and (hpr Phase A) products and prices imported from .hpr files.
- Values are NOK per m³ of commercial solid volume under bark (definition in CLAUDE.md).
- Deployment: NIBIO Linux server with many cores; testing on Lennart's remote desktop.

## Done

| Phase | Content |
|---|---|
| 0 | Housekeeping: root resolution, app split, smoke audit (17) |
| 1 | Rename nfiBuck → Apter; partner-facing README |
| 2 | Appearance (bslib, reactable, self-hosted fonts) |
| 3 | Under-bark diameters (barkNOR, B ≥ 0 floor), NOK/m³ denominator, shared solid-volume rule |
| 4a | Deterministic tie-breaking; price-matrix editor; comparison with cutting-pattern changes |
| 4b | Custom assortments; per-product volume methods (StanForD, Norwegian mid per Målereglement 2026); per-product price cap |
| 5 | Tree-list import; height imputation (fieldPlot, calibrated/uncalibrated FinnHoyde) |
| Performance | Shared candidate pass, vectorisation, parallel workers, ExtendedTask; `or_default()` |
| 4c | Interface redesign: two tabs, one assortment table, price sets A and B, default diameter limits from optBuck, Forest & sand look |
| 4d | Scenarios A and B with their own assortments and prices; simpler assortments; optBuck name in the app; all NFI plots; paged result tables; demo files |
| Restyle | Option 3 "Combination" look |
| Layout and figures | One flow per scenario card with a shared selection; live tree counts from the run's own selection code; ggplot2 tree and log figures; light grey page behind white cards |
| Altitude filter | Altitude range slider (m a.s.l., 50 m step) as an NFI selection variable |
| hpr Phase A | Import from .hpr per scenario: StanForD 2010 products, price matrices and species groups into Scenario A or B |
| Volume method Mid | StanForD "Mid" price volume (diameter at the middle of the log); imported from .hpr instead of solid volume |
| hpr Phase B | ".hpr stems" as a tree source: the measured stem profiles of an .hpr file, stump 0, streamed reading |
| Result tables | Log tally per price matrix (stocknota) and value by DBH class; duplicate outputs removed |
| Roadmap Phase 1 | Grade rule for .hpr products (reference grades); Copy from A/B; tally caption; comparisons for different assortments (shared assortments, by type) |

## Phase 4d: scenarios and polish (done, 2026-10-15)

Built on branch `scenarios-and-polish` after Phase 4c was merged. Results are identical to main except that NFI runs use all plots, and comparisons can use B's own assortments.

1. **Scenarios A and B** cover assortments as well as prices. The Assortments and prices tab has two columns, Scenario A on the left and Scenario B on the right, which stack on narrow screens. Each column has a compact assortment table (Assortment, Species, Top Ø, Max butt Ø, Length, Volume method) with Add/Edit/Remove, and that scenario's price matrices below it.
   - The scenarios are independent (revised before merging): B starts as an exact copy of A, both sides are always editable, and nothing is greyed or linked. Differences are highlighted on both sides.
   - One copy button per section and side: "Make assortment equal to B/A" (Assortments) and "Make matrix equal to B/A" (Price matrices). A whole scenario is copied by downloading one side and uploading it to the other.
   - An assortment can be added to both scenarios or one, and removed from one scenario or both. Its price type (matrix or flat) is set per scenario. The matrix selectors are independent.
   - No status notes. A comparison runs when A and B differ in assortments or prices. NOK/m³ per scenario uses that scenario's own commercial volume.
   - Each scenario has an xlsx upload and download (assortments plus price sheets), and price matrices load separately as before.
2. **Simpler assortments.** No Type field: the product type stays hidden, base waste rows stay waste when renamed, and user-added assortments are always commercial. No Length step: flat-priced assortments use 10 cm steps, and matrix-priced ones take their lengths from the matrix columns. No Diameter rounding: it is fixed per volume method. The defaults give identical products. The dialog has no blurred backdrop.
3. **Text.** The app shows "optBuck" (title, navbar, browser tab); the code and repository keep the name Apter. "2021" and "(wide or long)" are gone. "NFI sample" became "NFI data". The price reset button is "Reset prices".
4. **Data and speed.**
   - NFI runs use every plot of the stratum. The backend's `all_plots`; before this phase the backend capped runs at 50 plots although the app asked for 200.
   - Above 30,000 trees, a note under Run gives the tree count and an estimated run time.
   - The progress bar shows as soon as Run is clicked and covers loading, taperNOR, bucking and the summary.
   - The Per m³ tab was slow because the Trees table built every tree's log table at render time. Trees and changed trees are now paged on the server, with a CSV download of all rows, and a tree's logs render when the tree is selected.
5. **Results tables.** Columns are sized to their content, and headers are readable and wrap. The Products tab has one Assortment column; after a comparison it shows grouped Scenario A / Scenario B charts. Selected logs has no ProductType and shows Plot, with StartPos and StopPos as they are.
6. **Demo files** in `demo/`, from `scripts/make_demo_files.R`: both scenarios, both price files, and an example tree list.
7. **Audits.** New audit 24 covers scenarios and the demo files. Audits 17 and 23 were updated.
8. **Final polish.** The Summary shows the run once and Scenario A and B side by side after a comparison. Each price section has one copy button ("Make matrix equal to B/A"); the whole-scenario buttons are in Assortments only. Both assortment tables use the same fixed row order (species; sawlog, pulpwood, waste, custom), with no click-sorting and full headers. There is an intro line above the scenarios and the SmartForest logo in the navbar (`app/www/smartforest_logo.jpg`).
9. **Selected logs and Summary (after merging).** Selected logs shows a preview of Scenario A's first 20 logs and one "Download all logs (CSV)" button; after a comparison the CSV holds both scenarios with a Scenario column. The Summary shows each scenario's four values as a 2 × 2 grid. The Trees table (Per m³) has the same preview and CSV (no Status column); selecting a tree still shows its logs.
10. **Run time.** NFI taper profiles always come from the precomputed cache; taperNOR never runs for NFI trees. The ~2 min default-stratum comparison was mostly the per-tree cutting-pattern comparison (62 s). It is vectorised now (0.8 s, identical output); the bucking itself takes about 34 s on 15 workers.

## Restyle (2026-10-16, branch `restyle`)

Appearance only: option 3 "Combination" of `docs/design_reference/optbuck_design_directions.html`. The page and navbar are white with hairline rules, and the striped bar is gone. Tabs are grey with a black active tab, and cards have a border and no shadow. Section labels are uppercase grey, with green primary and grey secondary buttons. Tables have grey-green headers, A/B differences are pale green, and the summary boxes are grey-green. The chart colours were validated (pulpwood and waste shifted slightly to pass). No logic, output IDs or results changed; both logos are unchanged.

## Layout and figures (done, 2026-10-19)

Built on branch `layout-and-figures` and merged into main. Interface work; results are identical to main (`scripts/performance_check.R`: every case matches the main baseline).

1. **Assortments and prices: one flow per scenario column.** The assortment table on top (click a row to select it), then a detail panel for the selected assortment (its rules in one line and Edit; its prices directly below, as the matrix grid with the row/column buttons or one flat price field; Remove, Make assortment equal to B/A, Reset prices, price upload and download for that assortment), then the scenario actions (Add assortment, Reset to defaults, Upload and Download scenario). The selection is shared by A and B; a side without the assortment shows "Not in this scenario" and "Add from A/B". The assortment and matrix dropdowns, the "Flat prices" table and "Make matrix equal to B/A" are gone. Upload scenario also takes a full price file. Differences stay highlighted in the table, the grid and the flat price field.
2. **Live tree counts.** A box under the stratum inputs shows plots, trees per species and a rough run time, updated as the selection changes. It uses the run's own selection code: steps 3-5 of `prepare_sampled_job()` moved unchanged into `select_nfi_trees()` (`R/helpers_tree_selection.R`). Tree lists show the trees in taperNOR's range after upload. The note above 30,000 trees is replaced by this box.
3. **Figures (ggplot2, Inter, white background, light grid).** Trees card: DBH and height density curves per species (spruce #3F6E58, pine #C9A97A; validated). Selected logs: number of logs per length class and commercial volume per top-diameter class, stacked bars by assortment, Scenario A and B as facets after a comparison.
4. **Audits.** Audit 17 drives the new layout by row clicks and has new checks 4e (counts = the run's trees), 25 (selection and detail panel), 26 (live counts) and 27 (figures).
5. **Page.** Light grey page `#F3F4F3` behind the white cards (Scenario A and B, the Simulator sidebar and the results card), white navbar.
6. **Tree selection (recorded, unchanged).** NFI plots are selected by tree species (a plot counts if it has trees of the chosen species in the stratum), not by dominant species; only trees of the chosen species are bucked, with DBH ≥ 5 cm and height ≥ 4.5 m; data preparation keeps only spruce and pine. Default stratum (HKL 4, bonitet 11/14/17, all counties): Spruce 655 plots, 12,427 trees; Pine 477 plots, 4,166 trees; Spruce and pine 801 plots, 16,593 trees. Details in docs/development.md.

## Altitude filter (2026-10-20, branch `altitude-filter`)

1. **Data.** Altitude (`hoh`, m a.s.l.) was already in the sanitized data and in `select_nfi_trees()` (`hoh_min`/`hoh_max`); the app did not use it. No Oracle extraction or preparation script changed. No plot has missing altitude.
2. **Sidebar.** A two-handle range slider "Altitude (m a.s.l.)" after County, 0–1,100 m in 50 m steps, full range by default. It is the app's one slider, an intended exception to the no-sliders rule. The live plot and tree counts follow it.
3. **Filter.** Plot level, inside `select_nfi_trees()`, combined with the other strata; inclusive bounds. At the full range the app sends no bounds, so params and results are identical to before (plots with missing altitude stay in); a narrowed range excludes them. "No plots match this selection." also covers an empty altitude band.
4. **Audit 17.** Check 4f (full range identical to no filter; 200–400 m gives fewer plots and trees, all inside the band, and the run bucks the same trees; synthetic missing altitude) and check 28 (the slider in the browser).

## hpr Phase A: import products and prices from .hpr (done, 2026-10-20, branch `hpr-import`)

Reverses the earlier "Reading StanForD .hpr files is out of scope". Decisions in CLAUDE.md (accepted assumptions "hpr …").

1. **Parser.** `R/helpers_hpr.R` on xml2, ported from optBuck's getters (no dependency): species groups, classified products (limits, length and diameter classes, volume rules, permitted grades, price cells with BuckingCriteria). Reads only the part of the file before the first stem record (whole file only if that part lacks species groups or products); accepts .hpr, .hpr.gz and an .hpr inside a .zip (streamed); uploads up to 200 MB (configurable). Stems are not read. Never reads coordinates, dates, operators, object names or contacts.
2. **Import from .hpr** per scenario (Assortments and prices tab): a dialog with the file's validation (errors block, warnings listed), a species table (Spruce / Pine / Not used, guessed from the group name and ID) and a product table (products without the best grade start unticked). "Replace Scenario A/B" replaces that scenario only; then one line ("Imported 5 assortments from …"), with warnings only when something is lost or replaced. The uploaded file is kept for the session (for Phase B).
3. **Mapping.** One matrix-priced assortment per product and species, plus Waste per species; limits from DiameterMINTop / DiameterClassMAX / DiameterMAXButt; length classes rounded up to whole dm; forbidden cells empty; whole-cm diameter rows; a fully forbidden inner diameter class stays as empty rows (never chosen; `pm_drop_empty_cells()` keeps whole empty classes in grids, scenario files and uploads).
4. **Price length.** Products priced on "Length as defined in LengthClasses" use the class lower limit as price-volume length (hidden `PriceLengths_cm`; base tables unchanged).
5. **Assortment tables.** Each scenario lists only its own assortments (the struck-through "Not in A/B" rows, "Not in this scenario" and "Add from A/B" were removed); cells that differ on assortments both scenarios have stay highlighted; assortments only one scenario has are not highlighted.
6. **Audits.** New audit 25 (15 checks, synthetic fixture `inputs/test_fixtures/hpr_example_synthetic.hpr`); audit 17 check 29 (dialog from A and B, broken file blocked, prefilled species and products, Replace Scenario B changes B only).

## Volume method Mid (done, 2026-10-21, branch `volume-mid`)

Closes the open item noted 2026-10-20 (5 of 27 products in one of the owner's real files use it). Decision in CLAUDE.md (accepted assumption "Mid").

1. **Rule.** StanForD 2010 VolumeDiameterCategory 3: V = π/4 · Dmid² · L. Dmid is the under-bark diameter read from the stem profile at the middle of the cut log, (StartPos + StopPos)/2 (interpolated; off the 10 cm grid for e.g. 370 cm logs), rounded down to whole cm. L is the price length (the cut, or the class lower limit for products priced on length classes). Limits and the price class still use the top diameter. optBuck has no Mid.
2. **Code.** `price_volume_m3(..., mid_d_mm)` in `R/helpers_volume_methods.R`; `.candidate_columns()` reads Dmid only for Mid products, so the other methods are untouched (`scripts/performance_check.R`: every case identical to the main baseline).
3. **Import and app.** The .hpr import maps Mid 1:1 (no warning); Mid is in the assortment form's Volume method list and the scenario files.
4. **Audits.** Audit 20 check 15 (by hand on synthetic stems, off-grid midpoint, curved stem, backend path); audit 25 check 16 (an imported Mid product, priced on the class lower limit, chosen in a run) and check 10 now uses an invented unsupported method; audit 17 check 20 adds its custom assortment with Mid.

## hpr Phase B: bucking .hpr stems (done, 2026-10-21, branch `hpr-stems`)

Decisions in CLAUDE.md (accepted assumption "hpr stems").

1. **Source.** ".hpr stems" is the Simulator's third data source, never selected automatically (also not after an import). It reuses the file imported into a scenario this session with its confirmed species map, or takes its own upload (.hpr, .gz, .zip). Panel: the file, a species table (stems per group), and one line ("hpr_example_synthetic.hpr: 13 stems read, 5 used.") with warnings only for problems in the file.
2. **Reading.** `read_hpr_stems()` streams 2 MB chunks of complete stem records. It uses a regex whitelist, with xml2 for chunks with entities or CDATA. It was compared with xml2 on the owner's five files: identical results, 2× faster. The file's DBH, coordinates, dates and operators are never read. Owner's files: 3–16 s per file, +83–115 MB.
3. **Profiles.** Positions from the felling cut, stump 0, only the measured part (the profiles end at DiameterMeasuredEndHeight). Heights above ground = position + (130 − DBHHeight) via `hpr_height_above_ground()`. DBHHeight counts from the cut: the file DBH equals the profile there in 99.6–100 % of stems. DBH = the profile at 1.3 m above ground; under bark with barkNOR. No total height (barkNOR needs none).
4. **Dropped stems.** Multi-tree, no profile, under bark, duplicate key, species Not used (per group), not starting at 0, > 1,500 mm, ending below 1.3 m above ground. Only problems in the file are warnings (under bark, duplicate key, not starting at 0, > 1,500 mm, unreadable values); the expected drops (Not-used species, multi-tree, no profile, ending below 1.3 m) are covered by the summary line (decided 2026-10-21). A file with logs only (one of the owner's) is an error.
5. **Results.** The tree-list tables, `tree_source = "hpr"`, StemKey as tree ID, no plots; the app hides Plot columns and labels the height "Measured length (m)".
6. **Performance.** No cap. The largest file (11,650 stems): A/B run ~23 s (script) / ~22 s in the app on 15 workers, workers ~2.3 GB.
7. **Audits.** New audit 26 (12 checks, fixture extended with 12 invented stems and DBHHeight); audit 17 check 31; `scripts/hpr_stems_benchmark.R` for the owner's files (local only).

Later: compare the recorded logs with the optimal bucking; under-bark profiles; multi-tree stems (they have no profiles in the owner's files).

## Result tables (done, 2026-10-22, branch `result-tables`)

1. **Log tally (stocknota), Selected logs tab.** Logs or commercial volume u.b. per price-diameter class × length class of one assortment at a time, on its price grid ("–" = no price); flat-priced per length; A, B or B − A after a comparison; waste not tallied. xlsx (one sheet per assortment) and CSV downloads. Replaces the logs-per-length and volume-per-diameter figures (its totals).
2. **Value by DBH class, Per m³ tab.** Trees, commercial volume, value and NOK/m³ per species and 5 cm class (open from 60 cm); after a comparison trees and commercial volume once (A's), value and NOK/m³ for A, B and B − A (B's NOK/m³ on B's own volume); CSV; one figure (trees per class, NOK/m³ per class for A and B) replacing the DBH density.
3. **Removed duplicates.** The comparison tab's A/B overview table (= the Summary's A/B grid; trees with logs A / B moved to the Summary) and its A/B value bars (= the Products tab's figure). Figures 11 → 8.
4. **Audits.** New audit 27 (7 checks); audit 17 checks 14–16 and 27.

## Roadmap (agreed 2026-10-22)

1. **Phase 1 (now, branch `grades-and-comparisons`):**
   - A grade rule for .hpr products: which products start unticked as "Only for lower quality grades".
   - "Copy from A" in Scenario B and "Copy from B" in Scenario A: all assortments and prices, after a short confirmation.
   - The caption under the log tally becomes "Price-diameter class (cm) vs length class (dm)".
   - Comparisons for scenarios with different assortments: B − A per assortment (tally, value-change bars) only for assortments both scenarios share; a comparison by assortment type (sawlog, pulpwood, other) when the assortments differ. Totals, NOK/m³, DBH classes and changed patterns stay as they are.
2. **Phase 2: cleaning .hpr stems** (broken tops, plateaus, sudden drops). Skogforsk's Stambas defaults are the starting point:
   - check from 2 m up to a diameter of 10 cm;
   - broken top above 200 mm;
   - minimum diameter fall 1 mm over 30 dm;
   - maximum fall 30 mm over 1 dm and 60 mm over 10 dm.
3. **Phase 3: bulk editing of price matrices.** Selected cells ± or × %; a row price plus length corrections.
4. **Later:**
   - distribution bucking;
   - actual vs optimal bucking (merge with optBuck; .hpr and quality layer);
   - deployment.

## Roadmap Phase 1: grades and comparisons (done, 2026-10-22, branch `grades-and-comparisons`)

1. **Grade rule for .hpr products** (decision in CLAUDE.md, "hpr species and grades"). A report on the owner's five files (species groups, grade numbers and names, StartGrade, permitted grades) compared three rules: lowest grade (old), StartGrade, and most commonly permitted. The rule is now:
   - Reference grades are the grade(s) permitted by the most products of each species group, ties kept. Fallbacks: StartGrade, then the lowest grade.
   - A product without a reference grade starts unticked with "Only for grades set by the operator (special orders)".
   - No exemption by product group or name. A sawlog-group exemption was proposed and rejected: special-order butt logs and poles would take every defect-free stem.
   - In the largest test file, sawlogs restricted to grade 2 are now ticked; energy wood and special orders stay unticked.
2. **Copy from A / Copy from B.** A button in each scenario's actions copies the whole other scenario (assortments and prices) after a confirmation.
3. **Log tally caption.** "Price-diameter class (cm) vs length class (dm)" ("Length class (dm)" when flat-priced).
4. **Comparisons for different assortments.** Runs store both scenarios' assortment keys.
   - Per-assortment B − A is shown only for shared assortments: the tally's Scenario options, the xlsx, the value-change figure and the comparison table.
   - When the scenarios' assortments differ, the comparison tab is by type (Sawlog, Pulpwood, Other; waste left out): NOK/m³ and volume share by type, value change by type, plus a figure of the shared assortments.
   - The Products tab shows "–" for an assortment a scenario does not have.
   - Totals, NOK/m³, DBH classes and changed patterns are unchanged.
5. **Audits.**
   - Audit 25 check 4: reference grades on invented groups (mode, tie, StartGrade, no exemption).
   - Audit 27 checks 8–9: xlsx B − A only for shared assortments; type groups.
   - Audit 17: check 17 rewritten for different assortments; new checks 32 (Copy from A/B) and 33 (B imported from .hpr: by type only).
6. **Fixes before merging.**
   - **Price grid rows.** The grid scrolled inside a 480 px box (about 18 rows) and was redrawn at the top after every change, so added rows fell out of view. It is now drawn at full height. "+ larger" stops at the max top diameter: a class above it was accepted before but can never be used.
   - **Cap line.** A line under each matrix grid states the cap rule ("≥ 36 cm: priced as 36 up to max top 60 cm").
   - **Bar figures with many assortments.** The Products value figure and the value-change figure are horizontal bars sorted by value, so every assortment name shows (17 assortments from one .hpr file). The value change hides unchanged assortments and counts them in a line under the figure. The Products tab keeps only the value figure.
   - **Labels.** Assortment labels use the assortment's name ("Spruce REDACTED25", not "Spruce 140 sca sagt mmer").
   - **Audits.** Audit 23 check 6: no class above the max top. Audit 17 check 11: cap line and full-height grid. Audit 17 check 17: one value figure, no volume figure.

## Phase 6: stem profile import

- Long format (CSV or xlsx): `tree_id`, `species` (spruce/pine), `height` (m along the stem), `diameter` (cm, over bark), optional `plot_id`. Downloadable template. (.hpr stems are hpr Phase B.)
- Profiles are over bark. Under-bark diameters with barkNOR (B ≥ 0 floor); dbh is read from the profile at 1.3 m.
- Interpolate to the 10 cm grid and feed the profiles straight into candidate generation, bypassing taperNOR.
- Non-monotonic profiles are valid: no warnings or errors for local diameter increases.
- Validation: errors for missing columns, non-numeric values, missing IDs, unknown species, non-positive values, duplicated heights within a tree. Warnings for likely unit errors (state the suspected unit; never convert silently), profiles with very few points or large height gaps, and profiles that don't reach 1.3 m.

## Later and open items

- **App overhead on large runs** (open, noted 2026-10-15): the default-stratum comparison (12,427 trees) takes about 57 s from clicking Run to results, of which the backend's own work is about 37 s. Investigate the remaining ~20 s, likely the job's taper profiles serialised between the main process and the workers (prepare runs on a worker, then the chunks are sent out again), plus the worker loading the backend on the first run. The Summary's run time shows the full time from the click.

- **Sensitivity tool** (undecided): show how cutting patterns and NOK/m³ respond to ±x % changes in a price cell or assortment. A price-matrix optimiser would need an explicit objective (e.g. target volume shares), to be defined by Lennart first.
- **Negative pine bark in barkNOR:** the B ≥ 0 floor is provisional; ask the taperNOR authors whether the pine behaviour near the top is intended and whether Hansen et al. (2023) refitted the coefficients.
- **kublin_nor() species parameters (taperNOR 0.1.0, found 2026-10-21):** `kublin_nor()` uses `kublin_par_lme_spruce` for every species (sp 1, 2 and 3), so pine and birch are calibrated with spruce parameters. Ask the taperNOR authors together with the bark question. Apter does not call `kublin_nor()`.
- **optBuck fixes** (Lennart's package, found while porting the .hpr parser, 2026-10-20; Apter's own parser already avoids them):
  - `getProductData()` takes the minimum top diameter from the first `DiameterClassLowerLimit` instead of `DiameterMINTop` (example.hpr REDACTED23: 130 instead of 140 mm).
  - The maximum top diameter is lost: the column named `DiameterClassMAX` holds `DiameterMAXButt`, so `buckStem()` limits the top diameter by the butt limit (600 instead of 500 mm).
  - `BuckingCriteria` is ignored: cells marked "Forbidden to buck the log even manually" stay priced and can be chosen (35 of 126 cells in REDACTED23, several priced 200 as placeholders).
  - `PriceVolumeCalc()` off-by-one: `LengthClass[max(which(LogLength > LengthClass))]` gives a log cut exactly at a class's lower limit the previous class's length (should be `>=`).
  - `buckStem()` uses its own 1 cm/m frustum for every product, whatever the product's volume method, and floors length classes to 10 cm (365 → 360, below the class's lower limit).
- **fieldPlot fixes** (Lennart's package, separate session): loop over species present; error handling around the fitHD() fallback; a working timeout; species curve for plots without heights; nlme:: calls; README install URL. Apter's wrapper works around these meanwhile.
- **Deployment** on the NIBIO server: set apter.workers so that R processes × workers ≤ cores. Memory: a large .hpr stems A/B run (11,650 stems, 142 MB file, 15 workers) peaks at about 2.3 GB across the workers, plus about 0.4 GB in the main process (measured 2026-10-21).
