# Apter development plan

Updated 2026-10-27. Replaces the previous PLAN.md and supersedes docs/plans/phase0.md (and any other files in docs/plans/). Detailed conventions live in CLAUDE.md; developer details, audits and benchmarks in docs/development.md.

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
| Results restructure | Four result tabs (Summary, Products, Trees, Logs); no comparison tab; merged per-assortment table; trees table with A/B and the cutting pattern on click |
| Roadmap Phase 5a | Simulation results exported as .hpr, one file per scenario (stems copied from the source, no coordinates); hidden assortment column HprProductKey |
| Roadmap Phase 5b | Products and prices from .pin files (same import dialog, decided by the root element) and a scenario exported as .pin; "Only logs bucked manually allowed" cells are left empty for .hpr and .pin |

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

## Results restructure (2026-10-23, branch `results-restructure`)

Interface work for the demo; computations unchanged (`scripts/performance_check.R`: every case identical to the main baseline).

1. **Four result tabs: Summary, Products, Trees, Logs.** The Scenario comparison tab is gone: comparison outputs sit in the tab of their content and show only after a comparison. The layout is the same for NFI data, tree lists and .hpr stems.
2. **Summary.** As before, plus "Changed cutting patterns" ("Changed stems" for .hpr stems) after a comparison; every card has a one-line label and value, so the cards are one height.
3. **Products.**
   - One table per assortment (`assortment_summary()`). A single run shows logs, price volume, value, NOK/m³ contribution and volume share. After a comparison it shows value and contribution as A, B and change, and the volume share as A and B; "–" where a scenario lacks the assortment.
   - A CSV download holds every column.
   - Below the table: the type table (NOK/m³ and volume share by sawlog, pulpwood and other) and NOK/m³ by assortment as horizontal bars.
   - After a comparison: the value change (B − A) of the shared assortments, with the "n assortments unchanged" line.
4. **Trees** (replaces Per m³). One figure with the DBH and height distributions side by side, then NOK per m³ by DBH class (lines only, A solid, B dashed), and a table of all trees (pages, sortable; after a comparison value A, B and change). Clicking a tree shows its cutting pattern (A and B, or A alone). The downloads are all trees, value by DBH class and the changed trees.
5. **Logs** (renamed from Selected logs): unchanged.
6. **Removed.**
   - The Difference (B − A) table and the cutting-pattern changes table with its tree picker.
   - The old Per m³ tables, the share figure and the DBH table (its CSV stays).
   - The Trees card and the selected tree's log table.
   - The Products value figure and the type rows.
7. **Import dialog.** The intro text and the file label are gone; the 200 MB limit stays.
8. **Audits.** Audit 27 check 10 (`assortment_summary()`); audit 17 checks 12, 15–17, 21, 24, 31 and 33 for the new structure (31 screenshots), with one-line, equal-height Summary cards in check 16.

## Phase 1: Norwegian language option (2026-10-27, branch `norwegian-language`)

Interface work; computations unchanged. Decisions in CLAUDE.md (accepted assumption "Languages"); how to add a text in docs/development.md ("Languages").

1. **Switch and default.** A Norsk | English switch in the navbar. Norsk (bokmål) is the default. Switching keeps scenarios, uploads and results, with no reload. The browser remembers the choice, and `?lang=en` forces English.
2. **One table.** `inputs/i18n/translations.csv` holds about 540 texts:
   - app texts by id;
   - backend message templates and data values, matched by their English text.

   The backend stays English. The app translates messages when it shows them, so scripts, audits and baselines are unchanged.
3. **Terms.** Lennart's choices: vrak, oppgjørsvolum, sagtømmer, stokkfordeling. The other terms are as proposed (CLAUDE.md).
4. **Formats.**
   - Norsk shows a decimal comma and a space as thousands separator: on screen, in the figures, in the price grid (a typed "430,5" is stored as 430.5) and on the altitude slider.
   - Result downloads are translated: file names, columns and values. Norwegian CSVs use `;`, a decimal comma and a BOM.
   - Exchange files (scenario, prices, tree-list template) keep their machine format in both languages.
5. **Audits.**
   - New audit 28 (7 checks): the table, the ids in the code, no English literals left in `app/`, template round trips, 68 real backend messages without a miss, downloads, tree counts.
   - Audit 17 runs checks 05–33 in English and adds 34–39, a Norsk session: default, switch, both tabs, dialogs, a comparison, downloads, state kept across a switch, a Norsk file loaded in English, the choice kept after a reload. Screenshots are in `outputs/app_smoke/review_language/`.

## Roadmap (agreed 2026-10-27)

Replaces the roadmap of 2026-10-22. The phases come in this order. Each phase gets its own branch off main and plan-mode approval before any editing.

1. **Norwegian language option.**
   - A language switch in the top bar (Norsk/English).
   - Every user-facing text is translated: labels, buttons, messages, figure and table titles, column names, and download file and column names.
   - One translation table; no texts scattered in the code.
   - Lennart reviews the Norwegian forestry terms before the build.
   - Audit 17 tests both languages.
2. **Quick additions (done, 2026-10-28, branch `phase2-quick-additions`).**
   - A log length distribution figure in the Logs tab.
   - Several .hpr files uploaded at once as the tree source, with stem keys made unique per file.
3. **Cleaning of .hpr stems (done, 2026-10-28, branch `phase3-hpr-cleaning`):** broken tops, plateaus and measurement drops. Skogforsk's Stambas defaults; a stem failing a fall check is excluded, a broken top is only counted, one warning per check (CLAUDE.md, "hpr cleaning"):
   - check from 2 m up to a diameter of 10 cm;
   - broken top above 200 mm;
   - minimum diameter fall 1 mm over 30 dm;
   - maximum fall 30 mm over 1 dm and 60 mm over 10 dm.
4. **Bulk editing of price matrices.** Selected cells ± or × %; a row price plus length corrections.
5. **.pin and export.**
   - .pin import.
   - Scenarios A and B exported as .pin.
   - Simulation results exported as .hpr, one file per scenario (done 2026-10-28, Phase 5a; coordinates are never written, machine and operator data are copied from the source on purpose).
6. **Stem quality and forced cuts (tvangskapp) for .hpr stems, as in Timan.** Optional, only for .hpr stems.
   - Stem grades limit which products a log can go to.
   - Recorded forced cuts are kept.
   - Statistics on automatic versus forced cuts.
7. **Distribution bucking (fordelingsaptering).** A new tab after the Simulator.
   - A stand-level LP with a target log distribution, solved by column generation with the existing per-stem optimizer.
   - Outputs: the value under the target, the value loss compared with value bucking, and optimized price matrices from the shadow prices, which can be loaded as Scenario B.
   - Starts with a method note for Lennart's approval.
8. **Merge with optBuck.**
9. **Deployment on the NIBIO server.**
   - Price tuning (the "Price tuning" tab) runs synchronously in the main R process and blocks that process for up to several minutes. On a multi-user server it needs a separate process or a queue.
10. **JOSS paper.**


## Roadmap Phase 6: stem grades and cut at grade changes (done, 2026-10-28, branch `phase6-stem-quality`)

For .hpr stems only, optional, per scenario. Decisions (Lennart, 2026-10-28):

- **Grade semantics.** A stem grade (`StemGrade/GradeValue@gradeStartPosition`) applies from its start position to the next grade change. Starts are snapped to the 10 cm grid and the grades are stored as a step function on the profile grid, carried on the profile vector. A log may take a product only if the product permits every grade the log covers; overlaps under 10 cm at a log end are ignored.
- **Cut at grade changes (tvangskapp).** A forced cut at every grade change (snapped to the grid), implemented as a candidate filter: no candidate log contains a grade change in its interior. `CuttingReason` is ignored. `.dp_select()` and `.optimize_dp_fast()` are not changed.
- **UI.** Two checkboxes per scenario, "Use stem grades" and "Cut at grade changes", shown only when the tree source is .hpr, off by default, copied by Copy A↔B, independent of each other. Norwegian texts in `translations.csv`.
- **Missing data.** A stem without StemGrade gets the species group's StartGrade from position 0. A product without PermittedGradesDefinition, or not imported from .hpr, permits all grades.
- **Product grades.** New hidden assortment column `PermittedGrades` (semicolon-separated, empty = all grades), kept through `defs_from_products()`/`products_from_defs()` and the scenario xlsx. `same_def()` treats differing grade sets or checkbox states as different definitions. No UI editing of grades.
- **Reference grade (changed 2026-10-28).** The import no longer unticks products by the reference-grade rule. Each product row stores its group's reference grade(s) (`ReferenceGrades`); stems without grade data, or with "Use stem grades" off, have that grade in every cell, so a product that does not permit it is left out of their candidates. REDACTED28, all products ticked: off 1,493,116 NOK, "Use stem grades" 1,285,598 NOK.
- **Unchanged.** `CuttingWindowDefinition`, `LimitationCategory` and `DistributionCategory` are left out. With both checkboxes off all outputs and `performance_check.R` equal main.
- **Known limitation.** With several .hpr files, grade numbers are assumed comparable across files.
- **Tests.** Audit 30 on a small synthetic fixture (`inputs/test_fixtures/hpr_grades_synthetic.hpr`) plus one audit 17 smoke step.

## Roadmap Phase 5b: .pin import and export (done, 2026-10-29, branch `phase5b-pin`)

Decisions in CLAUDE.md (".pin import and export").

1. **Import.** The existing dialog ("Import .hpr/.pin") accepts a plain `.pin`; the root element (`ProductInstruction` / `HarvestedProduction`) decides. Species groups are the distinct `SpeciesGroupUserID`s; the key is `ProductUserID`, else `ProductName`, else `_2`, `_3`. Missing `PriceDefinition`: "Top cylinder", no message; missing `DiameterMINTop`: the smallest class. Permitted grades are kept; ReferenceGrades and HprProductKey stay NA; the header and the extension blocks are never read. A .pin never sets `hpr_imported`.
2. **BuckingCriteria (.hpr and .pin).** "Only logs bucked manually allowed" cells are left empty like "Forbidden to buck the log even manually"; "excluded from apportionment bucking" stays priced. REDACTED28 has no such cells (default import 1,493,115 NOK before and after). Two other local files: -1,129 NOK (-0.18 %, 418 of 16,403 logs cut differently, 194 of 6,668 stems) and -79 NOK (-0.02 %, 80 of 7,204 logs, 61 of 2,634 stems); no log kept its cut and changed product.
3. **Export.** One .pin per scenario ("Download as .pin"), waste left out, fixed minimal header without coordinates, `hpr_export_products(style = "pin")` for the product body.
4. **Tests.** Audit 32 (13 checks), audit 17 check 42, audit 25 adjusted (its fixture has manual-only cells). Check 13 (housekeeping, 2026-10-29): files under 512 bytes are read (`.hpr_prefix()` used to lose their bytes).

## Roadmap Phase 5a: .hpr export (done, 2026-10-28, branch `phase5a-hpr-export`)

Export part of roadmap item 5 only (.pin import and export are not built). Decisions in CLAUDE.md ("hpr export"):

1. **What is exported.** The result of an .hpr-stems run as a StanForD 2010 .hpr, one file per scenario, from the Logs tab ("Download as .hpr"; after a comparison "Download Scenario A/B (.hpr)"). Hidden for NFI and tree-list runs.
2. **Source-preserving copy.** Header, Machine, ObjectDefinition and species groups come from the first source file; each bucked stem is copied unchanged (StemGrade, DBH, StemDiameters, ...) with only its Logs replaced by the simulated ones. Stems dropped or cleaned out are left out; StemKey, SpeciesGroupKey and ProductKey stay as written. Machine, operator and object data are copied on purpose (Lennart); coordinates are never written (removed, and the written file is scanned and deleted if any remain). Streamed in 2 MB chunks.
3. **Products.** The scenario's assortments replace the classified ProductDefinitions with limits, classes, volume method and price matrix. A new hidden assortment column `HprProductKey` (next to `PermittedGrades` and `ReferenceGrades`) keeps the original key and metadata; other assortments get new keys above the file's.
4. **Logs.** Positions as cumulative lengths; a gap before or between logs is a log of the unclassified product (999999, added if the source lacks it); Top and Mid diameters and the three volumes from the profile; cutting reason `Automatic`.
5. **Several source files.** Header from the first file; later files' species groups are matched by name; stems per file are reported by position, never by file name. Uploaded .hpr files now stay until the session ends.
6. **Tests.** Audit 31 (10 checks, variants of the synthetic fixture with invented coordinates and private data), audit 17 checks 14 and 31. Real file (148.7 MB, 10,200 bucked stems): export 55 s, 124 MB, process peak 371 MB; re-import gives the same stems and logs, `m3 (price)` within 2.4e-4 m3 of 2,281 m3 (rounding), StemGrade identical (local benchmark, not committed).
7. **Known limitation.** "Top cylinder" is written as "Top" (StanForD has no cylinder rule).

## Roadmap 7, session 3: Price tuning tab (done, 2026-10-30, branch `roadmap7-tab`)

1. **What.** A "Price tuning" tab after the Simulator (see CLAUDE.md, "Price tuning tab"): tune one sawlog product of Scenario A or B on the .hpr stems and use the tuned prices in the other scenario.
2. **Needed first.** The hidden price-matrix column `FileDiameterClass_mm` (the file class lower limit of each whole-cm row), because the app has no file to read the classes from.
3. **Blocking.** The tuning runs synchronously in the main process (deployment, roadmap item 9).
4. **Tests.** Audit 33 (checks 9-12), audit 34 (checks 09-12), audit 17 check 43.

## Roadmap Phase 7a: distribution (target matrix) from .hpr and .pin (done, 2026-10-30, branch `distribution-import`)

1. **What.** The target distribution of imported products is read, kept and written back. It is not used by runs: no change to bucking results, no UI change (`performance_check.R` equals main).
2. **Hidden data.** Price matrix column `Distribution` (percent per cell; whole-cm rows get the value of their file class, as prices do; the column exists only when some cell has a value). Four hidden assortment fields `DistributionAllowed`, `DistributionCategory`, `DistributionWithinProduct`, `MAXDeviation` (the product's LengthDistributionDefinition). NA for everything not from a file and for files without them.
3. **Kept through edits.** `pm_minimal()` keeps the column, `pm_bind()` fills it with NA where one side lacks it, `pm_from_grid()` keeps a cell's value when its price changes, new cells have none, `pm_prices()` gives the four price columns that `write_price_file()` and the backend price loader use (the column never reaches a run). Not compared between A and B (`scenarios_differ()`, `assortment_diff()`).
4. **Export.** `<Distribution>` per cell is the stored value (0 when none). `LengthDistributionDefinition` is written when the assortment has stored fields, after `PriceDefinition`/`PermittedGradesDefinition` and before `ProductMatrixes`, children in the order of the source files (DistributionAllowed, DistributionCategory, MAXDeviation, DistributionWithinProduct).
5. **Tests.** Audit 34 (fixtures `distribution_synthetic.hpr/.pin` from `scripts/make_distribution_fixture.R`; checks 20-28 read the private files and are skipped when missing).
6. **Known limitation.** Distribution, the four distribution fields and `FileDiameterClass_mm` (hidden price-matrix column, added 2026-10-30 for price tuning: the lower limit in mm of the file diameter class a whole-cm row was spread from) are kept in copy A/B, edits, the scenario xlsx (since restructure R2.5: assortment columns, sheets target_<n> and file_classes) and the .hpr/.pin round trip. Exported .hpr/.pin write whole-cm diameter classes, so re-importing an exported file gives whole-cm classes instead of the original file classes. An uploaded price file for an assortment replaces its cells, so those cells have no Distribution.

## Phase 6: stem profile import (not scheduled in the 2026-10-27 roadmap)

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

## Restructure (done 2026-10-30)

Four tabs (Stems, Assortments and prices, Simulator, Price tuning); stems and scenarios independent; one upload field per scenario (.xlsx, .csv, .hpr, .pin); scenario names; a target distribution editor per assortment (also in the scenario xlsx); price tuning of several assortments at once from their own targets. Tested and approved by Lennart; merged to main. Details are in CLAUDE.md (Stems tab, Scenario names, Distribution, Price tuning tab rows). Full suite: 22 PASS. file5 check (`scripts/restructure_manual_check.R`): 140 SCA alone FG 82.9 -> 94.1, value -0.17 %; with 148 (Even target) 98.5 / 35.1, value -0.60 %. R1.9 crash: not reproduced (`scripts/repro_tuning_crash.R`).

### Decisions made
- Single-product tuning baseline lives in `outputs/restructure/` (gitignored), not in the repository (derived from a private buyer price list); audit 33 check 14 gives REVIEW without it.
- The scenario upload field has no .rds; NFI shows no stem summary (trees are built at run time); the grade boxes already sat in the scenario cards.
- Stems come only from files uploaded on the Stems tab; an import into a scenario never touches them and loading stems never touches a scenario.
- Target editor: an edit sets all whole-cm rows of the same `FileDiameterClass_mm`; first target sets Allowed TRUE, "Volume of logs", WithinProduct TRUE, MAXDeviation 0; the xlsx keeps the fields, `target_<n>` sheets and a `file_classes` sheet.
- Tuning: overall FG = mean weighted by iteration-1 logs (or volume); results are cleared only by a change of the tuned scenario, the selection or the stems, not by applying the prices to the other scenario; assortments with fewer than 20 logs are named in a warning and left out.
- Result field for the names is `ab_names` (a name starting with `scenario` partially matched `result()$scenario`).
- **Scenario names are read back from the scenario xlsx** (Lennart, 2026-10-30): uploading A's file into B also sets B's name to A's; Copy from A/B never changes a name.
- **Buttons that name the other scenario ("Copy from B", "Make assortment equal to B") keep the letters A and B** (Lennart, 2026-10-30).

Open questions: none. Blocked items: none.

## Polish (in progress) — Phase R4 "practitioner polish", branch `polish`

Rules, design and answers copied verbatim from Lennart's task (2026-11-02). After any context compaction re-read **this whole section** and `git log -15`. Short entries such as "as in the design" always mean the verbatim text below.

### Progress checklist (tick + commit hash)
- [ ] 0. This section (commit)
- [ ] 1. D3 Sortiment og priser
- [ ] 2. D1 Guidance (tabs, intros, empty states, Help tab)
- [ ] 3. D2 Stammer tab
- [ ] 4. D4 Simulator > Sammendrag
- [ ] 5. D6 StanForD exports (+ audit 35)
- [ ] 6. D5 Pristilpasning presentation
- [ ] 7. D7 Tuning on all stem sources
- [ ] 8. Full suite, manual check, screenshots, CLAUDE.md/PLAN.md, final report

### Decisions made
(what, choice, alternatives, why — appended after each step)
- XSD files live in `C:/apter_private/stanford_xsd/` (`v31/`, `v33/`), never committed (no licence text in the page or the files). Audit 35 gives REVIEW if they are missing. `.pin` is written as version 3.3 (as real.pin); an `.hpr` export keeps the source file's version (file5: 3.1) and is validated against the matching XSD.

### Open questions for Lennart
(none yet)

### Blocked items
(none yet)

### Rules (whole session)
- Branch: `polish` (from main). One git command per call, no &&; `git -c safe.directory=*` if needed. Push only with `git push origin polish`, never to main. No merge.
- Rscript: `C:\Users\leno\AppData\Local\Programs\R\R-4.5.1\bin\Rscript.exe`
- Progress log: this section with a checklist of all steps (tick + commit hash), "Decisions made" (what, choice, alternatives, why), "Open questions for Lennart", "Blocked items". Update and commit after every step. After any context compaction, re-read this section and `git log -15` before continuing.
- In Stage C, where something is still open, choose the simplest option that keeps the UI plain and existing behaviour, log it, continue.
- Audits: affected audits after each step; after each group of steps 17, 28, 29-34 and `performance_check.R`; at the end the full suite (`Rscript scripts/run_audits.R`). Restore tracked audit outputs that differ only in timestamps, runtimes, temp names, folder names or CRLF (one git restore per file).
- `performance_check.R` must match main. Bucking and tuning numbers must not change: audit 33's single-product invariant and `outputs/restructure/tuning_single_expected.rds` must still hold. Only the export files (D6) may change on purpose.
- Never weaken or delete audit checks. Updating expected UI structure (tab names, moved elements, labels) is allowed when this design intends it; log each such change.
- If a step fails after three genuine attempts: revert its uncommitted changes, log it under "Blocked items", continue with the next independent step. Stop the whole session only if `performance_check.R` cannot match main, a private file would be modified, or the repo would lose data.
- Private files (`C:/apter_private/`: file1..file5 as in audit 33, hand.pin, real.pin) are read in place, never copied or committed.
- UI plain: no notes beyond the texts specified here, no new colours outside the app's existing theme tokens. Every new text in translations.csv in English and Norwegian Bokmål; audit 28 must pass.
- Headless Chrome (audits 17/29): existing patterns (shoot(), browser_checks in tryCatch/finally, own Chrome profile). Never kill Chrome by name; retry a flaky check once.

### DESIGN (decided; implement in Stage C)
**D1 Guidance**
- Numbered tab labels: "1 Stammer", "2 Sortiment og priser", "3 Simulator", "4 Pristilpasning" (EN "1 Stems", "2 Assortments and prices", "3 Simulator", "4 Price tuning"). A last unnumbered tab "Hjelp"/"Help".
- One intro sentence at the top of each tab, in the same style as the existing one on Sortiment og priser:
  - Stammer: NO "Velg stammene optBuck skal kappe: et utvalg fra Landsskogtakseringen, en egen treliste eller stammer fra hogstmaskinens produksjonsfiler (.hpr)." EN "Choose the stems optBuck will buck: a selection from the National Forest Inventory, your own tree list, or stems from harvester production files (.hpr)."
  - Simulator: NO "optBuck finner den mest verdifulle kappingen av hver stamme med prisene i scenario A og B, og sammenligner resultatene." EN "optBuck finds the most valuable bucking of each stem with the prices in scenario A and B, and compares the results."
  - Pristilpasning: NO "Juster prisene i et scenario slik at kappingen gir lengdefordelingen sagbruket ønsker (ønsket fordeling), med minst mulig verditap." EN "Adjust the prices in a scenario so that bucking gives the length distribution the sawmill wants (target distribution), with the smallest possible loss of value."
- Empty states: Simulator and Pristilpasning show NO "Velg stammer under 1 Stammer først." / EN "Choose stems under 1 Stems first." when no stems are ready (instead of failing or showing empty panels). (Answer 1: the built-in source counts as ready with its current selection; the empty state applies only to .hpr and tree-list sources with nothing loaded.)
- Help tab, plain text in three sections. (1) "Slik bruker du optBuck": one or two sentences per numbered tab. (2) "Begreper", using these definitions (translate faithfully, do not add claims):
  - Scenario A og B: two sets of assortments, prices and bucking rules that optBuck compares on the same stems.
  - Ønsket fordeling: the share of logs (or volume) the sawmill wants in each length class within each diameter class; stored per assortment in .pin files.
  - Fordelingsgrad: how well the length mix in each diameter class matches the target, 100 % = perfect match; weighted over diameter classes by number of logs (or volume).
  - Pristilpasning: prices are adjusted step by step until bucking for maximum value gives a length mix close to the target.
  - Kostnad ved styring: the loss in value of the whole harvest, at the original prices, when bucking with the tuned prices instead of the original ones.
  - .hpr: harvester production file with stem profiles, logs and the products used. .pin: product instruction file with assortments, price matrices and target distributions, sent to the harvester.
  - For the other terms used in the app (salgbart volum, oppgjørsvolum, stammekvaliteter, sortiment, prismatrise) draft definitions from how the code computes them, and list them under "Open questions for Lennart" for review.
  - (3) "Filformater": accepted uploads per tab.

**D2 Stammer tab**
- Built-in source: a button NO "Bruk dette utvalget" / EN "Use this selection" below the settings. On click, build the tree list (no bucking) and show the right-hand panel. Changing any setting afterwards clears the panel until the button is clicked again. The Simulator run keeps working as now.
- Right-hand panel for every source: the existing species table plus two density plots, diameter at breast height (cm) and tree height (m), one line per species. Move them from Simulator > Trær and remove them there; if that tab is then empty, remove it (Stage A: the tab keeps the DBH-class figure, trees table and stem diagram, so it stays).
- Keep the "Bruk sortimentene fra denne filen i scenario A/B" buttons.

**D3 Sortiment og priser**
- Upload label: NO "Last opp sortiment og priser: scenario (.xlsx), prisfil (.xlsx eller .csv) eller hogstmaskinfil (.hpr eller .pin)" / EN "Upload assortments and prices: scenario (.xlsx), price file (.xlsx or .csv) or harvester file (.hpr or .pin)".
- Remove the scenario name inputs and every use of the names (results, legends, file names, xlsx metadata). Uploading an xlsx that still has a name sheet must work and ignore it.
- Rediger > Fast pris: "Startpris (kr/m³)" becomes NO "Pris (kr/m³)" / EN "Price (NOK/m³)".

**D4 Simulator > Sammendrag**
- Top: one sentence, NO "Scenario B gir {kr} kr ({p} %) høyere|lavere verdi enn scenario A for disse stammene." (or "samme verdi som"), EN equivalent.
- A comparison table: rows Totalverdi (kr), Kr per m³, Salgbart volum (m³fub), Oppgjørsvolum (m³), and sawlog share (%) (answer 2: sawlog commercial volume / total commercial volume, m³fub); columns A, B, Differanse (B - A), Differanse (%).
- Run info (kjøretid, prøveflater, trær, trær med stokker, endrede apteringsmønstre) as one small line or small tiles below the table.
- One small bar chart, value per assortment type A vs B side by side in theme colours.

**D5 Pristilpasning (presentation only; numbers unchanged)**
- List all sawlog assortments of the chosen scenario. Tunable ones as checkboxes (checked by default). Non-tunable ones greyed and disabled, with the reason in grey after the name: NO "ingen ønsket fordeling (legg inn under 2 Sortiment og priser)" / "bare én lengdeklasse" / "bare én diameterklasse" (EN equivalents). Pulpwood is not listed.
- Results, top to bottom:
  1. NO "Prisene ble tilpasset på {n} av {N} stammer; tallene under gjelder alle {N} stammene." (EN equivalent).
  2. Table NO "Treff per sortiment": one row per assortment: Sortiment | Stokker | Fordelingsgrad (%) før | etter | Volum (m³) før | etter | Status. Status NO "Tilpasset" or "Ikke tilpasset: bare {k} stokker; minst 20 stokker per diameterklasse trengs" (when no diameter class had at least 20 logs). Column groups NO "Før tilpasning" / "Etter tilpasning".
  3. Line under it: NO "Fordelingsgrad: hvor godt lengdefordelingen i hver diameterklasse treffer ønsket fordeling (100 % = perfekt treff)."
  4. Table NO "Kostnad for hele hogsten": rows Verdi av alle stammer (kr), Sagtømmervolum (m³); columns Før, Etter, Endring (kr and %). Caption NO "Begge verdiene er regnet med de opprinnelige prisene. Endringen er verditapet ved å kappe mot ønsket fordeling i stedet for høyest mulig verdi."
  5. Figure below the tables: title NO "Fordelingsgrad per iterasjon", about 700 x 320 px, one coloured line per tuned assortment in theme colours, best iteration marked, legend with display names. Caption NO "Prisene justeres trinn for trinn. Kurvene viser hvor godt hver iterasjon treffer ønsket fordeling; den beste iterasjonen (markert) brukes." (Answer 5: colours from the existing theme tokens in app/www/apter.css, first line in the primary green; with fewer tokens than assortments the same colour with a different line type; no new colour values.)
  6. Matrices as now, with fixes: rows are diameter classes, so the row header is NO "Diameterklasse (mm)" and a column group label NO "Lengdeklasse (cm)" sits above the length columns (currently "Diameterklasse (mm)" wrongly sits above the length columns); column "FG" becomes NO "Fordelingsgrad (%)"; diameter classes without logs show "–" instead of 0 and an empty fordelingsgrad; in the price change view unchanged cells show "–" with the caption NO "–: ikke justert (færre enn 20 stokker i diameterklassen, eller ingen ønsket fordeling)."
  7. Use display names everywhere, as in the checkbox list ("Gran – REDACTED25"), never internal ids like "Spruce REDACTED25".

**D6 StanForD exports (.pin and .hpr)**
- Every export must validate against the XSD (`C:/apter_private/stanford_xsd/`). Fix every violation; loop until zero errors (libxml2 reports one error per sequence).
- For products imported from a file, keep the source's own elements and values (ProductUserID, SpeciesGroupUserID, ModificationDate, ManualFreeBuck, TopLogFreeBuck, ProductCondition, LimitationDefinition, CuttingWindowDefinition and any other element Apter does not edit); store what is needed as hidden data at import, following the Distribution pattern. Write only what Apter changes (prices, targets, edited classes). Products created in Apter get schema-valid neutral values (log the choices).
- Set ModificationDate to the export time for products whose prices or targets differ from the imported ones.
- Write the original file diameter classes (FileDiameterClass_mm) when present; remove that known limitation from PLAN.md.
- Audit checks: exported .pin and .hpr validate against the XSD (REVIEW if the XSD is missing); ProductUserID and SpeciesGroupUserID survive import -> export; file5 import -> .pin export -> import gives identical prices, targets and classes.
- Answer 3: neutral values for Apter-created products provided every export validates. SpeciesGroupUserID for a product created in Apter = the SpeciesGroupUserID of an imported product of the same species in that scenario if one exists, otherwise "Spruce"/"Pine". ProductUserID for such products: built from the name, ASCII only ([A-Za-z0-9_-], Norwegian letters transliterated), unique within the file. ProductCondition and LimitationDefinition: schema-valid neutral values, logged.
- Extra: ProductUserID unique within each exported file (the XSD has a unique constraint): keep the original on the first, append "_2", "_3" to the others; log; audit 35 check.
- Extra: ModificationDate: at import store a hidden fingerprint per product of its imported prices and targets (hash of the price and Distribution cells). At export a product whose current fingerprint differs, or that has none, gets the export time; others keep their imported ModificationDate. Audit 35 check (unchanged product keeps its date; product with one edited price gets a new date).
- Extra: hidden source elements must also survive copy assortment, "Gjør sortimentet likt A/B", replace_scenario() and "Bruk tilpassede priser i scenario B". Audit 35.
- Extra (answer, A5 edge case): price edits behave like target edits. Editing a price cell sets the price for every whole-cm row with the same FileDiameterClass_mm (only that row when it is NA). Bulk edits are unaffected. Audit check. Default assortments have no file classes, so performance_check must still match main.

**D7 Tuning on all stem sources** (feasible per Stage A): allow price tuning with built-in and tree-list stems using the same functions; the D1 empty state replaces the ".hpr only" message. No cache (answer 4): accept the ~6 s NFI preparation.

Order in Stage C: D3 -> D1 -> D2 -> D4 -> D6 -> D5 -> D7, one commit per finished step, push the branch after each group.

### End of Stage C
1. Full suite, all PASS (REVIEW only for skipped private or XSD checks, explained).
2. Manual check on file5: tune 140 SCA alone with its file target, FG on all stems 82.9 -> 94.1, value -0.17 % (unchanged). If D7 is done: one tuning run with built-in stems (the A1 selection: Gran og furu, HKL 5, bonitet 11/14/17, fylke 11 Rogaland, 0-1000 m; 21 plots, 18 spruce, 131 pine) on default assortments with an Even target on the main spruce sawlog assortment; report numbers and runtime.
3. Screenshots in English and Norwegian of every tab, the Stems panel for built-in and .hpr stems, the Simulator summary, the Pristilpasning results with each matrix view, and the Help tab, in `outputs/app_smoke/review/polish/`. List them in the report.
4. Update CLAUDE.md and PLAN.md (keep Decisions made, Open questions, Blocked items for review). Commit, push branch, no merge.
5. Final report (short, tables): per design item what was done with commit hashes; validation results; decisions; open questions; blocked items; audits (also every audit expectation changed and why); manual check; screenshots.

### Stage A findings (summary)
- NFI tree list for the A1 selection: 0.08 s read + 0.19 s select; panel density plots are `fig_distributions` (`app/app_server.R:843`, `apter_distribution_figure()` in `app/helpers_ui.R:479`); Trær keeps `fig_dbh_class`, `tbl_trees`, `stem_compare`.
- Tuning works on any job (`prepare_sampled_job`/`prepare_tree_list_job`/`prepare_hpr_stems_job`).
- XSD validation before changes: file5 valid (3.1), real.pin valid (3.3), hand.pin 12 errors (hand-made, no action); our .pin 25 errors (areaUnit/volumeUnit/weightUnit, header ApplicationVersionCreated, ProductGroupName before ModificationDate); our .hpr 23 errors (ProductInfo/ProductGroupName before ModificationDate).
- Import keeps only `HprProductKey`; ProductUserID/SpeciesGroupUserID are lost (pin writes `200_Massevirke`, file5 has `200_Massevirke_FURU`; species `NO1_FURU`, `NO2_Gran`, ...).
- Export builds diameter classes from whole cm x 10 (`R/helpers_hpr_export.R:156`); `FileDiameterClass_mm` survives `pm_minimal()`.
- Summary numbers all available (`per_m3_summary` Total, `selected_logs_all`, `assortment_summary()`, `compare_by_type()`).
