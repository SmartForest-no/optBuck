# Apter — context for Claude and other AI assistants

This file is for AI collaborators. Apter was called nfiBuck until the
2026-10 rename. Other documents:

- [README.md](README.md) is **partner-facing** (industry partners). Keep it
  free of internal material: audits, prototype history, data sanitization,
  Oracle, and phase plans belong in [docs/development.md](docs/development.md)
  or here. Mark features as planned until they are built.
- [docs/development.md](docs/development.md) covers the developer side:
  repository layout, project root, audits, the data pipeline, and the files
  that keep the old name on purpose.

## What this is

Apter is a bucking simulator: given a spruce or pine stem's taper profile
(`taperNOR`) and a set of product/price definitions, a backward-induction DP
optimizer finds the cutting pattern that maximizes total timber value. It is
for comparing price matrices and planning bucking, not an inventory or
estimation tool. Trees come from sanitized NFI data (all plots of the selected
stratum), an uploaded tree list (Phase 5) or the measured stems of a StanForD
2010 .hpr file (hpr Phase B); stem profile import is planned. Products and
prices can be imported from .hpr files into a scenario (hpr Phase A).
A Shiny app (`app/`) exposes the pipeline. **The app shows the name
"optBuck"** (title, navbar, browser tab, download file names; Phase 4d,
`APP_DISPLAY_NAME` in `app/helpers_ui.R`). The repository, paths, code and
docs keep the name Apter.

## Domain terms (Norwegian NFI)

- **hogstkl** — maturity/harvest class (1–5; the app only offers 3–5,
  production/mature forest — 1–2 are young/establishment, excluded by design)
- **abonitet** — site productivity class (bonitet; lower number = poorer site)
- **fylnr** — county code. Norway's counties were reorganized in 2024; the
  app's `FYLNR_CHOICES`/`.FYLNR_NAMES` mapping in `app/setup.R` handles both
  current and historical codes.
- **hoh** — elevation (m)
- **bucking** — cutting a stem into logs (sawlog/pulpwood/waste) to maximize
  value; **taper** — the stem's diameter-vs-height profile

## Accepted assumptions (load-bearing, not guesses)

| Parameter | Value | Where |
|-----------|-------|-------|
| Stump height | 10 cm | first cut position above terrain |
| DP grid step | 10 cm | position resolution in `helpers_dp_optimizer.R` |
| Taper function | `taperNOR::taperNOR()`, over-bark (`with_bark = TRUE`) profiles | `helpers_taper.R`; the precomputed cache stays over bark |
| Bark model | `taperNOR::barkNOR()` along the whole stem, all tree sources; dbh = measured dbh, or read from the profile at 1.3 m for imported profiles. Double bark floored at B ≥ 0 so under bark ≤ over bark — **provisional (2026-10-12), pending the taperNOR authors** (barkNOR gives negative pine bark near the top) | `R/helpers_bark.R` (swappable: `BARK_MODELS`, `options(apter.bark_model = …)`); audit 18 |
| `DiameterUnderBark` | `TRUE` for all six products: product limits, price-diameter class and price volume all use the under-bark diameter (one diameter drives all three, as in optBuck `buckStem`) | `inputs/ProductData_spruce_pine_default.csv`; `helpers_candidate_generation.R`; audit 14 check 7 |
| Price lookup | top diameter **under bark**, floored to cm (class cap 50 cm) | `helpers_candidate_generation.R`, `helpers_valuation.R`; audit 18 check 8 |
| Price-volume method | per product, `ProductData$VolumeDiameterCategory` (Phase 4b, 2026-10-13): `Top cylinder` (Apter value; the default sawlog rule, labelled `Top` until Phase 4b): cylinder at the under-bark top diameter rounded down to whole cm × log length; `Top` (StanForD/optBuck): frustum with 1 cm/m taper (optBuck's former `LogLength*0.01` was a bug); `All diameters (solid volume)`: Apter's solid volume (below); `Calculated Norwegian mid` (below); `Mid` (below, 2026-10-21). Formula methods rounded to 5 decimals; defaults unchanged (audit 20 check 8) | `R/helpers_volume_methods.R` `price_volume_m3()`; audit 20 checks 1-5 |
| Norwegian mid | Målereglement for skogsvirke, Generelle bestemmelser (Norsk Virkesmåling, approved 15 April 2026), 4.1.3–4.1.4: Dt 10 cm from the top end (`DiameterTopPosition = 10`), rounded down to whole cm, + 0.5 cm (class midpoint), taper 1.0 cm/m; V = **3.14**/4 · (Dt + L/20)² · L/100 (cm, dm, dm³), 3.14 not π to reproduce the official volume; not rounded to whole dm³. Dt 187.6 mm, L 4 m → 131.9585 dm³. optBuck's `PriceVolumeCalc.R` rounds Dt to whole mm and adds 0.5 mm (units bug, 0.13657 m³ there); Apter follows optBuck with the units fixed (decided 2026-10-13) | `R/helpers_volume_methods.R`; audit 20 check 3 |
| Mid | StanForD 2010 VolumeDiameterCategory 3 (2026-10-21): V = π/4 · Dmid² · L, rounded to 5 decimals. Dmid is the under-bark diameter **measured at the middle of the cut log**, (StartPos + StopPos)/2, interpolated from the stem profile (off the 10 cm grid for e.g. a 370 cm log), rounded down to whole cm, no class midpoint added (unlike Norwegian mid, which is calculated from the top). L is the price length: the cut, or the class lower limit for hpr products priced on length classes (`price_len_of()`); the midpoint stays at the middle of the physical log. The top diameter (at `DiameterTopPosition`) still drives the limits and the price class. optBuck's `PriceVolumeCalc.R` has no Mid branch; the rule is Lennart's (decided 2026-10-21) | `R/helpers_volume_methods.R` (`mid_d_mm`), `.candidate_columns()`; audits 20 check 15, 25 check 16 |
| `DiameterTopPosition` | cm below the cut where a product's top diameter is read; drives its limits, price class and price volume (one diameter for all three). 0 for the default products (at the cut) | `helpers_candidate_generation.R`; audit 20 check 5b |
| `VolumeDiameterAdjustment` / `VolumeLengthCategory` | rounding down to cm applies to Top cylinder, Top and Mid; Norwegian mid always floors; solid volume never rounds (combination rejected). Since Phase 4d the rounding is not a setting: it is fixed per method (`volume_rounding()`: Top cylinder, Top, Norwegian mid and Mid down to cm, solid volume none). Length category has no effect: logs are cut at exactly their class length (no trim allowance) | `R/helpers_volume_methods.R`, `validate_products()` |
| Default diameter limits | under bark, from optBuck's example.hpr (`DiameterClassMAX`, `DiameterMAXButt`; Phase 4c, 2026-10-13): spruce sawlog max top 60 / max butt 65 cm, pine sawlog 50 / 60, pulpwood 70 / 70, waste none; minimum top diameters unchanged (12, 5, 0). The only intended result change of Phase 4c (`outputs/performance/phase4c_limits_change.txt`); baseline audits use `pre_phase4c_limits()` | `inputs/ProductData_spruce_pine_default.csv`; audit 23 check 2 |
| Assortment table | (Phase 4c/4d) base and custom assortments in one table, one row per ProductKey = `<species>_<slug(Name)>`, edited and removed alike; "Reset to defaults" restores A's base rows and keeps custom ones. Columns `ASSORTMENT_TABLE_COLS` (no LengthStep or rounding since Phase 4d). **Type is a hidden internal field** (never shown or in the form): base rows keep theirs, so a renamed base waste row stays waste; user-added rows are always commercial (`custom_type()`: slug of the name, never "waste"). Hidden, generated: ProductName, VolumeLengthCategory (no effect), VolumeDiameterAdjustment (`volume_rounding()`), DiameterUnderBark (always TRUE), DiameterSource, Priority (derived: matrix-priced 1, flat-priced commercial 2, waste 99). A matrix-priced product's lengths are the columns of the run's matrix (a removed column = length not allowed) and its price classes the rows (consecutive, added/removed at the ends only); flat-priced: MinLength..MaxLength **every 10 cm** (`FLAT_LENGTH_STEP_CM`). LengthClasses is derived, never edited. The defaults give the same ProductData and LengthClasses as the files (audit 23 check 1). AllowAsFirstLog/AllowAsTopLog removed (always TRUE, unused) | `R/helpers_products.R` (`defs_from_products()`, `products_from_defs()`); audits 23, 24 |
| Scenarios A and B | (Phase 4d, replacing Phase 4c's price sets) **two independent scenarios**, each with its own assortment table and price matrices (`sc$A`, `sc$B` in `app/server_scenarios.R`). B starts as an exact copy of A; nothing is linked, there are no Same as A / Own prices / Not used states, both sides are always editable and nothing is greyed. Cells that differ on an assortment both scenarios have are highlighted on both sides (`assortment_diff()`; price cells against the other scenario's grid of the same assortment, a cell without a counterpart there highlighted, every cell when the other prices it flat). **Assortments only one scenario has are not highlighted** (rows, grid and flat price; 2026-10-20). **Each table lists only its own assortments** (2026-10-20, hpr Phase A: the struck-through "Not in A/B" rows were removed). Both tables use the same fixed row order (species spruce, pine; then sawlog, pulpwood, waste by the hidden Type; then custom by name) and cannot be sorted by clicking. Each assortment's price type (matrix or flat) is set per scenario, so spruce pulpwood can be matrix-priced in A and flat in B. **Layout (2026-10-19):** each scenario is a white card on the grey page, one flow from top to bottom: the table (click a row to select it), a detail panel for the selected assortment (its rules in one line + Edit; its prices directly below: the matrix grid with row/column buttons, or one flat price field; Remove, "Make assortment equal to B/A" (`copy_assortment()`: definition and prices), Reset prices (base assortments priced as by default), price upload/download for that assortment only (`upload_assortment_prices()`)), then the scenario actions (Add assortment, Reset to defaults, Copy from B/A, Download/Upload scenario; the upload also takes a full price file, applied as `upload_price_set()`). **The selection is shared by A and B** (one `selected` key in `scenario_server()`): a side without the assortment shows no detail panel (2026-10-20; "Not in this scenario" and "Add from A/B" were removed: a missing assortment is added with Add assortment, or a whole scenario copied with Copy from A/B). There are no assortment or matrix dropdowns, no flat-price table and no "Make matrix equal" button. Flat price edits carry their scenario and key (`input$flat_edit` from `www/apter.js`), as grid edits carry `matrix_id`, so a late edit is never written to another assortment. **Copy from A / Copy from B** (2026-10-22; `scen_copy_b` / `scen_copy_a`, confirmed in a dialog, `scen_copy_ok`) copies the whole other scenario, all assortments and prices, through `commit()`; the .hpr file kept for ".hpr stems" is untouched. Adding offers "Add to both scenarios" (default on); removing asks "this scenario only" or "both". A comparison runs when `scenarios_differ()` (assortments or prices differ); no status note is shown. NOK/m³ per scenario uses that scenario's own commercial volume (intended). One xlsx per scenario (`scenario_export()`/`scenario_import()`: sheet `assortments` incl. the hidden Type, plus the wide price sheets). User-facing text says "Scenario A/B"; never "default" or "price set"; internal names (`default`, `scenario`, backend `$scenario`) stay | `R/helpers_price_matrix.R` (`assortment_diff()`, `same_assortments()`, `same_prices()`, `scenarios_differ()`, `copy_assortment()`, `copy_prices()`), `app/server_scenarios.R`; audits 17, 23, 24 |
| NFI plots | (Phase 4d) the app uses **every plot of the selected stratum** (backend `params$all_plots = TRUE`: no sampling, no RNG, no cap). Before Phase 4d the app asked for 200 plots but the backend capped `n_plots` at 50. The explicit `n_plots` path (default 10, max 50, seed 42) is unchanged for scripts and audits; the test-only `APTER_N_PLOTS` makes the app sample that many (audit 17 uses 5). The sidebar always shows live tree counts ("801 plots · 12,427 spruce · 4,166 pine trees") and a rough run time (`estimate_run_seconds()`), from the cached tree table and the run's own selection code: `select_nfi_trees()` (`R/helpers_tree_selection.R`, steps 3-5 of `prepare_sampled_job()` moved there unchanged) with `build_run_params()`; never reimplement the selection (audit 17 check 4e). Plots are selected by tree species (eligible if the stratum has trees of the chosen species on them), not by dominant species; only trees of the chosen species are bucked; DBH ≥ 5 cm and height ≥ 4.5 m (applied after plot selection); data preparation keeps only spruce and pine. **Altitude** (2026-10-20): `hoh` (m a.s.l., a plot attribute) via the sidebar range slider `hoh_range`, 0–1,100 m in 50 m steps (`HOH_RANGE`, `HOH_STEP_M` in `app/setup.R`), full range by default; filtered at plot level in `select_nfi_trees()` with inclusive `hoh_min`/`hoh_max`. At the full range `build_run_params()` sends no bounds (`selected_hoh()`), so params and results equal those without the filter and plots with missing altitude stay in; a narrowed range excludes them (none in the data; audit 17 checks 4f, 28). Default stratum: Spruce 655 plots / 12,427 trees, Spruce and pine 801 plots / 16,593 trees (docs/development.md). Tree lists: the rows in taperNOR's range | `app/setup.R`, `build_run_params()`, `R/helpers_tree_selection.R`, `prepare_sampled_job()` |
| Custom assortments | extra ProductData rows (Phase 4b; since Phase 4c rows of the assortment table): species, min/max top and butt diameter, lengths, `DiameterTopPosition`, volume method, `PriceType` matrix or flat. Definitions are shared by A and B (only membership, prices and matrix axes differ). The Phase 4b definition format (`custom_assortment_rows()`, `products_for_run()`, `UseIn`, explicit Priority) remains only for `scripts/performance_cases.R` and audit 20, whose baselines depend on it. `ProductType` = the assortment class: groups species in the Total per-m³ scope; user-added assortments never get `waste` (`custom_type()`; Phase 4b definitions: `validate_assortment_defs()`), so they always count in the denominator; a renamed base waste row keeps `waste` (audit 24 check 7). No quality restrictions (stems defect-free). An unpriced custom assortment FAILs price validation. Editing an assortment in the app keeps its A and B prices when its pricing stays (moved to the new key on a rename); changing matrix <-> flat starts from the starting price. Colours (restyle, dataviz validator all pairs, headless Chrome, 2026-10-16): sawlog `#3F6E58`, pulpwood `#C09A66`, waste `#CBD0CD`, custom fold `#3A3836` (worst CVD dE 15.5, normal 16.5); the requested pulpwood `#C9A97A` / waste `#B9BFBC` failed (normal dE 8.7) and were shifted slightly; A green, B sand `#C09A66`. Species in the tree figures (2026-10-19): spruce `#3F6E58`, pine `#C9A97A` (`APTER_SPECIES`; CVD dE 22.2, normal 27.1) | `R/helpers_products.R`, `R/helpers_price_matrix.R`, `app/theme.R`; audits 20, 23 |
| DP tie-breaking | totals within `DP_TIE_TOL_NOK` = 1e-6 NOK count as equal; then fewer logs, higher `ProductData$Priority` (sawlog 1, pulpwood 2, waste 99; derived from pricing in the app since Phase 4c), longer log, `ProductKey`; unallocated ranks after a log at equal log count. Independent of row order and prices; reported value = sum of selected logs (decided 2026-10-12) | `R/helpers_dp_optimizer.R` and `.optimize_dp_fast()` must stay identical (audits 16 checks 14-15, 19) |
| Pattern change | a tree's cutting pattern counts as changed only if the log sets differ and value (at either matrix) or commercial volume by assortment differs; equal-value ties are not changes | `R/helpers_comparison.R`; audit 19 |
| "Solid volume" | **Everywhere in Apter** (pulpwood and waste price volume, and the NOK/m³ denominator): the sum of 10 cm cylinders, each using the diameter at the **top** of its section (under bark), not rounded. One shared function, `section_volume_fun()`; don't reimplement it (decided 2026-10-12, replacing the prototype's mean-of-end-diameters rule) | `R/helpers_commercial_volume.R`, used by `helpers_candidate_generation.R`; audit 18 check 10 |
| NOK/m³ denominator | commercial volume under bark: 10 cm cylinders over non-waste logs, each using the under-bark diameter at the top of its section; excludes bark, stump, waste and the top above the last merchantable log; independent of prices and product volume rules. Assortment contribution = assortment value ÷ the scope's commercial volume; each species uses its own denominator | `R/helpers_commercial_volume.R`; audit 18 |
| Max price-diameter class | per product: the largest `PriceDiameterClass_cm` in that product's matrix in the run's price matrix (50 cm for the default 2021 matrices); logs above it are priced at that class — physical diameter is never changed. Applied inside `lookup_prices()`, so re-pricing under another matrix uses that matrix's cap (amendment, 2026-10-13) | `build_price_lookup()` / `price_class_used()` in `helpers_valuation.R`; audit 20 check 12 |
| Waste price | 0 NOK/m³, intentional (accepted 2026-10-12) | price matrix; audit 14 check 18 |
| Tree-list heights | measured heights kept exactly. Missing heights per species: **≥ 10 measured** (`HEIGHT_MIN_MEASURED_PER_SPECIES`; 10 chosen from a synthetic test, 49/50 fits within 2 m) → fieldPlot `fitHD_Plot()` (Näslund, plot random effect; plot-level prediction where the plot has measured heights of the species, else the species curve `level = 0`), `fitHD()` (nls) with < 2 plots, an error or the time limit; **1–9** → FinnHoyde × ratio, ratio = Σh_measured / ΣFinnHoyde(d) over that species' measured trees (NFI tariff method; warning outside 0.7–1.3); **none** → FinnHoyde uncalibrated with the site-independence warning. Each tree reports `height_source` (NFI trees: "NFI") (decided 2026-10-13) | `R/helpers_heights.R`; audit 21 checks 1–4 |
| FinnHoyde curves | spruce d < 15: 1.3 + 0.6d (Eid & Fitje 1993), d ≥ 15: −14.17 + 20.86·log10(d) (Fitje & Vestjordet 1977); pine d < 10: 1.3 + 0.65456d (Eid & Fitje 1993), 10 ≤ d < 35: 0.39 + 0.852d − 0.010644d² (Vestjordet 1968), d ≥ 35: 13.35 + 0.107d (Eid & Fitje 1993); d cm, h m | `finnhoyde_height()`; audit 21 check 1 |
| nlme time limit | 30 s per species fit (`HEIGHT_FIT_TIMEOUT_S`), enforced by Apter with `setTimeLimit()`; fieldPlot's own `timeout` is passed as `Inf` because it never interrupts nlme. fitHD_Plot() catches the time-limit error and returns its nls fallback, so a timeout is recognised by elapsed time | `R/helpers_heights.R`; audit 21 check 4 |
| hpr import (Phase A, 2026-10-20) | "Import from .hpr" per scenario **replaces** that scenario's assortments and prices (the other is untouched), after a dialog that confirms each species group's species and the products to import. Parser ported from optBuck's getters onto xml2 (read, not vendored; no optBuck dependency). Each product → one **matrix-priced** row per mapped species (+ the base Waste row per species, 0 NOK/m³); min top = `DiameterMINTop`, max top = `DiameterClassMAX`, max butt = `DiameterMAXButt` (mm ÷ 10, may be e.g. 14.2 cm), min butt 0; `DiameterTopPosition` kept; volume method Top / Calculated Norwegian mid / Mid / All diameters (solid volume) mapped 1:1, any other → solid volume + warning; Type from ProductGroupName (Tømmer/Sag* → sawlog, Massevirke/Energi → pulpwood, else `custom_type()`). Under-bark flags false, other volume rounding/length rules, margins: warnings (all limits read under bark). Errors block (not XML, wrong root/message type, units other than mm/cm, no Machine, duplicate keys, no groups/products, no group mapped, scenario fails `check_price_set()`; zip without .hpr). **Messages (2026-10-20):** after an import one line ("Imported 5 assortments from example.hpr into Scenario A."), plus warnings only when something is lost or replaced: a product skipped (unknown group, no priced cells, no whole-dm length), a length class dropped, an unsupported volume method or length rule replaced, over-bark flags read as under bark, a different volume top position replaced, margins ignored. Everything else that follows the documented rules goes to `$info` and is not shown: length rounding, non-whole-cm diameter classes, the mm volume diameter (Apter rounds to cm), forbidden cells and empty rows, narrowed top limits, renamed duplicate names, merged species groups, and `check_price_set()` notes. The dialog has no "Not taken over" note; unticked grade products say "Only for grades set by the operator (special orders)" (`HPR_GRADE_REASON`). **Files:** .hpr, .hpr.gz (gzfile) or an .hpr in a .zip (streamed with unz(); the first .hpr entry). Only the bytes before the first stem record (`<Stem>` followed by `<StemKey>`; `<Stem>` alone also occurs in biomass functions) are parsed, after closing the open tags (`.hpr_close_tags()`); if that part lacks species groups or products, the whole file is parsed. On the owner's six real files (3-151 MB) all definitions precede the first stem (0.3-1.3 MB read; 0.4-3 s; +21 MB process memory on 151 MB vs +1 GB for a whole parse). Upload limit 200 MB (`HPR_MAX_UPLOAD_MB`: option `apter.hpr_max_mb` or env `APTER_HPR_MAX_MB`; Shiny's one limit for all uploads). The uploaded file is moved (not copied) into `tempdir()/apter_hpr_<token>`, one file per session (a new upload replaces it); at session end that folder and Shiny's upload folders are deleted (audit 17 check 30) and returned by `scenario_server()` as `hpr_file` for Phase B. StanForD classic .apt (bucking instructions with price matrices) is a different, non-XML format and is not accepted | `R/helpers_hpr.R`, `app/server_scenarios.R`; audits 25, 17 check 29 |
| hpr lengths and cells | **Length classes round UP** to whole dm (365 → 37 dm: the log stays in its class; optBuck floors, 360); a class with no whole-dm length below the next class (or LengthClassMAX) is dropped. Diameter rows: whole cm from the first to the last priced class, row c = the largest file class ≤ c·10 mm (so 20.7–20.99 cm gets the 190 mm price; noted in info). **"Forbidden to buck the log even manually" cells are left empty** (never chosen; optBuck ignores BuckingCriteria); "Only logs bucked manually allowed" stays priced; fully forbidden end classes narrow min/max top; a fully forbidden inner class stays as **empty rows** (NA prices, never chosen; noted in info). `pm_drop_empty_cells()` keeps a whole empty class in the grid, scenario files and uploads so classes stay consecutive (a class missing altogether is still a gap error); partly empty rows drop their empty cells as before (decided 2026-10-20) | `.hpr_grid()`; audit 25 checks 6–7, 13 |
| hpr price length | Products whose file says "Length as defined in LengthClasses" (not solid volume) price their volume on the **class lower limit**, not the cut (cut 370 → priced on 365; the extra length is unpaid trim, as in StanForD). Hidden assortment field `PriceLengths_cm` (comma-separated limits; empty for everything else) → `LengthClasses$PriceLength_cm`, added only when some row has it, so base tables and all baselines are unchanged; kept on edits and in scenario files; `.prepare_runs()` treats a different price length as a different definition (decided 2026-10-20) | `price_lengths()` in `R/helpers_products.R`, `price_len_of()` in `R/helpers_candidate_generation.R`; audit 25 checks 8, 11–12 |
| hpr stems (Phase B, 2026-10-21) | ".hpr stems" is the Simulator's third data source, **never selected automatically**. It uses the file imported into a scenario this session with its confirmed species map (a new import takes over again), or a file uploaded in the panel (deleted once read). **Reading:** streamed in 2 MB chunks of complete stem records, never parsed whole; a regex whitelist per chunk (StemKey, SpeciesGroupKey, single/multi-tree kind, StemDiameters diameterCategory, DiameterValue positions and values), xml2 for a chunk with `&` or CDATA, comments stripped first; identical to xml2 on the owner's files and 2× faster. Text handled as bytes (`useBytes`; `substring()` needs an explicit end). The file's DBH, coordinates, dates, operators, names, recorded logs and HPRCMResults are never read. **Profile:** positions from the felling cut, **stump 0**; only the measured part (10 cm grid from 0 to the last measured position rounded down; off-grid positions interpolated; local increases kept); the profile ends exactly at DiameterMeasuredEndHeight in the owner's files (no predicted top). **Heights above ground:** only `hpr_height_above_ground()` = position + offset, offset = 130 − the species group's DBHHeight (0–50 cm; 10 cm if absent). DBHHeight counts from the cut: on the owner's files the file DBH equals the profile at DBHHeight in 99.6–100 % of stems. **DBH** = the over-bark profile at 1.3 m above ground, never the file's DBH; under bark with barkNOR (which needs no height). **No total height** is estimated (`hfromd()` exists; not used, decided 2026-10-21); `height_m` = measured length ("Measured length (m)"). **Dropped stems** (checked in this order): multi-tree, no StemDiameters, not over bark (under-bark profiles not supported yet), duplicate StemKey (the later ones), species group Not used (per group), unreadable values, not starting at 0, a diameter > 1,500 mm, ending below 1.3 m above ground. **Messages (2026-10-21):** the summary line ("3,176 stems read, 2,676 used.") covers the expected drops (Not-used species, multi-tree, no StemDiameters, ending below 1.3 m): counted in `$info` only, no warning. Warnings (count and reason, one per reason) only for problems in the file: not over bark (or no diameterCategory), duplicate StemKey, unreadable values, not starting at 0, > 1,500 mm. Every dropped stem is in `excluded_trees` with its reason. Errors: no stems; stems but no profiles (one of the owner's files has logs only); none left. **Results:** the tree-list tables, `tree_source = "hpr"`, treid = StemKey, no plots (no Plot column or Plots in the app; `plot_id_github` NA internally). **No cap** on stems (largest file 11,650 stems: A/B run ~23 s script, ~22 s in the app on 15 workers; workers peak ~2.3 GB) | `R/helpers_hpr_stems.R`, `run_hpr_bucking()`; audits 26, 17 check 31 |
| hpr species and grades | Species guessed from SpeciesGroupName + SpeciesGroupUserID letter tokens: prefixes gran/spruce/picea/kuusi/fichte → spruce, furu/fura/tall/pine/pinus/mänty/kiefer → pine; none or both → Not used; the user confirms. Several groups → one species: their products all become that species' rows (a duplicate name gets " (GROUP)"), noted in info that stems can then take either group's products. **Reference grades** (decided 2026-10-22, replacing "the lowest GradeNumber", which unticked sawlogs whose groups use grade 2): per species group, the grade(s) permitted by the most products of the group, each product counted once, ties kept; fallback StartGrade (no product of the group lists grades), then the lowest GradeNumber. Products that permit none of their groups' reference grades start **unticked** with "Only for grades set by the operator (special orders)": energy wood for rotten logs and special orders (e.g. butt logs, poles) that the operator assigns by setting a grade; stems are defect-free, so they would otherwise take every stem at special-order prices. **No exemption by product group or name** (a sawlog-group exemption was proposed and rejected). Products without permitted grades are kept. Grade names are labels without meaning (Kv1…, "A Knapp"…) and are not read. On the owner's five files this unticks only energy, rot, dry-wood and special-order products. Grades are otherwise ignored | `guess_hpr_species()`, `hpr_reference_grades()`, `hpr_grade_ok()`; audit 25 checks 3–4 |

## Known out-of-scope items (don't assume these are bugs)

- .hpr files: products, prices and species groups are imported (hpr
  Phase A, 2026-10-20) and the measured stem profiles bucked (hpr Phase B,
  2026-10-21). The recorded logs are ignored: comparing actual with optimal
  bucking is a later step. Under-bark profiles and multi-tree stems are
  dropped. CSV stem profile import is planned (phase 6). Quality grades are
  out of scope except the best-grade product filter
- No per-ha, plot-level, stratum or NFI-expansion outputs: results describe
  the selected trees only (plots are only how NFI trees are sampled). Never
  reintroduce per-ha values; audits 16 (check 10) and 17 (check 16) fail on them
- No stand-level routing — optimization is strictly per-stem
- Uploaded price matrices must conform to the `PriceMatrix_long` schema
  validated by `validate_price_matrix_against_products()` in
  `helpers_valuation.R` — no format auto-conversion

## Conventions: follow optBuck when undecided

When a modelling or measurement convention is not decided (product limits,
price-class lookup, volume methods, length rounding, bark handling, …),
follow Lennart's optBuck package (`https://github.com/SmartForest-no/optBuck`,
read it, don't vendor it), **unless** it conflicts with a decision recorded
in [docs/plans/PLAN.md](docs/plans/PLAN.md) or in this file. Report what
optBuck does before relying on it, and record the decision here.

Recorded so far: price-volume methods follow optBuck's `PriceVolumeCalc.R`
StanForD categories, with two deliberate departures — the frustum taper is
1 cm/m (optBuck had 0.01), and Norwegian mid fixes optBuck's units and uses
the Målereglement's 3.14 (see the accepted assumptions). Apter's solid
volume stays its own rule, not optBuck's "All diameters". optBuck has no
"Mid"; Apter's Mid (2026-10-21) is its own rule (accepted assumptions). The .hpr import
(2026-10-20) follows optBuck's getters with four departures: min/max top and
max butt from `DiameterMINTop` / `DiameterClassMAX` / `DiameterMAXButt`,
length classes rounded up (optBuck floors), forbidden cells empty (optBuck
ignores BuckingCriteria), and the class lower limit as price length without
optBuck's `PriceVolumeCalc()` off-by-one.

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
`scripts/local_only/export_plot_coordinates_rounded.R` is a local-only script
that is **not in the repository** (deliberately untracked; it exists only on
the project owner's machines). It queries Oracle for
plot coordinates and exports them rounded to 1 km to
`C:/apter_private/plot_coords_km.csv`, outside the repo. The project owner
authorised it for the 2026-10 status presentation only. It needs Oracle
access, writes nothing into the repository, and its output rows carry no
plot IDs of any kind. It is not used by the app or the pipeline, and it does
not make coordinates available for features: a map or location feature is
still out of bounds.

**.hpr files carry exact stem coordinates and personal names** (StemCoordinates
with WGS84 latitude/longitude, ObjectName, operators, contacts). The parser
(`R/helpers_hpr.R`) must never read those elements, and no real .hpr file goes
into the repository (optBuck's `example.hpr` included). Tests use
`inputs/test_fixtures/hpr_example_synthetic.hpr`, built by
`scripts/make_hpr_test_fixture.R` from a whitelist (species groups, products,
prices) plus one invented stem; that script refuses to write a file containing
coordinate or contact fields. Phase B (bucking .hpr stems,
`R/helpers_hpr_stems.R`) extracts a whitelist per stem only; audit 26 check 10
asserts that invented coordinates, dates and names reach no output. The
owner's real files live in `C:/apter_private/hpr/` (outside the repo) and are
used only by `scripts/hpr_stems_benchmark.R`, whose output is never committed.

## Architecture notes

- The app is split into explicitly sourced files under `app/` (no
  `R/` autoload, no `global.R`, no `modules/`): `app.R` (entry point: root
  resolution, sources the backend and the files below, `shinyApp(ui,
  server)`), `helpers_root.R`, `setup.R` (data, choice mappings, label,
  `APP_VERSION` from the repo-root `VERSION` file), `theme.R`,
  `helpers_ui.R`, `helpers_validation.R` (inputs -> backend params, stratum
  counts), `helpers_upload.R` (price files, uploads to A or B), `app_ui.R`,
  `server_scenarios.R` (the Assortments and prices tab: `scenario_server()`,
  Phase 4d) and `app_server.R` (Simulator: runs and results). Never name them `ui.R`/`server.R`: next to `app.R` Shiny
  treats those as a second app format, and shinytest2 refuses to start.
- **Styling:** bslib (Bootstrap 5) `page_navbar` in option 3
  "Combination" of `docs/design_reference/optbuck_design_directions.html`
  (2026-10-16): light grey page `#F3F4F3` (2026-10-19; `--apter-page`)
  behind white cards and a white sidebar card (`--apter-surface`; bs_theme
  `bg` stays white because Bootstrap's cards take it), white navbar with a `#E6E6E6` rule, grey tabs with a
  black active tab, cards with a 1 px `#E6E6E6` border and no shadow, small
  uppercase grey section labels, forest-green primary and light-grey
  secondary buttons, inputs on `#F3F4F3`, grey-green table headers, A/B
  differences in pale green `#EAF3EE`, summary boxes on `#F7F8F7`; no
  stripe. Tokens and chart colours live in
  `app/theme.R`, component CSS in `app/www/apter.css`, building blocks
  (cards, summary boxes, grid renderers,
  `apter_reactable()` (columns sized to content, headers wrap),
  `paged_table_ui()`/`paged_table_server()` (server-side pages for large
  result tables), the horizontal ggplot2 bars `apter_hbar()` /
  `apter_hbar_ab()` with `hbar_height()` (one row per assortment sorted by
  value, so every name shows with 17+ .hpr assortments), the ggplot2
  figures `apter_density_species()` (heights) and
  `apter_dbh_class_figure()` (trees and NOK/m³ by DBH class; A solid, B
  dashed), and the base-graphics stem diagram `apter_stem_compare()`
  (`logs_scenario = NULL`: Scenario A alone)) in `app/helpers_ui.R`;
  ggplot2 figures use `theme_apter()` (`app/theme.R`)
  and Inter from `app/www/fonts/*.ttf`, registered with systemfonts and drawn
  by ragg; `app/www/apter.js` shows the progress bar as soon as
  Run is clicked. **Result tabs (results restructure, 2026-10-23):
  Summary, Products, Trees, Logs** (`result_tabs` values `summary`,
  `products`, `trees`, `logs`); no comparison or Per m³ tab: comparison
  outputs sit in the tab of their content and render only when
  `compared()`. The layout is the same for NFI data, tree lists and .hpr
  stems (`is_hpr()`: no Plot column, "Stems", "Measured length (m)").
  - *Summary* (`summary_cards`): after a comparison run time, plots, trees,
    trees with logs (A / B) and "Trees with a changed cutting pattern"
    ("n of N", `compare_cutting_patterns()`) once, then Scenario A and B side
    by side, each a 2 × 2 grid (value, commercial volume / NOK per m³, price
    volume).
  - *Products*: `tbl_products` from `assortment_summary()`
    (`R/helpers_result_tables.R`, audit 27 check 10: sums of the selected
    logs; contribution = assortment value / the run's total commercial
    volume, so contributions add up to NOK/m³; share = its commercial volume
    / the total; NA = not in that scenario, shown "–"). A single run: Logs,
    Price volume, Value, NOK per m³ (contribution), Volume share. After a
    comparison value and contribution A, B and change, volume share A and B
    (decided 2026-10-23); `dl_products` has every column. `tbl_per_m3_type`:
    NOK/m³ and volume share by type (`compare_per_m3(by = "type")`, always by
    type). `bar_nok_contribution`: NOK/m³ by assortment (A and B; waste left
    out). After a comparison `cmp_value_ui`: `bar_cmp_delta`, the value
    change of the shared non-waste assortments, unchanged ones counted in
    `cmp_delta_note` ("No assortments in both scenarios." when none).
  - *Trees*: `fig_dbh_class`, `fig_height`, the trees table `tbl_trees`
    (`paged_table_ui/server`, all trees, 15 per page, sortable; after a
    comparison Value A, Value B, Change; logs and NOK/m³ are A's), and
    `stem_compare` for the clicked row (the first row until one is
    clicked). Downloads `dl_trees` (all trees, Scenario column),
    `dl_dbh_class`, `dl_changed_trees` (after a comparison).
  - *Logs* (renamed from Selected logs): the tally and the logs preview,
    as below.
  Removed 2026-10-23: the Difference (B − A) table, the cutting-pattern
  changes table and tree picker, the old Per m³ tables and share figure,
  the DBH table (its CSV stays), the Trees card and the selected tree's
  log table, the Products value figure and the type rows.
  **Result tables (2026-10-22, `R/helpers_result_tables.R`, audit 27):** the log tally
  (stocknota, Logs tab: `tally_key`, `tally_measure`, `tally_scenario`,
  `tbl_tally`, `dl_tally_xlsx`, `dl_tally_csv`) counts commercial logs (not
  waste) per `PriceDiameterClass_cm_used` × `LengthClass_dm` on the
  scenario's own price grid (`pm_to_grid()`: NA = no price, 0 = no logs;
  logs above the largest class in it), flat-priced per length, volume =
  `commercial_volume_m3` (grand total = the run's commercial volume); B − A
  on the union of axes, per length when an assortment is matrix-priced in one
  scenario and flat in the other. Caption: "Price-diameter class (cm) vs
  length class (dm)" (flat: "Length class (dm)"). **Different assortments
  (2026-10-22):** each run stores both scenarios' keys (`result()$keys`,
  from the run's `extra`); B − A per assortment only for keys both have
  (shared, whatever their rules or prices). The tally's Scenario radio
  offers the scenarios that have the selected key (B − A only when shared;
  dropdown "(A only)" / "(B only)"), and the xlsx has B − A only for shared
  keys (`log_tally_xlsx(scenario_keys =)`). Type groups (`type_group()`,
  `compare_by_type()`, `compare_per_m3(by = "type")` in
  `R/helpers_comparison.R`: Sawlog, Pulpwood, Other = custom commercial
  types; waste left out). Value by DBH class (`dbh_class_summary()`): 5 cm
  classes from 0, open from 60 cm; totals per species equal
  `per_m3_summary`. The Logs tab also has a preview (Scenario A's first 20
  logs) plus `dl_logs`, a CSV of all logs with a Scenario column (both
  scenarios after a comparison). The Summary's run time is
  the full time from clicking Run to the results (`elapsed_total_seconds`;
  the backend's `elapsed_seconds` counts only its own work). The SmartForest
  logo (`app/www/smartforest_logo.jpg`, `navbar_logo()`) sits at the right of
  the navbar, exactly as tall as the optBuck icon (26 px, also set as the
  img height attribute so it never shows at its natural 2560 px).
  `asset_url()` adds a version tag to `apter.css`/`apter.js` so browsers do
  not keep stale copies. The sidebar says "Data source" (input id
  `tree_source` unchanged). Forest green is the only strong colour. Two
  tabs, Assortments and prices (first) and Simulator; no Home, footer, Run
  details/steps, Diagnostics or explanatory notes (keep real errors and
  warnings only). No sliders, with one intended exception: the altitude
  range slider in the Simulator sidebar (styled in `apter.css`). Use those
  instead of inline `style =` colours, and never copy Oracle/ForestSens
  assets. Fonts are self-hosted (no Google Fonts). Output IDs are what
  audit 17 drives; keep them stable.
- **Project root:** no hardcoded paths. Order is `APTER_ROOT` (and option
  `apter.root` in `get_root_dir()`) -> fallback (app: parent of `app/`;
  `get_root_dir()`, backend, scripts and audits: walk up from `getwd()` to
  the folder containing `R/helpers_paths.R`) -> `stop()`. The legacy
  `NFIBUCK_ROOT` / `nfibuck.root` are no longer read. The app exports the
  resolved root as `APTER_ROOT` so the backend agrees. Scripts that run
  before `R/helpers_paths.R` is sourced carry an inline copy of this logic.
- The backend runs the vectorised DP `.dp_select()` (`R/helpers_bucking_core.R`;
  `.optimize_dp_fast()` is its data.frame wrapper), not the reference
  `optimize_one_tree_dp()`; audit 16 checks 14-15 and audit 19 assert they
  agree exactly, including tie-breaking (audit 16 also compares the backend's
  own per-tree result).
- **Performance (2026-10-13):**
  - **Bucking core.** `R/helpers_bucking_core.R` is the per-tree core of both runners: candidates (`.candidate_columns(usable_only = TRUE)`), pricing (`.price_vectors()`, the engine of `lookup_prices()`) and the DP, all on plain vectors.
  - **Comparisons.** The runners take `scenario =`. Both runs are priced from one candidate pass for the union of their products, in memory, and the scenario comes back as `$scenario`. There is no cache between runs (decided 2026-10-13).
  - **Jobs.** Runners are `prepare_*_job()` → `buck_job()` → `finish_job()`. `R/helpers_parallel.R` splits trees into chunks on `future` multisession workers (`apter.workers`, default cores − 1; `apter.chunk_size`).
  - **App.** It runs jobs as `ExtendedTask`s (`app/helpers_jobs.R`). Its own future queue polls with `later`; don't use `promises::future_promise()`, which breaks with future ≥ 1.40.
  - **Rule: results must not depend on the number of workers or on chunking** (identical, not just equal; audit 22). Trees are bucked in `treid` order; per-tree code must stay free of RNG and global state.
  - **Multi-user servers.** The cores − 1 default assumes one R process. On a server with several R processes, set `apter.workers` so that processes × workers ≤ cores.
  - **Main baseline.** `outputs/performance/baseline_main_3fa0a9d.rds` holds main's results before the phase; never regenerate it.
  - **NFI runs never call taperNOR** (checked 2026-10-15): all 93,607 bucked trees of HKL 3–5 have a precomputed profile in `outputs/app_cache/`, and a missing profile stops the run. Default-stratum comparison (spruce, HKL 4, bonitet 11/14/17, all counties; 12,427 trees, 15 workers): loading 5.8 s (cache 2.7, profile conversion 1.7, bark 0.8), bucking 26.9 s, result tables 1.3 s. The app's former ~2 min came from `compare_cutting_patterns()` (61.7 s, a per-tree loop); vectorised to 0.8 s with identical output.
- **`or_default(x, default)`** (`R/helpers_utils.R`, the single definition)
  is Apter's "value or default" rule: `default` when `x` is NULL or a single
  NA. Use it everywhere; never define or rely on `%||%` (base R's, R >= 4.4,
  only replaces NULL, and helpers used to define their own conditionally,
  so the app and the audits ran different versions). Every helper that
  needs it sources `helpers_utils.R` when `or_default` is missing; the
  backend sources it first and the workers' core environment includes it.
- **Price scenarios (Phase 4a):** `R/helpers_price_matrix.R` (grids in the
  source layout: diameter rows × length columns; diff, reset, upload with
  automatic long/wide detection, validation, templates, export) and
  `R/helpers_comparison.R` (pattern changes, per-m³ comparison). Prices are
  looked up through one function, `lookup_prices()` /
  `price_logs()` in `helpers_valuation.R`, for runs and re-pricing alike.
  The editor lives on the Assortments and prices tab (`rhandsontable`).
  The grid is drawn at full height (34 + 24 px per row, no inner scrolling:
  the former 480 px box hid rows added at the end, 2026-10-22); "+ larger"
  stops at the assortment's max top diameter (`pm_add_class(max_top_cm =)`:
  no class above floor(max top), it could never be used); under the grid
  `pm_<s>_cap` states the cap rule ("≥ 36 cm: priced as 36 up to max top 60
  cm"; without a max top "≥ 50 cm: priced as 50"; none when the largest
  class reaches the max top). Assortment labels in the app (`key_label()`,
  the results' `asm_label()`) come from the assortment's name with only its
  first letter lowercased ("Spruce REDACTED25"), the key only as a
  fallback.
- **Scenarios A and B (Phase 4c/4d):** `scenario_server()` holds two
  independent scenarios, `sc$A` and `sc$B`, each list(`defs`, `pm`)
  (assortment table, `ASSORTMENT_TABLE_COLS`, cm; prices). Every action is
  written once for a side and its `other()`. `commit()` validates a scenario
  (`check_price_set()`) before storing it.
  `products_from_defs()` builds each run's ProductData/LengthClasses, which
  the app passes to the runners (`products =`, `length_classes =`); B goes as
  `scenario =`. The backend needs no change for B's own rules: a key defined
  differently in A and B gets its own candidate pass (`.prepare_runs()`). With the default table and 2021 prices A runs through the
  default price file and products (audit 23 check 10: identical to the
  explicit path). `check_price_set()` validates a set before a run. Never test a product's kind by its key name (no
  `grepl("sawlog")`): matrix vs. flat comes from matrix rows / `PriceType`,
  species products from `species_products()`.
- **Tree lists (Phase 5):** `R/helpers_tree_list.R` (`read_tree_list()`,
  `prepare_tree_list()`: validation, heights, taperNOR range) and
  `run_tree_list_bucking()` in the backend, which shares the per-tree core
  (`buck_job()` / `finish_job()`, `R/helpers_bucking_core.R`) with
  `run_sampled_plot_bucking()` and runs taperNOR at run time (in chunks on
  the workers). The tree key is `plot_id/tree_id` (or `tree_id`); the user's own
  plot ID goes in the `plot_id_github` column so the comparison and app code
  stay shared (it is the user's data, not an NFI plot ID). Stem-profile ends:
  candidates never end past the profile (heights that are not whole dm).
- **hpr import (Phase A):** `R/helpers_hpr.R` (sourced by the backend):
  `read_hpr()` → `hpr_product_table()` (the dialog's rows and default ticks)
  → `hpr_to_scenario()` (defs + prices, `check_price_set()`). The dialog is in
  `scenario_server()` (`hpr_import_a/b`, `hpr_file`, `hpr_sp_<key>`,
  `hpr_pr_<key>`, `hpr_apply`) and ends in `commit()`, like a scenario upload.
  Applying it moves the file to the session's `imported/` folder and sets
  `hpr_imported` (name, path, side, confirmed map), returned by
  `scenario_server()`.
- **hpr stems (Phase B):** `R/helpers_hpr_stems.R`: `read_hpr_stems()`
  (streaming) → `hpr_stem_checks()` (drop reasons and messages, cheap; the
  app calls it as the map changes) → `hpr_stem_profiles()` → backend
  `prepare_hpr_job()` / `run_hpr_bucking()` (`.new_job("hpr", stump_cm = 0)`;
  `.result_tree_list()` serves both kinds). The app reads the stems on a
  worker (`read_hpr_stems_async()`, an ExtendedTask) and prepares the job on
  a worker (`prepare_hpr_stems_job()`); panel outputs `hs_file_ui`,
  `hs_species_ui` (`hs_sp_<read>_<key>`), `hs_validation`, input `hs_upload`.
- **fieldPlot is a dependency, not vendored.** Use only its exported
  `fitHD_Plot()` / `fitHD()` through `impute_heights()`. Don't call
  `predictMissingHeights()`: it always fits species codes 1–3 and errors
  when one is absent (confirmed 2026-10-13; reported to Lennart with the
  timeout issue).
- **Default-results baseline:** `outputs/assortments/baseline_phase4b_default.rds`
  (from `scripts/phase4b_save_default_baseline.R`, taken on e8b1f05) is what
  audit 20 check 8 compares against; never regenerate it on later code.
  Backend row order varies between runs (arrow reads the taper cache in no
  fixed order), so compare results after sorting.
  `audits/audit_17_app_smoke.R` drives the app headlessly (shinytest2).
- The app only reads `data_processed_github/` (sanitized, committed, no
  Oracle needed). `data_processed/` and Oracle access are only needed to
  *regenerate* that sanitized data via the frozen
  `scripts/prototype_reference/00_prepare_nfi_2024_data.R` and
  `scripts/98_create_github_safe_processed_data.R`. Script 00 hardcodes the
  old nfiBuck root; run it only through
  `scripts/run_prototype_00_prepare_nfi_data.R`, which substitutes the root
  in memory without touching the frozen file.
- `scripts/prototype_reference/` and `audits/prototype_reference/` are a
  frozen, matched pair — the original validated prototype scripts (00–15)
  and their audits (05–12). `audit_14`'s checks 19–20 assert these scripts
  still exist and are unmodified; don't delete or edit them.
- See [docs/development.md](docs/development.md) for the full file layout and
  [docs/plans/PLAN.md](docs/plans/PLAN.md) for the development roadmap.
