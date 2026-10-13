# Apter development plan

Updated 2026-10-13. Replaces the previous PLAN.md and supersedes docs/plans/phase0.md (and any other files in docs/plans/). Detailed conventions live in CLAUDE.md; developer details, audits and benchmarks in docs/development.md.

Working rules: one phase per session, plan mode first, show the plan to Lennart before editing, work on a branch off main, re-run all audits at the end of a phase, and don't merge or push until Lennart says so. Never edit `scripts/prototype_reference/` or `audits/prototype_reference/`. When a convention is undecided, follow optBuck (SmartForest-no/optBuck) unless it conflicts with a decision in this file or CLAUDE.md.

## Scope

- Apter is a general bucking simulator for Norway spruce and Scots pine, for industry partners: compare and improve price matrices and plan bucking. It is not an inventory or estimation tool, and NFI is not emphasised.
- Tree sources: NFI sample trees, uploaded tree lists, and (Phase 6) uploaded stem profiles. Results describe the selected trees only. No per-ha, plot-level or stratum outputs.
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

## Phase 4c: interface redesign

Results for identical inputs must stay identical to main, except for the intended change from the new diameter limits (3a). Report that change against the performance baseline.

### 1. Look (palette "Forest & sand")

- Forest green `#3F6E58` is the only strong colour: logo, primary buttons, active tab, status icons. Secondary buttons `#EAE5DC`. Neutrals unchanged (page `#F9F8F6`, text `#161513`, muted `#6F6A66`, lines `#E4E1DD`).
- Assortment chart colours: sawlog `#3F6E58`, pulpwood `#C9A97A`, waste `#A39E98`. Custom assortments stay grey with their name on the log. Re-run the colour-blindness check via chromote; adjust shades only if needed.
- Logo: remove the semicircle at the bottom. Keep the current tapered stem (broader at the base than at the top) and the cut marks.
- Stripe: plain diagonal bands without dots, in `#8A8580`, `#D9CDB8`, `#C2B59F`, `#E8E0D2`, `#3F6E58`.
- Remove the release footer (VERSION may stay internal).

### 2. Navigation

- Remove the Home page, including the tree-source tiles and AVAILABLE/PLANNED labels.
- Two tabs, in this order: **Assortments and prices** (opens first) and **Simulator**.

### 3. Assortments and prices

**3a. Assortments** (replaces "Product definitions" and "Custom assortments")

- One table for all assortments. Base products (spruce/pine sawlog, pulpwood, waste) are edited and removed exactly like custom ones, through one form in the app; "Reset to defaults" restores the base set.
- Visible columns: Assortment (one name), Type, Species, Min/Max top diameter (cm), Max butt diameter (cm), Min/Max length (cm), Length step (cm), Volume method, Diameter rounding. "No limit" is shown as "–", never "Infinity".
- Hidden (kept internally where needed): ProductKey (generated from name and species), species_group, DiameterSource, VolumeLengthCategory (no effect in Apter), DiameterUnderBark (always TRUE), DiameterTopPosition (set in the form only), Priority.
- Priority is needed by the tie rule. Derive it automatically: matrix-priced 1, flat-priced commercial 2, waste 99.
- AllowAsFirstLog / AllowAsTopLog: remove from code and data if always TRUE and unused; otherwise keep hidden and report.
- Default diameter limits under bark, from optBuck's example.hpr (`DiameterClassMAX`, `DiameterMAXButt`):

  | Assortment | Max top (cm) | Max butt (cm) |
  |---|---|---|
  | Spruce sawlog | 60 | 65 |
  | Pine sawlog | 50 | 60 |
  | Spruce and pine pulpwood | 70 | 70 |
  | Waste | – | – |

  Minimum top diameters stay as they are. Report how many logs and how much NOK/m³ change against the baseline.
- Remove the explanatory note.

**3b. Length and diameter classes**

- Remove the section, including the 31 dm interpolation note and the italic marking of interpolated cells (the data stay).
- Sawlog lengths and diameter classes come from the columns and rows of each assortment's price matrix (single source). Flat-priced assortments use min/max length and length step. LengthClasses is derived from these, never edited separately.

**3c. Price matrices**

- Heading "Price matrices". No explanatory notes.
- Two columns, **A** and **B**, both editable. A starts from the 2021 prices; each side has "Reset to 2021 prices", "Upload price matrix" (xlsx/csv, wide or long, as now) and download.
- Each assortment in B is "Same as A" (default, shown greyed), "Own prices" or "Not used". Removing an assortment (3a) removes it from both sides.
- Rows (top-diameter classes) and columns (lengths) can be added and removed. A removed column means that length is not allowed. Rows can only be removed at the ends, so there are no gaps.
- Minimal axis label on the grids: "Top diameter (cm) × length (dm), NOK/m³ under bark".
- Cells in B that differ from A stay highlighted, without a text note.

**3d.** Remove "Notes and limitations".

### 4. Simulator

Sidebar:
- Remove the random seed input (fixed at 42, hidden) and "Plots to sample" (fixed at 200; strata with fewer plots use all of them).
- Remove the comparison checkbox and scenario lines. A comparison runs automatically when B differs from A; otherwise a single run with A.
- Remove the notes under Run (eligible plots, "Selection OK", HKL 1–2, county filtering). Keep real validation errors and warnings (tree lists, height warnings).

Results area (right side shows results only):
- Remove the Run details and Run steps cards. Keep a progress indicator while running.
- Remove all trees-only and bark notes.
- One results card with tabs: Summary, Per m³, Products, Selected logs, Price matrix comparison (shown only when B differs from A).
- Summary, without repetition or notes: run time, plots, trees, trees with logs, logs, commercial volume (m³ u.b.), total value (NOK), NOK per m³, price volume (m³).
- Remove the Diagnostics tab. Trees excluded from a tree list are listed in the validation panel.

### 5. Terminology

"Default" and "scenario" become **A** and **B** wherever a user sees them: UI, downloads, file names, README. Internal names may stay; rename only if cheap and safe, in a separate commit.

### 6. Audits and docs

- Update audit 17 for the new layout and add checks: no Home, A editable, B states, adding/removing rows and columns, automatic comparison when B differs, no "default"/"scenario" in the UI.
- All other audits pass; the only expected result change comes from the new diameter limits.
- Update README, CLAUDE.md and docs/development.md. Screenshots of both tabs for review before merging.

## Phase 6: stem profile import

- Long format (CSV or xlsx): `tree_id`, `species` (spruce/pine), `height` (m along the stem), `diameter` (cm, over bark), optional `plot_id`. Downloadable template. Reading StanForD .hpr files is out of scope.
- Profiles are over bark. Under-bark diameters with barkNOR (B ≥ 0 floor); dbh is read from the profile at 1.3 m.
- Interpolate to the 10 cm grid and feed the profiles straight into candidate generation, bypassing taperNOR.
- Non-monotonic profiles are valid: no warnings or errors for local diameter increases.
- Validation: errors for missing columns, non-numeric values, missing IDs, unknown species, non-positive values, duplicated heights within a tree. Warnings for likely unit errors (state the suspected unit; never convert silently), profiles with very few points or large height gaps, and profiles that don't reach 1.3 m.

## Later and open items

- **Sensitivity tool** (undecided): show how cutting patterns and NOK/m³ respond to ±x % changes in a price cell or assortment. A price-matrix optimiser would need an explicit objective (e.g. target volume shares), to be defined by Lennart first.
- **Negative pine bark in barkNOR:** the B ≥ 0 floor is provisional; ask the taperNOR authors whether the pine behaviour near the top is intended and whether Hansen et al. (2023) refitted the coefficients.
- **fieldPlot fixes** (Lennart's package, separate session): loop over species present; error handling around the fitHD() fallback; a working timeout; species curve for plots without heights; nlme:: calls; README install URL. Apter's wrapper works around these meanwhile.
- **Deployment** on the NIBIO server: set apter.workers so that R processes × workers ≤ cores.
