# Phase 0 (housekeeping): plan

## Context

`PLAN.md` Phase 0 calls for four housekeeping steps: triage unfinished items, remove the hardcoded root path, split `app/app.R`, and add a headless app smoke test. Do this before the rename (Phase 1) and the feature phases.

**New blocker found while planning:** the project folder was renamed `optBuck\nfiBuck` → `optBuck\Apter` during this session. These files still hardcode `S:/.../optBuck/nfiBuck`, so they are **currently broken**:

- `app/app.R:16`
- `R/helpers_paths.R:33`
- `scripts/app_backend/00,01,02`
- `audits/audit_14/15/16`

The app cannot start, and audits 14–16 cannot be re-run at the end of the phase. Step 2 below therefore has to cover more than `app/app.R`, otherwise the phase cannot be verified. The full rename sweep (97/98/99, `local_only/`, text, labels) stays in Phase 1.

Also, this Claude session's working directory no longer exists, so shell commands fail. **Implementation must happen in a new session started from `S:\Prosjekter\52106_SFI_SmartForest\optBuck\Apter`.**

---

## Step 1: Triage list (report only, no action)

This is the finding. It is presented to Lennart for triage and goes into the end-of-phase summary.

**TODO/FIXME/XXX comments.** Only 3 exist, all in frozen prototype scripts, which must not be edited:

- `scripts/prototype_reference/03_define_products_and_volume.R:553`: TODO saying the top-measured cylinder volume is simplified and must be revisited "before production use". This relates to the Phase 3 denominator.
- `scripts/prototype_reference/04_import_or_define_price_matrices.R:389`: flat prices are "temporary placeholders".
- `scripts/prototype_reference/10_run_mixed_species_example_bucking.R:906`: placeholder arithmetic (`* 0`).

**Unfinished or inconsistent items in live code:**

1. `app/app.R:254`: `uiOutput("lc_filter_note")` has no server renderer. It is a dead output.
2. `app/app.R:694`: the histogram title contains mojibake (`???`, originally an em dash).
3. Mojibake also appears in `scripts/98_create_github_safe_processed_data.R` comments and output at lines 75, 96, 97 and 155.
4. README "Running the app" claims the app honours `NFIBUCK_ROOT`. It does not, because `app.R` hardcodes the path.
5. The README "Current status" table is dated 2026-08-03 and still lists scenario multipliers. "Recommended next steps" covers ha-expansion, which is out of scope per PLAN.md.
6. Audit 14's REVIEW item (zero-priced waste) is accepted per PLAN.md but not yet recorded in the audit output or docs.
7. Audit 16 checks 09–10 assert per-ha output and the "Not ha-expanded" label. Both are slated for removal in Phase 3.
8. `scripts/local_only/export_plot_coordinates_rounded.R` exports 1 km-rounded plot coordinates to `C:/nfibuck_private/`. Its header says this is an authorised presentation exception, but CLAUDE.md says "No exact plot coordinates exist anywhere in this pipeline" and does not mention it. CLAUDE.md should probably note the exception.
9. `.claude/settings.local.json:12` contains an allow-rule with the old `nfiBuck` path.
10. `data_raw/README.md` is a placeholder only. This is expected.

**`02_benchmark_dp_optimizer_alternatives.R`: concluded.**

- v3 (pre-split by StartPos with a vectorised inner loop) won.
- It was integrated as `.optimize_dp_fast()` in `scripts/app_backend/01_run_sampled_plot_bucking.R:115`. A comment there records "10x speedup … identical results".
- **The app uses v3**, through `run_sampled_plot_bucking()` (line 572). The reference `optimize_one_tree_dp()` in `R/helpers_dp_optimizer.R` is unchanged and used only by audits.
- Loose ends:
  - The benchmark's numeric output was never saved to `outputs/`.
  - No audit asserts that `.optimize_dp_fast()` gives the same result as `optimize_one_tree_dp()`. Audit 15 tests only the reference, and audit 16 tests only overlaps.
  - The script's "consider Rcpp" branch was never pursued. This may matter for the Performance section.
  - The v2/v3 code is duplicated between script 02 and backend 01.

---

## Step 2: Root-path resolution (no hardcoded paths)

The resolution order everywhere is: `APTER_ROOT` → `NFIBUCK_ROOT` (legacy, read only so Lennart's existing `.Renviron` keeps working until the Phase 1 checklist) → a context-specific fallback → `stop()` with a clear message. No hardcoded `S:/` path remains.

- **`app/app.R`:** fallback is the app directory's parent (`normalizePath("..")`, since `runApp("app")` sets the working directory to `app/`). Validate that `R/helpers_paths.R` exists under the resolved root. Then call `Sys.setenv(APTER_ROOT = ROOT)` so the backend resolves the same root.
- **`R/helpers_paths.R` `get_root_dir()`:**
  - Order: `APTER_ROOT` → `NFIBUCK_ROOT` → option `apter.root`, with `nfibuck.root` kept as a legacy option.
  - Fallback: walk up from `getwd()` to the first directory that contains `R/helpers_paths.R`.
  - Otherwise `stop()`.
  - Update the roxygen header to match.
- **`scripts/app_backend/01_run_sampled_plot_bucking.R:42`:** `.BACKEND_ROOT` uses the same env vars, then falls back to walking up from `getwd()`. It is sourced before `helpers_paths.R`, so it gets a small inline copy of the walk-up.
- **`scripts/app_backend/00_…`, `02_…` and `audits/audit_14/15/16`:** replace `root_dir <- "S:/…"` with the same pattern: env var, else walk-up, else stop. These are needed so the audits can run. Audit 14 checks 19–20 use mtimes of prototype scripts only, so they are unaffected.
- Not touched: `scripts/97/98/99`, `scripts/local_only/` and all frozen prototype files. They go to Phase 1.

---

## Step 3: Split `app/app.R`

Target layout. All files are sourced explicitly from `app.R`, with no reliance on Shiny's `R/` autoload or `global.R` magic:

| File | Contents (moved verbatim from current `app.R`) |
|---|---|
| `app/app.R` | `library()` calls, root resolution (step 2), source the backend, source the files below in order, `shinyApp(ui, server)` |
| `app/setup.R` | data loading (`plots_all`, `plots_prod`, `APP_PRODUCTS`, `APP_LC`, `APP_PRICE_LONG`), `HKL_*`, `BONITET_LABELS`, `.FYLNR_NAMES`, `FYLNR_CHOICES`, `ESTIMATE_LABEL`, `APP_CSS` |
| `app/helpers_validation.R` | Pure functions extracted from repeated server code: `build_run_params(input_list)` (used in 2 places today: `result` and `result_s2`) and `count_eligible_plots(plots_prod, hkl_raw, abon, fyl)` (used by `abonitet_ui`/`stratum_preview`). |
| `app/helpers_upload.R` | `.make_s2_pm()` and a new `resolve_s2_price_path(mode, upload, mults, base_pm)` that wraps the current `switch(s2_mode, …)`, including tempfile writing. |
| `app/ui.R` | `ui <- fluidPage(…)`, unchanged |
| `app/server.R` | `server <- function(input, output, session)`, calling the helpers above |

Rules for the split:

- **Behaviour-preserving:** no UI or text changes. That includes keeping the `???` and dead `lc_filter_note` as they are; they are triage items.
- `shiny::runApp("app")` from the project root keeps working, and so does `runApp("app/app.R")`.
- Update the header comment in `app/app.R` with run instructions.
- Update the CLAUDE.md "Architecture notes" bullet to describe the new file layout and root resolution.
- Update the README "Repository contents" and "Running the app" sections. These are factual updates only; the scope/rename edits wait for their phases.

---

## Step 4: Headless smoke audit: `audits/audit_17_app_smoke.R`

This follows the audit-16 pattern: `helpers_audit.R` (`init_audit_log`, `record_check`, `write_audit_outputs`), PASS/REVIEW/FAIL, and outputs in `outputs/app_smoke/audit_17_app_smoke.{csv,txt}`.

**Checks:**

1. `shinytest2` and `chromote` are installed and a Chrome is found. If not, record FAIL with an install hint.
2. The split files exist: `app.R`, `ui.R`, `server.R`, `setup.R`, `helpers_*.R`.
3. **Root resolution, without a browser:**
   - With `APTER_ROOT` unset, sourcing the resolution from `app/` gives the project root.
   - With `APTER_ROOT` set to a temp dir, resolution fails with a clear error.
   - With `APTER_ROOT` set to the real root, it resolves.
4. **Pure helpers, without a browser:**
   - `.make_s2_pm()` applies the sawlog, pulp and waste multipliers correctly.
   - `build_run_params()` maps HKL "4" to `c(41L, 42L)` and `"__all__"` to `NULL`.
   - `count_eligible_plots()` matches a direct filter of the data.
5. `AppDriver$new("app", load_timeout = 60s)` starts without error.
6. The initial `run_status` shows the idle text, and the title contains "nfiBuck". The title check gets updated in Phase 1.
7. **Main run:**
   - Inputs: spruce, HKL 4, abonitet 17, 5 plots, seed 42.
   - Click `run_btn` and wait for idle with a 120 s timeout.
   - `run_status` contains "Run complete", and no error notification is present.
8. `summary_cards` is rendered and non-empty, and `tbl_products` has at least one row.
9. **Scenario comparison:**
   - Set `compare_on = TRUE` with mode "same", then run.
   - `cmp_status` contains "Same plots sampled".
10. **Multiplier mode:** sawlog multiplier 1.2, then run. `tbl_cmp_delta` renders.
11. Runtime under 180 s in total. If above, record REVIEW.

`shinytest2` snapshot files are not used. Assertions are explicit, so there are no brittle snapshot directories to maintain.

---

## Step 5 (added by Lennart): Optimizer equivalence check, in audit 16

The app runs `.optimize_dp_fast()` (`scripts/app_backend/01_run_sampled_plot_bucking.R:115`). No audit currently checks it against the reference `optimize_one_tree_dp()` (`R/helpers_dp_optimizer.R`).

**Approach:**

- Add the check to `audits/audit_16_sampled_plot_backend_smoke.R` as new checks 14–15. The backend file itself is not changed.
- After the existing smoke run, so that check 13's runtime is unaffected, wrap the function in global env:
  ```r
  .fast_impl <- .optimize_dp_fast
  .optimize_dp_fast <- function(vc) {
    f <- .fast_impl(vc); r <- optimize_one_tree_dp(vc)
    <append comparison row to E_eq>; f
  }
  ```
  `run_sampled_plot_bucking()` resolves `.optimize_dp_fast` from global env, so every tree's real priced `dp_in` gets compared. No candidate-building code is duplicated, unlike benchmark script 02.
- **Coverage:** run the backend with the shim on three settings:
  - spruce, HKL 41/42, abonitet 14/17, 25 plots
  - pine, the same strata, 25 plots
  - `species_group = "both"`, 10 plots, a different seed
- Restore the original function afterwards.
- **Check 14:** `dp_best_value` is identical within 1e-8 for every tree, and `n_selected` matches. Result is FAIL on any mismatch, and the detail lists the number of trees compared.
- **Check 15:** the cutting pattern matches, meaning the set of (StartPos, StopPos, ProductKey). If values are equal but the pattern differs (an equal-value tie), record REVIEW and list the count. Any other difference is FAIL.
- Update audit 16's header check list and verdict text.

## Step 6 (added by Lennart): Record the accepted zero-price waste item, in audit 14 and docs

- **`audits/audit_14_validate_first_helpers.R:336–343` (check 18):** when every REVIEW item from `validate_price_matrix_against_products()` is a zero-price item, and every zero-price row in the price matrix belongs to a waste ProductKey, record **PASS**. The detail reads: "zero-priced waste accepted as intentional (scope decision 2026-10-12)".
  - A zero price on a non-waste product, or any other REVIEW reason, still gives REVIEW.
  - Update the header comment (check 18 text, verdict text and a dated note).
  - Expected audit 14 verdict becomes **PASS**.
- **CLAUDE.md "Accepted assumptions":** add a row "Waste price | 0 NOK/m³, intentional (accepted 2026-10-12) | price matrix; audit 14 check 18".
- **README "Current status":** update the audit 14 line and drop the "may be intentional" sentence.

## Step 7 (added by Lennart): Coordinate exception in CLAUDE.md

In the CLAUDE.md "Data privacy" section, keep the existing rule and add an "Authorised exception" paragraph:

- `scripts/local_only/export_plot_coordinates_rounded.R` exports plot coordinates rounded to 1 km to `C:/nfibuck_private/`, outside the repo. It was authorised by the project owner for the 2026-10 status presentation only.
- It requires Oracle access, writes nothing into the repository, and output rows carry no plot IDs.
- It is not used by the app or the pipeline, and it does not make coordinates available for features. A map feature is still out of bounds.

I will also soften the absolute first sentence ("No exact plot coordinates exist anywhere…") so that it stays true: no exact coordinates exist in the pipeline or the repository.

---

## Verification (end of phase)

From a new session in `…\Apter`:

1. `Rscript -e 'shiny::runApp("app", launch.browser=FALSE)'` launches, then stop it. The same works with `APTER_ROOT` set and unset.
2. Run audits 14, 15, 16 and the new 17 (`Rscript audits/audit_1X_….R`). Expected verdicts:
   - 14: **PASS**, with the zero-price waste recorded as accepted (step 6).
   - 15: PASS 23/23.
   - 16: PASS, including new checks 14–15. A REVIEW on check 15 is acceptable only for equal-value ties.
   - 17: PASS.
3. Grep for `optBuck/nfiBuck` in non-frozen, non-output files. Only Phase 1 leftovers should remain: `97/98/99`, `local_only`, `.claude/settings.local.json`.
4. `git diff --stat`: no changes under `scripts/prototype_reference/` or `audits/prototype_reference/`.
5. Present the step 1 triage list to Lennart.

## Open points for Lennart (non-blocking)

- Is `shinytest2` and `chromote` (and Chrome) installed on the remote desktop? If not, audit 17's browser checks will FAIL until they are installed; the non-browser checks still run.
- Keep the `NFIBUCK_ROOT` legacy fallback until Phase 1? This is assumed yes.
