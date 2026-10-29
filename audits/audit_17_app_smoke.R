# ==============================================================================
# audits/audit_17_app_smoke.R
#
# Purpose : Headless smoke audit for the Shiny app (app/). Checks the split
#           file layout, root resolution and pure app helpers without a
#           browser, then drives the running app through shinytest2.
#           Assertions are explicit; no shinytest2 snapshot files are used.
#           Rewritten for the Phase 4c layout (2026-10-13): two tabs,
#           one assortment table, price sets A and B. Phase 4d (2026-10-15):
#           Scenarios A and B side by side, each with its own assortments
#           and prices; the app shows the name "optBuck"; NFI runs use all
#           plots; paged result tables.
#
# Checks  :
#  1.  shinytest2 and chromote are installed and a Chrome is found
#  2.  Split app files exist (app.R, app_ui.R, app_server.R,
#      server_scenarios.R, setup.R, theme.R, helpers_*.R, www/apter.css,
#      www/apter.js, www/apter-icon.svg) and app/
#      has no ui.R/server.R (Shiny would treat those as a second app format;
#      shinytest2 refuses to start)
#  3.  Root resolution (no browser):
#        a. APTER_ROOT unset              -> parent of app/ (project root)
#        b. APTER_ROOT = empty temp dir   -> clear error
#        c. APTER_ROOT = project root     -> project root
#  4.  Pure helpers (no browser):
#        a. write_price_file() + upload_price_set(): a long file of A x1.2
#           uploads back unchanged into A; into B it makes every product
#           "own" and B differs from A
#        b. build_run_params(): HKL "4" -> c(41L, 42L), "__all__" -> NULL,
#           all plots (APTER_N_PLOTS instead samples that many, seed 42)
#        c. count_eligible_plots() matches a direct filter of plots_prod
#        d. the default assortment table gives the base ProductData
#        e. live tree counts (layout-and-figures): select_nfi_trees() gives
#           exactly the trees and plots prepare_sampled_job() bucks (all
#           plots and a 5-plot sample, both species); tree_counts() text
#        f. altitude filter: the full slider range gives the same params and
#           trees as no slider; a narrowed band (200-400 m) gives fewer plots
#           and trees, all inside it, and the run bucks the same trees;
#           count_eligible_plots() matches a direct filter; a plot with
#           missing altitude (synthetic) counts at the full range only
#  5.  AppDriver$new("app") starts without error (load timeout 60 s)
#  6.  Layout: two tabs, "Assortments and prices" first and open, then
#      "Simulator"; no Home, tree-source tiles, AVAILABLE/PLANNED labels or
#      release footer; no striped bar (removed in the restyle, option 3
#      "Combination"); the look's main colours, read from the browser: light grey
#      page (#F3F4F3) behind white cards and sidebar, white navbar with a #E6E6E6 rule, grey tabs with a black active
#      tab, borderless shadowless cards with a #E6E6E6 border, green primary
#      and grey secondary buttons, uppercase grey section labels; the window title and the
#      navbar say "optBuck"; the intro line above the scenarios and the
#      SmartForest logo in the navbar are shown; the logo, measured in
#      headless Chrome, is as tall as the optBuck icon (26 px), not taller
#      than the navbar, on one line with the brand and the tabs at 1920 px,
#      and the navbar stays one bar at 700 px (screenshots of the navbar at
#      both widths); "Data source", not "Tree source"; "NFI data"; no "2021" (as a number on its own;
#      grid headers run together) or "(wide or long)"
#  7.  Theme: Bootstrap 5 loaded, self-hosted Inter font loaded, no Google
#      Fonts request
#  8.  Scenario A's compact assortment table: six base rows, columns
#      Assortment, Species, Top Ø, Max butt Ø, Length, Volume method (no
#      Type, Length step or Diameter rounding); no limit shown as "–" or
#      "≥", never "Inf"/"Infinity"; columns cannot be sorted by clicking;
#      one copy button per side ("Make assortment equal to B/A"); no
#      "Make matrix equal" or whole-scenario buttons, no assortment or
#      matrix dropdowns
#  9.  Independent scenarios (Phase 4d): both grids editable (no read-only
#      cells), nothing greyed, no highlighted differences at the start, no
#      "Same as A" dropdown and no "B equals/differs" notes
# 10.  Spruce pulpwood (selected by a row click) edited to a price matrix
#      in B only: B's detail panel shows a matrix grid, A's the flat price
#      field, and the Pricing cell is highlighted on both sides; "Make
#      assortment equal to A" in B restores it (no highlights)
# 11.  Rows and columns of A (spruce sawlog, from the downloaded A): a
#      larger and a smaller class added and removed at the ends, a length
#      (58 dm) added and removed; the download matches each step; the grid
#      is at full height (34 + 24 px per row, no inner scrolling) and the
#      line under it reads "≥ 50 cm: priced as 50 up to max top 60 cm"
#      (then 51 after "+ larger")
# 12.  Run with B = A: one run, the result tabs are Summary, Products,
#      Trees, Logs (results restructure, 2026-10-23; no comparison tab), no
#      error notification;
#      Run is disabled with progress while the task runs (REVIEW if the run
#      finished before the driver looked)
# 13.  Summary shows the nine fields (run time, plots, trees, trees with
#      logs, logs, commercial volume, total value, NOK per m3, price
#      volume); no Diagnostics tab
# 14.  Logs tab: a preview of Scenario A's first 20 logs (one-line
#      label naming Scenario A, no sorting or paging controls); readable
#      headers (Plot, StartPos, StopPos; no ProductType, plot_id_github or
#      underscores); "Download all logs (CSV)" holds every log with a
#      Scenario column (A)
# 15.  Trees tab (2026-10-23): the distribution figure (DBH and height panels) and NOK per m³ by DBH class render; the
#      trees table shows all trees on server-side pages (15 rows, "of N"),
#      no Status column, one Value column; clicking a row draws the cutting
#      pattern (Scenario A only); "Download all trees (CSV)" holds every
#      tree with a Scenario column (A); the DBH CSV's totals are the run's
#      trees; the removed outputs (old Per m3 tables, Trees card, comparison
#      tab, value figure) are gone; no "per ha" / "ha-expanded" text.
#      Products tab: the merged table with Logs, Price volume, Value, NOK
#      per m3 (contribution) and Volume share, the type table, the
#      contribution figure, no value change
# 16.  Automatic comparison: B's price matrix uploaded (spruce sawlog x1.2,
#      wide CSV) -> differing cells highlighted in both grids, 351 cells
#      differ in B's download; Run -> a comparison with the same four tabs
#      (no comparison tab); the Summary has "Changed cutting patterns" (a
#      count; every card label on one line, cards of one height); Trees: Value A, Value B and Change columns,
#      sorted by change largest first, the row click draws A and B, the
#      changed-trees CSV; Products: value and contribution A, B and change,
#      volume share A and B, the value-change figure, the products CSV with
#      logs and price volume; no error notification;
#      the Summary shows the run once and Scenario A and B side by side,
#      each as a 2 x 2 grid (value, commercial volume / NOK per m3, price
#      volume); the logs and trees CSVs hold both scenarios (Scenario A and
#      B; the trees CSV has every tree once per scenario)
# 17.  Spruce pulpwood removed from B only: the comparison run's B has no
#      spruce pulpwood; B's table has no row for it and, with it selected
#      in A, B shows no detail panel; no struck-through rows anywhere.
#      Different assortments (2026-10-22; tabs 2026-10-23): Products lists
#      spruce pulpwood with "–" for B (value and change), no "(type)" rows,
#      the type table by Sawlog and Pulpwood, the value-change figure of
#      the shared assortments, the contribution figure and no value or
#      volume figure; the tally offers only A for spruce pulpwood ("(A only)") and
#      A, B, B − A for spruce sawlog; the tally caption is "Price-diameter
#      class (cm) vs length class (dm)" (screenshots 15-18: Products, its
#      value change, Trees, tally)
# 18.  Stale notice: absent right after a run; after a price edit,
#      "Assortments or prices changed since this run" shows
# 19.  No "default" or "price set" (as words) anywhere in the page text
#      (Phase 4d: "Scenario A/B" is the user-facing term)
# 20.  Custom assortment through the form in A (spruce poles, price matrix,
#      12-30 cm, 600-1000 cm, volume method Mid, 2000 NOK/m3): the form has no Type, Length step
#      or Diameter rounding and no blurred backdrop; listed and selected
#      (its matrix grid shown), edit keeps its prices; a run selects it; Reset to defaults
#      keeps it and restores the base rows; Remove (from both) drops it
# 21.  Tree list (Phase 5): upload the template -> validation panel lists 12
#      trees and the height methods; Run -> the FinnHoyde height banner shows
#      and the Trees table has a "Height source" column; no error notifications
# 22.  Screenshots of both tabs, the results and the comparisons saved to
#      outputs/app_smoke/review/audit17/ (for review; gitignored, cleared by each
#      run). FAIL if any attempted screenshot is missing (the count is the number
#      of shoot() calls made); 22b: the same for the Norsk session
# 23.  Total browser runtime under 300 s (REVIEW if above)
# 24.  Scenario B's assortments (Phase 4d): a B rule change (spruce sawlog
#      min top 14 cm) is highlighted on both sides; an assortment added to B
#      only is not listed in A and not highlighted in B; one removed from B only
#      is not listed in B; Run -> comparison with B's products (poles); each
#      table lists its own assortments in the fixed order (species, sawlog,
#      pulpwood, waste, custom), no struck-through rows; "Make assortment equal to B" in A copies spruce sawlog;
#      Scenario A downloaded and uploaded to B -> no highlighted
#      differences left
# 25.  Selection and detail panel (layout-and-figures): the first row
#      (spruce sawlog) starts selected and highlighted on both sides;
#      clicking spruce pulpwood in A selects it in A and B (one highlighted
#      row each), both panels show its rules and the flat price field, no
#      grid; a flat price edited in B (333) is highlighted on both sides
#      and stored in B only; clicking spruce sawlog in B shows the grids
#      again; per-assortment price download holds that assortment only
# 26.  Live tree counts: the sidebar box names the plots and trees per
#      species and a run time; it changes with the species group and
#      matches the run's Plots and Trees; after a tree-list upload it
#      counts the 12 trees
# 27.  Log tally (Selected logs; 2026-10-22, replaces the log figures): rows
#      shown, its CSV volume equals the logs' commercial volume; after a
#      comparison B − A is offered, shown signed, and the CSV holds A and B.
#      The DBH-class figure (Per m³) and the height density (Trees card)
#      render as images for a single run and after a comparison
# 28.  Altitude slider: starts at the full range; its value labels stay
#      within the other inputs' edges; 200-400 m changes the
#      counts to select_nfi_trees() for that band; the full range restores
#      the earlier text (check 12 then runs at the full range)
# 29.  Import from .hpr (hpr Phase A): the dialog opens from A; a broken file
#      shows its error and Replace stays disabled; from B the fixture's
#      species (pine, spruce, not used x2) and products (100 Energi
#      unticked) are prefilled; Replace Scenario B closes the dialog, B holds
#      the imported assortments, A is unchanged, nothing is highlighted (the
#      only shared assortments, Waste, are equal; B-only rows are not shaded)
# 30.  The kept .hpr upload (path exported in test mode) exists during the
#      session and is deleted, with its folder, when the session ends (since
#      hpr Phase B the imported file, kept in imported/)
# 31.  .hpr stems (hpr Phase B), run before 30: after the import the source is
#      still NFI; ".hpr stems" uses the imported file with its confirmed
#      species map; 13 stems read, 5 used, 4 warnings for problems in the file
#      (none for Not-used species, multi-tree or short stems); live
#      counts in stems; a comparison runs; the Logs tab has one .hpr export
#      button per scenario (Phase 5a), B's file has the 5 stems, logs and no
#      coordinates (an NFI run has no such button: check 14); no Plots in the Summary and no
#      Plot column in the logs and trees; "Measured length (m)"
# 32.  Copy from A / Copy from B (2026-10-22), run before 29: the buttons
#      are labelled "Copy from B" (A) and "Copy from A" (B); after a flat
#      price edit in B, "Copy from A" asks first ("Copy Scenario A to
#      Scenario B"), Cancel keeps B, Copy makes B equal to A, says so, and
#      nothing is highlighted
# 33.  After 29 (B imported from .hpr), an NFI comparison: A and B share only
#      the Waste rows, so Products shows no value-change figure ("No
#      assortments in both scenarios."), lists A's and B's assortments (no
#      "(type)" rows) and the type table; the tally offers only B for 140
#      SCA Sagtømmer (screenshots 20-21)
#
# Language (Phase 1, 2026-10-27): checks 05-33 drive the page in English
# (APTER_LANG=en); checks 34-39 a second session that starts in Norsk, the
# app's default (screenshots in outputs/app_smoke/review/audit17/, prefix no_):
# 34.  Norsk by default: html lang nb, Norsk active on the switch, the tabs,
#      Run button, window title and the server-rendered assortment table in
#      Norsk; the switch changes static and server text to English and back
#      without a reload (a page marker survives)
# 35.  Both tabs and the sidebar in Norsk (species, county, site class, live
#      tree counts); the price grid, the altitude slider and the DBH-class
#      axis in Norwegian number format (a price typed as "430,5" is stored
#      as 430.5); no English text of the table on the page
# 36.  Dialogs and messages in Norsk: Add assortment, Copy from A, Remove,
#      Import from .hpr with the fixture (species and products tables), a
#      broken tree list (errors from the backend, translated); no English
# 37.  A comparison in Norsk: the four result tabs, Summary labels and
#      decimal commas, Products and Trees headers, the tally caption, the
#      figures render; no English on any result tab
# 38.  Downloads in Norsk: file names, CSVs with BOM, ";" and decimal comma
#      and Norwegian headers; the tally xlsx's "liste" sheet; the scenario
#      xlsx keeps its machine format
# 39.  Switching keeps everything: the results, their numbers and B's edit
#      survive a switch to English and back; the scenario file saved in
#      Norsk loads in English; the choice survives a reload; runtime of the
#      Norsk session (REVIEW above 240 s)
# 40.  Several .hpr files (Phase 2, 2026-10-28; English session, after 31): the
#      fixture and a copy with StemKeys + 1000 uploaded at once give "2 files:
#      26 stems read, 10 used.", one species row per group name, a comparison
#      on 10 stems, a File column in the Trees table, no file name on the
#      page; the trees and logs CSVs have a File column with 1 and 2 and no
#      file name (screenshot in outputs/app_smoke/review/audit17/, prefix phase2_)
# 41.  Stem grades (Phase 6, 2026-10-28): the two checkboxes per scenario ("Use
#      stem grades", "Cut at grade changes") show only with the .hpr source and
#      are off by default; with both ticked in A the invented grades fixture
#      runs as a comparison (A's cutting patterns differ from B's), and Copy
#      from A makes B's boxes equal to A's (screenshots, prefix phase6_)
#
# Verdict :
#   PASS                   -- all checks pass
#   ACCEPTED WITH REVIEW   -- only the runtime check (23), check 22 or check 12's
#                             progress part is REVIEW
#   FAIL                   -- missing dependencies, files, wrong helper
#                             results, app start failure or a failed run
#
# Requires: install.packages(c("shinytest2", "chromote")) and Chrome
#           (or CHROMOTE_CHROME set to a Chromium-based browser).
#
# Outputs :
#   outputs/app_smoke/audit_17_app_smoke.csv
#   outputs/app_smoke/audit_17_app_smoke.txt
#
# Usage   : Rscript audits/audit_17_app_smoke.R   (from the project root)
# ==============================================================================
suppressPackageStartupMessages(library(readr))
root_dir <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
app_dir <- file.path(root_dir, "app")
out_dir <- path_output("app_smoke", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_17_app_smoke.txt")
audit_csv <- file.path(out_dir, "audit_17_app_smoke.csv")
init_audit_log(audit_txt)
E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: Shiny app headless smoke\n",        file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()),           file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
# ==============================================================================
# Check 01: browser-test dependencies
# ==============================================================================
section("01. shinytest2 / chromote / Chrome")
has_st2      <- requireNamespace("shinytest2", quietly = TRUE)
has_chromote <- requireNamespace("chromote",   quietly = TRUE)
chrome_path  <- if (has_chromote)
  tryCatch(chromote::find_chrome(), error = function(e) NULL) else NULL
has_chrome   <- !is.null(chrome_path) && nzchar(chrome_path)
browser_ok   <- has_st2 && has_chromote && has_chrome
rec("Check 01: shinytest2 + chromote installed and Chrome found",
    if (browser_ok) PASS else FAIL,
    if (browser_ok) sprintf("Chrome: %s", chrome_path)
    else paste0(
      "shinytest2=", has_st2, " chromote=", has_chromote,
      " chrome=", has_chrome,
      " -- install.packages(c(\"shinytest2\", \"chromote\")); install Chrome ",
      "or set CHROMOTE_CHROME to the browser path"))
# ==============================================================================
# Check 02: split app files exist
# ==============================================================================
section("02. Split app files exist")
app_files <- c("app.R", "app_ui.R", "app_server.R", "server_scenarios.R", "setup.R",
               "theme.R", "helpers_root.R", "helpers_ui.R", "helpers_validation.R",
               "helpers_upload.R", "www/apter.css", "www/apter.js", "www/apter-icon.svg")
missing_app  <- app_files[!file.exists(file.path(app_dir, app_files))]
conflicting  <- c("ui.R", "server.R", "global.R")
conflicting  <- conflicting[file.exists(file.path(app_dir, conflicting))]
rec("Check 02: Split app files exist",
    if (length(missing_app) == 0L && length(conflicting) == 0L) PASS else FAIL,
    if (length(missing_app) == 0L && length(conflicting) == 0L)
      sprintf("all %d present in app/; no ui.R/server.R/global.R", length(app_files))
    else paste0("missing: ", paste(missing_app, collapse = ", "),
                " | conflicting: ", paste(conflicting, collapse = ", ")))
# ==============================================================================
# Check 03: root resolution (no browser)
# ==============================================================================
section("03. Root resolution")
root_env <- new.env()
root_src_ok <- tryCatch({
  source(file.path(app_dir, "helpers_root.R"), local = root_env)
  TRUE
}, error = function(e) FALSE)
if (root_src_ok) {
  saved_root  <- Sys.getenv("APTER_ROOT", unset = NA)
  restore_env <- function() {
    if (is.na(saved_root)) Sys.unsetenv("APTER_ROOT")
    else Sys.setenv(APTER_ROOT = saved_root)
  }
  # a. unset -> parent of app/
  Sys.unsetenv("APTER_ROOT")
  r_a <- tryCatch(root_env$resolve_app_root(app_dir),
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  rec("Check 03a: Env unset -> resolves to project root",
      if (identical(r_a, root_dir)) PASS else FAIL,
      sprintf("resolved '%s' (expected '%s')", r_a, root_dir))
  # b. APTER_ROOT = empty temp dir -> clear error
  bad_dir <- file.path(tempdir(), "audit17_not_a_root")
  dir.create(bad_dir, showWarnings = FALSE)
  Sys.setenv(APTER_ROOT = bad_dir)
  r_b <- tryCatch({ root_env$resolve_app_root(app_dir); NULL },
                  error = function(e) conditionMessage(e))
  rec("Check 03b: APTER_ROOT = temp dir -> clear error",
      if (!is.null(r_b) && grepl("does not contain R/helpers_paths.R", r_b,
                                 fixed = TRUE)) PASS else FAIL,
      if (is.null(r_b)) "resolved without error (expected stop())"
      else sprintf("error: %s", r_b))
  # c. APTER_ROOT = real root -> resolves
  Sys.setenv(APTER_ROOT = root_dir)
  r_c <- tryCatch(root_env$resolve_app_root(app_dir),
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  rec("Check 03c: APTER_ROOT = project root -> resolves",
      if (identical(r_c, root_dir)) PASS else FAIL,
      sprintf("resolved '%s'", r_c))
  restore_env()
} else {
  rec("Check 03: Root resolution", FAIL, "app/helpers_root.R failed to source")
}
# ==============================================================================
# Check 04: pure helpers (no browser)
# ==============================================================================
section("04. Pure app helpers")
# Same load order as app.R: backend (global), then app files into one env.
H <- new.env()
H$ROOT <- root_dir
saved_np <- Sys.getenv("APTER_N_PLOTS", unset = NA)
Sys.unsetenv("APTER_N_PLOTS")
helpers_err <- tryCatch({
  suppressPackageStartupMessages(library(shiny))
  source(file.path(root_dir, "scripts", "app_backend",
                   "01_run_sampled_plot_bucking.R"), local = FALSE)
  for (f in c("setup.R", "theme.R", "helpers_ui.R", "helpers_validation.R",
             "helpers_upload.R"))
    source(file.path(app_dir, f), local = H)
  ""
}, error = function(e) conditionMessage(e))
if (!is.na(saved_np)) Sys.setenv(APTER_N_PLOTS = saved_np)
if (!nzchar(helpers_err)) {
  # a. price file round trip into A and into B
  defs0 <- H$APP_DEFS; pm0 <- H$APP_PM_DEFAULT
  pm_x <- pm0; pm_x$PriceNOKperM3 <- round(pm_x$PriceNOKperM3 * 1.2, 2)
  f_a  <- H$write_price_file(pm_x)
  up_a <- H$upload_price_set(f_a, f_a, defs0, pm0)
  file.remove(f_a)
  ok_a <- isTRUE(up_a$ok) && identical(up_a$pm, pm_minimal(pm_x)) &&
    isTRUE(scenarios_differ(defs0, pm0, defs0, up_a$pm))
  rec("Check 04a: write_price_file() + upload_price_set() round trip; the scenarios then differ",
      if (ok_a) PASS else FAIL,
      sprintf("ok %s, identical %s, differs from the defaults %s",
              isTRUE(up_a$ok), identical(up_a$pm, pm_minimal(pm_x)),
              isTRUE(up_a$ok) && scenarios_differ(defs0, pm0, defs0, up_a$pm)))
  # b. build_run_params
  p_all <- H$build_run_params(list(species_group = "spruce", hkl_group = "4",
                                   abonitet = c("14", "17"), fylnr_sel = "__all__"))
  some_fyl <- H$.DATA_FYLNR[1L]
  p_fyl <- H$build_run_params(list(species_group = "pine", hkl_group = c("3", "5"),
                                   abonitet = "11", fylnr_sel = some_fyl), n_plots = 5L)
  # hogstkl carries names from unlist(); only the codes matter.
  ok_b <- identical(unname(p_all$hogstkl), c(41L, 42L)) &&
          is.null(p_all$fylnr) && identical(p_all$abonitet, c(14L, 17L)) &&
          isTRUE(p_all$all_plots) && is.null(p_all$n_plots) && identical(p_all$seed, 42L) &&
          identical(p_fyl$n_plots, 5L) && identical(p_fyl$all_plots, FALSE) &&
          identical(unname(p_fyl$hogstkl), c(31L, 32L, 51L, 52L)) &&
          identical(p_fyl$fylnr, some_fyl)
  rec("Check 04b: build_run_params(): HKL groups, county, all plots (or APTER_N_PLOTS), seed 42",
      if (ok_b) PASS else FAIL,
      sprintf("HKL '4' -> %s; '__all__' -> %s; all plots %s, seed %d; HKL '3','5' -> %s; fylnr '%s' -> %s",
              paste(p_all$hogstkl, collapse = ","),
              if (is.null(p_all$fylnr)) "NULL" else p_all$fylnr, isTRUE(p_all$all_plots), p_all$seed,
              paste(p_fyl$hogstkl, collapse = ","), some_fyl,
              if (is.null(p_fyl$fylnr)) "NULL" else p_fyl$fylnr))
  # c. count_eligible_plots vs direct filter
  pp <- H$plots_prod
  n_helper_all <- H$count_eligible_plots(pp, c(41L, 42L), 17L, NULL)
  n_direct_all <- sum(pp$hogstkl %in% c(41L, 42L) & pp$abonitet %in% 17L)
  n_helper_fyl <- H$count_eligible_plots(pp, c(41L, 42L), c(14L, 17L), some_fyl)
  n_direct_fyl <- sum(pp$hogstkl %in% c(41L, 42L) &
                      pp$abonitet %in% c(14L, 17L) & pp$fylnr %in% some_fyl)
  rec("Check 04c: count_eligible_plots() matches direct filter",
      if (n_helper_all == n_direct_all && n_helper_fyl == n_direct_fyl &&
          n_direct_all > 0L) PASS else FAIL,
      sprintf("HKL4/ab17/all: %d vs %d; HKL4/ab14,17/fylnr %s: %d vs %d",
              n_helper_all, n_direct_all, some_fyl, n_helper_fyl, n_direct_fyl))
  # d. default assortment table
  p_d  <- products_from_defs(defs0, pm0)
  ok_d <- identical(p_d$pd, H$APP_PRODUCTS)
  rec("Check 04d: Default assortment table gives the base products",
      if (ok_d) PASS else FAIL,
      sprintf("ProductData identical: %s", identical(p_d$pd, H$APP_PRODUCTS)))
  # e. live tree counts = the trees the run bucks (same selection code)
  cases_e <- list(
    H$build_run_params(list(species_group = "both", hkl_group = "4",
                            abonitet = c("11", "14", "17"), fylnr_sel = "__all__")),
    H$build_run_params(list(species_group = "both", hkl_group = c("3", "5"),
                            abonitet = c("8", "11"), fylnr_sel = some_fyl), n_plots = 5L))
  det_e <- character(0); ok_e <- TRUE
  for (pe in cases_e) {
    sel <- select_nfi_trees(H$trees_meta, pe)
    job <- tryCatch(prepare_sampled_job(pe, root = root_dir), error = function(e) conditionMessage(e))
    same <- is.list(job) &&
      identical(sort(as.character(sel$trees$treid)), sort(as.character(job$tree_data$treid))) &&
      identical(length(sel$plots), as.integer(job$meta$n_plots_sampled))
    tc <- H$tree_counts(sel$trees$species_group, length(sel$plots))
    ok_e <- ok_e && same && identical(tc$n_trees, nrow(sel$trees))
    det_e <- c(det_e, sprintf("%s: '%s' (run: %s trees, %s plots) %s", if (isTRUE(pe$all_plots)) "all plots" else "5 plots",
                              tc$text, if (is.list(job)) nrow(job$tree_data) else job,
                              if (is.list(job)) job$meta$n_plots_sampled else NA, if (same) "same" else "DIFFERENT"))
  }
  rec("Check 04e: Live tree counts select exactly the trees and plots the run bucks",
      if (ok_e) PASS else FAIL, paste(det_e, collapse = " | "))
  # f. altitude filter: the full range is no filter (same params and trees as
  #    without the slider); a narrowed band gives fewer plots and trees, all
  #    inside it, and the run bucks the same trees; plots with missing
  #    altitude count at the full range only
  in_f   <- list(species_group = "both", hkl_group = "4",
                 abonitet = c("11", "14", "17"), fylnr_sel = "__all__")
  band_f <- c(200, 400)
  p_none <- H$build_run_params(in_f)
  p_full <- H$build_run_params(c(in_f, list(hoh_range = H$HOH_RANGE)))
  p_band <- H$build_run_params(c(in_f, list(hoh_range = band_f)))
  s_none <- select_nfi_trees(H$trees_meta, p_none)
  s_full <- select_nfi_trees(H$trees_meta, p_full)
  s_band <- select_nfi_trees(H$trees_meta, p_band)
  job_f  <- tryCatch(prepare_sampled_job(p_band, root = root_dir), error = function(e) conditionMessage(e))
  same_run_f <- is.list(job_f) &&
    identical(sort(as.character(s_band$trees$treid)), sort(as.character(job_f$tree_data$treid)))
  n_cnt_band <- H$count_eligible_plots(pp, c(41L, 42L), c(11L, 14L, 17L), NULL, band_f)
  n_dir_band <- sum(pp$hogstkl %in% c(41L, 42L) & pp$abonitet %in% c(11L, 14L, 17L) &
                    pp$hoh >= band_f[1L] & pp$hoh <= band_f[2L])
  # synthetic NA altitude (the real data has none): one selected plot set to NA
  tm_na  <- H$trees_meta
  na_pl  <- s_full$plots[1L]
  tm_na$hoh[tm_na$plot_id_github == na_pl] <- NA
  na_full_in <- na_pl %in% select_nfi_trees(tm_na, p_full)$plots
  na_band_in <- na_pl %in% select_nfi_trees(tm_na, H$build_run_params(
    c(in_f, list(hoh_range = c(H$HOH_RANGE[1L], H$HOH_RANGE[2L] - 50)))))$plots
  ok_f <- identical(p_full, p_none) && is.null(p_full$hoh_min) &&
    identical(s_full$plots, s_none$plots) && identical(s_full$trees$treid, s_none$trees$treid) &&
    identical(c(p_band$hoh_min, p_band$hoh_max), band_f) &&
    length(s_band$plots) < length(s_full$plots) && nrow(s_band$trees) < nrow(s_full$trees) &&
    nrow(s_band$trees) > 0L && all(s_band$trees$hoh >= band_f[1L] & s_band$trees$hoh <= band_f[2L]) &&
    same_run_f && identical(n_cnt_band, n_dir_band) && na_full_in && !na_band_in
  rec("Check 04f: Altitude filter: full range unchanged, narrowed band fewer plots and trees, missing altitude",
      if (ok_f) PASS else FAIL,
      sprintf("slider %s-%s m; full range: params identical %s, %d plots / %d trees (no slider: %d / %d) | %s-%s m: %d plots / %d trees, all inside %s, run same trees %s, count_eligible_plots %d vs direct %d | NA-altitude plot kept at full range %s, dropped when narrowed %s",
              H$HOH_RANGE[1L], H$HOH_RANGE[2L], identical(p_full, p_none),
              length(s_full$plots), nrow(s_full$trees), length(s_none$plots), nrow(s_none$trees),
              band_f[1L], band_f[2L], length(s_band$plots), nrow(s_band$trees),
              all(s_band$trees$hoh >= band_f[1L] & s_band$trees$hoh <= band_f[2L]),
              same_run_f, n_cnt_band, n_dir_band, na_full_in, !na_band_in))
} else {
  rec("Check 04: Pure app helpers", FAIL,
      paste("sourcing backend/app files failed:", helpers_err))
}
# ==============================================================================
# Checks 05-23: drive the app with shinytest2
# ==============================================================================
browser_checks <- c(
  "Check 05: AppDriver starts the app",
  "Check 06: Two tabs (Assortments and prices first), no Home or footer, no stripe, colours, optBuck, texts",
  "Check 07: Bootstrap 5 and self-hosted Inter font loaded",
  "Check 08: Compact assortment table: base rows, columns, '–' for no limit",
  "Check 09: Both scenarios editable, nothing greyed, no differences at the start",
  "Check 10: Price type per scenario (matrix in B, flat in A); Make assortment equal to A",
  "Check 11: Diameter classes and lengths added and removed",
  "Check 12: Run with B = A: single run, no comparison tab; Run disabled while running",
  "Check 13: Summary shows the nine fields; no Diagnostics tab",
  "Check 14: Logs tab preview (Scenario A, first 20), readable headers, CSV of all logs",
  "Check 15: Trees tab (figures, paged trees table, row click draws the cutting pattern, CSVs) and Products tab after a single run; removed outputs gone; no per-ha text",
  "Check 16: Comparison runs automatically when B differs; four tabs, changed patterns in the Summary, A/B in Products and Trees",
  "Check 17: Assortment removed from B only drops it from B's run",
  "Check 18: Stale notice after a price edit",
  "Check 19: No 'default'/'price set' in the page text",
  "Check 20: Custom assortment (simplified form) added, edited, run, kept by Reset to defaults, removed",
  "Check 21: Uploaded tree list validates, runs and reports height sources",
  "Check 22: Screenshots saved for review",
  "Check 23: Browser runtime under 300 s",
  "Check 24: Scenario B assortments: own rule, B-only, removed from B only, row order, comparison, A downloaded and uploaded to B",
  "Check 25: Row selection, detail panel, selection synced between A and B, flat price field",
  "Check 26: Live tree counts in the sidebar match the run",
  "Check 27: Log tally (volume = the logs' commercial volume; B − A after a comparison) and the distribution and NOK-per-DBH-class figures render",
  "Check 28: Altitude slider: full range at the start, a narrowed band changes the counts, full range restores them",
  "Check 29: Import from .hpr: dialog from A and B, a broken file blocked, species and products prefilled, Replace Scenario B changes B only",
  "Check 30: The kept .hpr upload is deleted when the session ends",
  "Check 31: .hpr stems: not selected automatically; the imported file and species map are reused; a comparison runs on the stems, without plots",
  "Check 32: Copy from A / Copy from B: confirmation, Cancel keeps the scenario, Copy makes it equal to the other",
  "Check 33: Comparison with B imported from .hpr (no shared commercial assortments): no value-change figure, all assortments listed, tally without B − A",
  "Check 34: Norsk by default; the switch changes static and server text to English and back without a reload",
  "Check 35: Both tabs and the sidebar in Norsk; no English text of the table on the page",
  "Check 36: Dialogs and messages in Norsk (assortment form, copy, remove, .hpr import, broken tree list)",
  "Check 37: A comparison in Norsk: result tabs, labels, decimal commas, headers, figures; no English",
  "Check 38: Downloads in Norsk: file names, CSV format and headers, tally xlsx, scenario xlsx format",
  "Check 39: Switching keeps results and edits; a Norsk scenario file loads in English; the choice survives a reload; Norsk session runtime",
  "Check 40: Two .hpr files uploaded at once: one total line, species merged by name, a comparison on both files' stems, a File column, no file names",
  "Check 41: Stem grades: two checkboxes per scenario only for .hpr stems, off by default; ticked in A the run compares A with B; Copy from A copies the boxes")
if (!browser_ok) {
  for (ck in browser_checks)
    rec(ck, FAIL, "skipped: browser dependencies missing (see Check 01)")
} else tryCatch({
  section("05-23. Browser checks")
  txt <- function(app, sel)
    tryCatch(paste(app$get_text(sel), collapse = " "), error = function(e) "")
  js <- function(app, code) tryCatch(app$get_js(code), error = function(e) NA)
  n_err_notif <- function(app)
    js(app, "document.querySelectorAll('.shiny-notification-error').length")
  n_tbl_rows <- function(app, id)
    js(app, sprintf("document.querySelectorAll('#%s .rt-tbody .rt-tr-group').length", id))
  n_cells <- function(app, id, cls)
    js(app, sprintf("document.querySelectorAll('#%s .ht_master td%s').length", id, cls))
  err_txt <- function(...) {
    e <- c(...); e <- e[nzchar(e)]
    if (length(e)) paste(" | driver error:", paste(e, collapse = "; ")) else ""
  }
  try_drv <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))
  # A paged table's sort choice by its label ("Change (NOK) ↓"); the values
  # are column positions, so a choice survives a language switch.
  sort_value <- function(app, id, label) js(app, sprintf(paste0(
    "(function(){ var o = $('#%s_sort')[0].selectize.options;",
    " for (var k in o) if (o[k].label === %s) return o[k].value; return ''; })()"),
    id, jsonlite::toJSON(label, auto_unbox = TRUE)))
  shot_dir <- file.path(out_dir, "review", "audit17")   # gitignored; only this subfolder is cleared
  unlink(shot_dir, recursive = TRUE); ensure_dir(shot_dir)
  # The only screenshot path (2026-10-29). A shot is rendered to a local temp
  # file, then copied into shot_dir (created again if it vanished; one retry
  # after 2 s). A failure keeps its error text; check 22 fails if any shot
  # was attempted and is missing. Norsk-session shots (prefix no_) count apart.
  shots <- character(0)
  shot_state <- new.env()
  shot_state$attempted <- 0L; shot_state$errors <- character(0)
  shot_state$attempted_no <- 0L; shot_state$errors_no <- character(0)
  shoot <- function(app, name, dir = shot_dir, selector = NULL, norsk = FALSE) {
    f <- file.path(dir, paste0(name, ".png"))
    if (norsk) shot_state$attempted_no <- shot_state$attempted_no + 1L
    else shot_state$attempted <- shot_state$attempted + 1L
    tmp <- tempfile(fileext = ".png")
    attempt <- function() tryCatch({
      unlink(tmp)
      if (is.null(selector)) app$get_screenshot(tmp) else app$get_screenshot(tmp, selector = selector)
      if (!file.exists(tmp)) stop("the screenshot was not rendered")
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      if (!isTRUE(file.copy(tmp, f, overwrite = TRUE)) || !file.exists(f)) stop("could not write ", f)
      ""
    }, error = function(e) conditionMessage(e))
    msg <- attempt()
    if (nzchar(msg)) { Sys.sleep(2); msg <- attempt() }
    unlink(tmp)
    if (nzchar(msg)) {
      msg <- sprintf("%s: %s", name, gsub("\\s+", " ", msg))
      if (norsk) shot_state$errors_no <- c(shot_state$errors_no, msg)
      else shot_state$errors <- c(shot_state$errors, msg)
      return(invisible(FALSE))
    }
    if (!norsk) shots <<- c(shots, f)
    invisible(TRUE)
  }
  # "7 of 34 screenshots in <dir>; first error: ..." (check 22 and 22b)
  shot_summary <- function(n_ok, n_all, errs, where)
    sprintf("%d of %d screenshots in %s%s", n_ok, n_all, where,
            if (length(errs)) paste0("; first error: ", errs[1L]) else "")
  # A's or B's prices as the app would download them (long CSV).
  dl_pm <- function(app, id) {
    f <- tryCatch(app$get_download(id), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    pm_minimal(as.data.frame(readr::read_csv(f, show_col_types = FALSE)))
  }
  # Select an assortment as a user does: click its row in A's or B's table
  # (the key is the row's hidden .key column).
  pick <- function(app, side, key) {
    app$run_js(sprintf(paste0(
      "(function(){ var id = 'tbl_asm_%s', rows = Reactable.getInstance(id).rows;",
      " var i = rows.findIndex(function(r){ return r.original['.key'] === '%s'; });",
      " var g = document.querySelectorAll('#' + id + ' .rt-tbody .rt-tr-group')[i];",
      " if (g) g.querySelector('.rt-td').click(); })()"), side, key))
    app$wait_for_idle()
  }
  # A scenario as downloaded (assortments and all prices).
  scen_of <- function(app, side) {
    f <- tryCatch(app$get_download(paste0("dl_scen_", side)), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    scenario_import(f)
  }
  axes_of <- function(pm, id) {
    s <- pm[pm$ProductKey == id & !is.na(pm$LengthClass_dm), , drop = FALSE]
    list(cls = sort(unique(s$PriceDiameterClass_cm)), len = sort(unique(s$LengthClass_dm)))
  }
  # AppDriver$new() skips (as "On CRAN") outside interactive sessions unless set.
  Sys.setenv(NOT_CRAN = "true")
  # NFI runs sample 5 plots instead of all (setup.R), to keep the audit short.
  Sys.setenv(APTER_N_PLOTS = "5")
  # Checks 05-33 drive the English page (test-only APTER_LANG, setup.R; the
  # app starts in Norsk otherwise); checks 34-39 a second session in Norsk.
  Sys.setenv(APTER_LANG = "en")
  # chromote waits 10 s for Chrome by default; cold starts on the remote
  # desktop can exceed that. 30 s per attempt, and audit_app_driver() retries
  # once on a fresh Chrome and profile (helpers_audit.R).
  options(chromote.timeout = 30)
  t_browser <- proc.time()
  # Own Chrome profile and Chrome process, closed on every exit path: the
  # finally of this tryCatch, and at R exit if the script halts first.
  audit_browser_start()
  app <- audit_app_driver(app_dir, name = "audit17",
                          load_timeout = 60 * 1000,
                          timeout = 120 * 1000,
                          width = 1400, height = 1000)
  app_ok <- inherits(app, "AppDriver")
  rec(browser_checks[1], if (app_ok) PASS else FAIL,
      if (app_ok) "app started" else paste("start error:", app))
  # Probe: one screenshot right after the start. If this fails, screenshots do
  # not work in this run at all; say so now, with the message.
  if (app_ok) {
    app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30 * 1000)
    if (!isTRUE(shoot(app, "00_probe"))) {
      rec("Check 05b: Screenshot probe right after the start", FAIL,
          shot_summary(0L, 1L, shot_state$errors, shot_dir))
    } else {
      unlink(file.path(shot_dir, "00_probe.png"))
      shots <- setdiff(shots, file.path(shot_dir, "00_probe.png"))
      shot_state$attempted <- shot_state$attempted - 1L
    }
  }
  if (app_ok) {
    # Runs are background tasks (ExtendedTask): after the click, wait until
    # no task is running and Run is enabled again.
    wait_run <- function(app, timeout = 120 * 1000) {
      app$wait_for_idle(timeout = timeout)
      app$wait_for_js(paste0(
        "!document.querySelector('#run_status .status-running') && ",
        "!document.getElementById('run_btn').disabled"), timeout = timeout)
      app$wait_for_idle(timeout = timeout)
    }
    run <- function(app) { app$click("run_btn"); wait_run(app) }
    tab_shown <- function(app, value) isTRUE(js(app, sprintf(paste0(
      "(function(){ var a = document.querySelector('#result_tabs a[data-value=\"%s\"]');",
      " return !!a && a.offsetParent !== null; })()"), value)))
    # The visible result tabs (results restructure, 2026-10-23).
    result_tabs_txt <- function(app) js(app, "Array.from(document.querySelectorAll('#result_tabs .nav-link')).filter(function(a){ return a.offsetParent !== null; }).map(function(a){ return a.textContent.trim(); }).join('|')")
    RESULT_TABS <- "Summary|Products|Trees|Logs"
    # Column headers and the rows of a reactable ("a;b;c|d;e;f").
    headers_of <- function(app, id) js(app, sprintf(
      "Array.from(document.querySelectorAll('#%s .rt-th')).map(function(e){ return e.textContent.trim(); }).join('|')", id))
    rows_txt <- function(app, id) js(app, sprintf(paste0(
      "Array.from(document.querySelectorAll('#%s .rt-tbody .rt-tr')).map(function(r){",
      " return Array.from(r.querySelectorAll('.rt-td')).map(function(c){ return c.textContent.trim(); }).join(';'); }).join('|')"), id))
    compared_now <- function(app) grepl("Changed cutting patterns|Changed stems", txt(app, "#summary_cards"))
    # 06-07. layout and theme
    app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30 * 1000)
    tabs <- js(app, paste0("Array.from(document.querySelectorAll('#main_nav .nav-link'))",
                           ".map(function(a){ return a.textContent.trim(); }).join('|')"))
    active <- js(app, "document.querySelector('#main_nav .nav-link.active').textContent.trim()")
    body0 <- js(app, "document.body.textContent")
    n_old <- js(app, "document.querySelectorAll('.status-tile, .apter-footer, .apter-hero').length")
    stripe <- js(app, "document.querySelectorAll('.banner-strip').length")
    # The look (option 3 "Combination"), as computed by the browser.
    css <- function(sel, prop) js(app, sprintf(
      "(function(){ var e = document.querySelector(%s); return e ? getComputedStyle(e).%s : 'missing'; })()",
      jsonlite::toJSON(sel, auto_unbox = TRUE), prop))
    look <- c(
      page        = css("body", "backgroundColor"),
      navbar      = css(".navbar", "backgroundColor"),
      navbar_rule = css(".navbar", "borderBottomColor"),
      tab         = css("#main_nav .nav-link:not(.active)", "color"),
      tab_active  = css("#main_nav .nav-link.active", "color"),
      tab_line    = css("#main_nav .nav-link.active", "borderBottomColor"),
      card_border = css(".bslib-card", "borderTopColor"),
      card_shadow = css(".bslib-card", "boxShadow"),
      card_bg     = css(".bslib-card", "backgroundColor"),
      sidebar_bg  = css(".bslib-sidebar-layout > .sidebar", "backgroundColor"),
      primary     = css(".btn.btn-primary", "backgroundColor"),
      secondary   = css(".btn.btn-secondary", "backgroundColor"),
      label       = css(".section-hdr", "textTransform"),
      label_col   = css(".section-hdr", "color"))
    look_exp <- c(page = "rgb(243, 244, 243)", navbar = "rgb(255, 255, 255)",
                  navbar_rule = "rgb(230, 230, 230)", tab = "rgb(107, 107, 107)",
                  tab_active = "rgb(17, 17, 17)", tab_line = "rgb(17, 17, 17)",
                  card_border = "rgb(230, 230, 230)", card_shadow = "none",
                  card_bg = "rgb(255, 255, 255)", sidebar_bg = "rgb(255, 255, 255)",
                  primary = "rgb(63, 110, 88)", secondary = "rgb(238, 240, 239)",
                  label = "uppercase", label_col = "rgb(138, 138, 138)")
    look_ok <- identical(unname(unlist(look)), unname(look_exp))
    title <- js(app, "document.title")
    brand <- js(app, "document.querySelector('.navbar-brand').textContent.trim()")
    labels6 <- js(app, "Array.from(document.querySelectorAll('label, .control-label')).map(function(e){ return e.textContent; }).join(' | ')")
    texts6 <- paste(body0, labels6)
    intro6 <- grepl("Define two scenarios with assortments and prices. optBuck finds the optimal bucking",
                    body0, fixed = TRUE)
    # The logo measured in the browser: height, the brand icon's height, the
    # navbar's height and whether logo and brand sit on one line.
    geom <- function(app) js(app, paste0(
      "(function(){ var n = document.querySelector('.navbar'), i = document.querySelector('.navbar img.navbar-logo'),",
      " b = document.querySelector('.navbar-brand img'); if (!n || !i || !b) return 'missing';",
      " var ri = i.getBoundingClientRect(), rb = b.getBoundingClientRect(), rn = n.getBoundingClientRect();",
      " return [i.complete && i.naturalWidth > 0 ? 1 : 0, Math.round(ri.height), Math.round(rb.height),",
      " Math.round(rn.height), Math.round(Math.abs((ri.top + ri.height / 2) - (rb.top + rb.height / 2))),",
      " i.getAttribute('height'), Math.round(ri.width)].join(','); })()"))
    shot_nav <- function(app, name) shoot(app, name, selector = ".navbar")
    e6 <- try_drv({ app$set_window_size(width = 1920, height = 1000); app$wait_for_idle() })
    g1920 <- strsplit(as.character(geom(app)), ",")[[1]]
    shot_nav(app, "00a_navbar_1920")
    e6b <- try_drv({ app$set_window_size(width = 700, height = 1000); app$wait_for_idle() })
    g700 <- strsplit(as.character(geom(app)), ",")[[1]]
    shot_nav(app, "00b_navbar_700")
    e6c <- try_drv({ app$set_window_size(width = 1400, height = 1000); app$wait_for_idle() })
    gi <- function(g, k) suppressWarnings(as.integer(g[k]))
    # 1920: loaded, 26 px like the icon, inside the navbar, centred with the brand.
    logo_ok <- length(g1920) == 7L && identical(gi(g1920, 1), 1L) && identical(gi(g1920, 2), gi(g1920, 3)) &&
      isTRUE(gi(g1920, 2) <= gi(g1920, 4)) && isTRUE(gi(g1920, 5) <= 4L) && identical(g1920[6], "26") &&
      isTRUE(gi(g1920, 4) <= 80L)
    # 700: the navbar collapses; it must stay one bar (the logo hidden in the
    # collapsed menu or at most icon height).
    narrow_ok <- length(g700) == 7L && isTRUE(gi(g700, 4) <= 80L) && isTRUE(gi(g700, 2) <= 26L)
    logo6 <- if (length(g1920) == 7L) gi(g1920, 2) else 0L
    ok6 <- identical(tabs, "Assortments and prices|Simulator") &&
      identical(active, "Assortments and prices") && identical(n_old, 0L) &&
      !grepl("AVAILABLE|PLANNED|Release ", body0) && !grepl("\\bHome\\b", body0) &&
      identical(stripe, 0L) && look_ok && grepl("optBuck", title, fixed = TRUE) && identical(brand, "optBuck") &&
      !grepl("Apter", paste(title, brand), fixed = TRUE) &&
      grepl("NFI data", texts6, fixed = TRUE) && !grepl("NFI sample", texts6, fixed = TRUE) &&
      !grepl("(^|[^0-9.])2021([^0-9]|$)", texts6) && !grepl("wide or long", texts6, fixed = TRUE) &&
      !grepl("runs use Scenario A only|runs compare both", texts6) &&
      intro6 && logo_ok && narrow_ok && !nzchar(paste0(e6, e6b, e6c)) &&
      grepl("Data source", texts6, fixed = TRUE) && !grepl("Tree source", texts6, fixed = TRUE)
    rec(browser_checks[2], if (ok6) PASS else FAIL,
        sprintf("tabs: '%s' | open: '%s' | Home/tiles/footer elements: %s | stripe: %s | look: %s | title: '%s' | brand: '%s' | intro line: %s | logo height: %s px | 'NFI data': %s | '2021': %s | 'wide or long': %s",
                tabs, active, n_old, stripe,
                if (look_ok) "as option 3" else paste(sprintf("%s %s (expected %s)", names(look), unlist(look), look_exp)[unlist(look) != look_exp], collapse = "; "),
                title, brand, intro6,
                sprintf("%s (1920 px: loaded,logo,icon,navbar,centre offset,height attr,width = %s; 700 px: %s)",
                        logo6, paste(g1920, collapse = ","), paste(g700, collapse = ",")),
                grepl("NFI data", texts6, fixed = TRUE),
                grepl("(^|[^0-9.])2021([^0-9]|$)", texts6), grepl("wide or long", texts6, fixed = TRUE)))
    bs_ver <- js(app, "typeof bootstrap !== 'undefined' ? bootstrap.Tooltip.VERSION : ''")
    inter_loaded <- js(app, paste0(
      "Array.from(document.fonts).some(f => f.family.replace(/[\"']/g, '') ",
      "=== 'Inter' && f.status === 'loaded')"))
    n_gfonts <- js(app, "document.querySelectorAll('link[href*=\"fonts.googleapis\"]').length")
    ok7 <- startsWith(as.character(bs_ver), "5") && isTRUE(inter_loaded) && identical(n_gfonts, 0L)
    rec(browser_checks[3], if (ok7) PASS else FAIL,
        sprintf("Bootstrap %s | Inter loaded: %s | Google Fonts links: %s",
                bs_ver, isTRUE(inter_loaded), n_gfonts))
    # 08. assortment table
    asm_txt <- txt(app, "#tbl_asm_a")
    hdr <- c("Assortment", "Species", "Top Ø (cm)", "Max butt Ø (cm)", "Length (cm)",
             "Volume method")
    gone8 <- c("Type", "Length step", "Diameter rounding")
    hdr_ok <- all(vapply(hdr, grepl, logical(1), x = asm_txt, fixed = TRUE)) &&
      !any(vapply(gone8, grepl, logical(1), x = asm_txt, fixed = TRUE))
    n_asm0 <- n_tbl_rows(app, "tbl_asm_a")
    sortable8 <- js(app, "document.querySelectorAll('#tbl_asm_a [aria-sort], #tbl_asm_b [aria-sort], #tbl_asm_a .rt-sort-header').length")
    # Gone: whole-scenario and matrix copy buttons, the assortment and
    # matrix dropdowns, the flat-price table selector.
    copy8 <- js(app, paste0("['pm_a_copy_all','pm_b_copy_all','asm_copy_all_a','asm_copy_all_b',",
                            "'pm_a_copy_one','pm_b_copy_one','asm_pick_a','asm_pick_b','pm_id_a','pm_id_b']",
                            ".filter(function(i){ return !!document.getElementById(i); }).length"))
    copy1 <- js(app, paste0("['asm_copy_one_a','asm_copy_one_b'].map(function(i){",
                            " var b = document.getElementById(i); return b ? b.textContent.trim() : ''; }).join('|')"))
    n_eq8 <- js(app, "Array.from(document.querySelectorAll('button')).filter(function(b){ return /^Make [AB] equal to [AB]$|^Make matrix equal/.test(b.textContent.trim()); }).length")
    ok8 <- hdr_ok && identical(n_asm0, 6L) && grepl("–", asm_txt, fixed = TRUE) &&
      !grepl("Inf", asm_txt, fixed = TRUE) && identical(sortable8, 0L) && identical(copy8, 0L) &&
      identical(copy1, "Make assortment equal to B|Make assortment equal to A") &&
      identical(n_eq8, 0L)
    rec(browser_checks[4], if (ok8) PASS else FAIL,
        sprintf("rows %s | all visible columns: %s | '–' shown: %s | 'Inf' shown: %s | sortable headers: %s | copy buttons: '%s' (removed controls present: %s, 'Make A/B equal to' / 'Make matrix equal' buttons: %s)",
                n_asm0, hdr_ok, grepl("–", asm_txt, fixed = TRUE),
                grepl("Inf", asm_txt, fixed = TRUE), sortable8, copy1, copy8, n_eq8))
    shoot(app, "01_assortments_and_prices")
    # 09. Both scenarios editable (Handsontable marks read-only cells
    # htDimmed), nothing greyed or highlighted at the start.
    # Visible highlights only (a hidden grid of another assortment does not count).
    marks <- function(app) js(app, paste0("Array.from(document.querySelectorAll('.pm-diff, .asm-diff, .asm-only, ",
                                          ".asm-none, .pm-same')).filter(function(e){ return e.offsetParent !== null; }).length"))
    a_cells <- n_cells(app, "pm_a", ""); a_dim <- n_cells(app, "pm_a", ".htDimmed")
    b_cells <- n_cells(app, "pm_b", ""); b_dim <- n_cells(app, "pm_b", ".htDimmed")
    marks9 <- marks(app)
    dropdown9 <- js(app, "document.querySelectorAll('#pm_b_state, #pm_status').length")
    ok9 <- isTRUE(a_cells > 0L) && identical(a_dim, 0L) && isTRUE(b_cells > 0L) &&
      identical(b_dim, 0L) && identical(marks9, 0L) && identical(dropdown9, 0L)
    rec(browser_checks[5], if (ok9) PASS else FAIL,
        sprintf("A cells %s (read-only %s) | B cells %s (read-only %s) | highlighted or greyed: %s | B dropdown / status note: %s",
                a_cells, a_dim, b_cells, b_dim, marks9, dropdown9))
    # 25. Row selection, detail panel, selection synced between A and B
    head_of <- function(app, s) js(app, sprintf(
      "(function(){ var t = document.querySelector('#asm_head_%s .asm-detail-title'); return t ? t.textContent.trim() : ''; })()", s))
    shown <- function(app, sel) isTRUE(js(app, sprintf(
      "(function(){ var e = document.querySelector('%s'); return !!e && e.offsetParent !== null; })()", sel)))
    sel_rows <- function(app) js(app, paste0(
      "['a','b'].map(function(s){ return Array.from(document.querySelectorAll('#tbl_asm_' + s + ' .asm-selected'))",
      ".map(function(r){ return r.querySelector('.rt-td').textContent.trim() + ' ' + r.querySelectorAll('.rt-td')[1].textContent.trim(); }).join(','); }).join('|')"))
    flat_of <- function(sc, key) { if (is.null(sc)) return(NA); p <- sc$pm
      p$PriceNOKperM3[p$ProductKey == key & is.na(p$LengthClass_dm)][1] }
    c25 <- list()
    c25$start_rows <- sel_rows(app); c25$start_head <- paste(head_of(app, "a"), head_of(app, "b"), sep = "|")
    c25$start_grid <- shown(app, "#pm_a") && shown(app, "#pm_b")
    e25 <- try_drv({
      pick(app, "a", "spruce_pulpwood")
      c25$rows <- sel_rows(app); c25$head <- paste(head_of(app, "a"), head_of(app, "b"), sep = "|")
      c25$flat <- shown(app, "#flat_wrap_a input") && shown(app, "#flat_wrap_b input")
      c25$grid <- shown(app, "#pm_a") || shown(app, "#pm_b")
      c25$rules <- txt(app, "#asm_head_a .asm-rules")
      app$run_js("var i = document.querySelector('#flat_b input'); i.value = '333'; $(i).trigger('change');")
      app$wait_for_idle()
      c25$diff <- js(app, "['flat_wrap_a','flat_wrap_b'].map(function(i){ return document.getElementById(i).classList.contains('is-diff') ? 1 : 0; }).join(',')")
      c25$b <- flat_of(scen_of(app, "b"), "spruce_pulpwood"); c25$a <- flat_of(scen_of(app, "a"), "spruce_pulpwood")
      c25$dl_keys <- paste(unique(dl_pm(app, "dl_b_long")$ProductKey), collapse = ",")
      shoot(app, "01a_detail_flat_synced")
      app$click("asm_copy_one_b"); app$wait_for_idle()
      c25$marks <- marks(app)
      pick(app, "b", "spruce_sawlog")
      c25$back <- paste(head_of(app, "a"), head_of(app, "b"), sep = "|")
      c25$back_grid <- shown(app, "#pm_a") && shown(app, "#pm_b")
    })
    a_flat0 <- flat_of(list(pm = H$APP_PM_DEFAULT), "spruce_pulpwood")
    ok25 <- !nzchar(e25) && identical(c25$start_rows, "Sawlog Spruce|Sawlog Spruce") &&
      identical(c25$start_head, "Spruce sawlog|Spruce sawlog") && isTRUE(c25$start_grid) &&
      identical(c25$rows, "Pulpwood Spruce|Pulpwood Spruce") &&
      identical(c25$head, "Spruce pulpwood|Spruce pulpwood") && isTRUE(c25$flat) && identical(c25$grid, FALSE) &&
      grepl("Solid volume", c25$rules, fixed = TRUE) && identical(c25$diff, "1,1") &&
      isTRUE(c25$b == 333) && isTRUE(c25$a == a_flat0) && identical(c25$dl_keys, "spruce_pulpwood") &&
      identical(c25$marks, 0L) && identical(c25$back, "Spruce sawlog|Spruce sawlog") && isTRUE(c25$back_grid)
    rec(browser_checks[21], if (ok25) PASS else FAIL,
        sprintf("start: selected rows '%s', panels '%s', grids %s | click spruce pulpwood in A: rows '%s', panels '%s', flat fields %s, grids %s, rules '%s' | B flat 333: highlighted A,B %s, stored B %s, A %s (default %s) | B's price download: %s | Make assortment equal to A: %s marks | click spruce sawlog in B: '%s', grids %s%s",
                c25$start_rows, c25$start_head, c25$start_grid, c25$rows, c25$head, c25$flat, c25$grid,
                trimws(c25$rules), c25$diff, c25$b, c25$a, a_flat0, c25$dl_keys, c25$marks, c25$back,
                c25$back_grid, err_txt(e25)))
    # 10. Price type per scenario: spruce pulpwood a price matrix in B only
    e10b <- try_drv({
      pick(app, "b", "spruce_pulpwood"); app$click("asm_edit_b"); app$wait_for_idle()
      app$set_inputs(asm_price_type = "matrix", asm_price = 300, asm_max_class = 30, wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
    })
    grid_b10 <- shown(app, "#pm_b"); flat_a10 <- shown(app, "#flat_wrap_a input")
    head10 <- paste(head_of(app, "a"), head_of(app, "b"), sep = "|")
    diff_a10 <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-diff').length")
    diff_b10 <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-diff').length")
    shoot(app, "01b_price_type_in_B")
    e10c <- try_drv({ app$click("asm_copy_one_b"); app$wait_for_idle() })
    marks10 <- marks(app); msg10 <- txt(app, "#msg_b")
    ok10 <- !nzchar(paste0(e10b, e10c)) && grid_b10 && flat_a10 &&
      identical(head10, "Spruce pulpwood|Spruce pulpwood") &&
      isTRUE(diff_a10 >= 1L) && isTRUE(diff_b10 >= 1L) && identical(marks10, 0L) &&
      grepl("equal to Scenario A", msg10, fixed = TRUE)
    rec(browser_checks[6], if (ok10) PASS else FAIL,
        sprintf("panels '%s': B shows a matrix grid %s, A the flat price field %s | highlighted cells A %s, B %s | after Make assortment equal to A: %s marks, '%s'%s",
                head10, grid_b10, flat_a10, diff_a10, diff_b10, marks10, trimws(msg10),
                err_txt(e10b, e10c)))
    # 11. rows and columns of A (spruce sawlog)
    pick(app, "a", "spruce_sawlog")
    ax0 <- axes_of(dl_pm(app, "dl_a_long"), "spruce_sawlog")
    step <- function(btn, set = NULL) {
      e <- try_drv({
        if (!is.null(set)) do.call(app$set_inputs, c(set, list(wait_ = FALSE)))
        app$click(btn); app$wait_for_idle()
      })
      list(e = e, ax = axes_of(dl_pm(app, "dl_a_long"), "spruce_sawlog"))
    }
    # The cap rule under the grid, and the grid at full height (no inner
    # scrolling: every row on the page; 2026-10-22).
    cap0 <- txt(app, "#pm_a_cap")
    h0 <- js(app, "document.getElementById('pm_a').offsetHeight")
    s1 <- step("pm_a_cls_add_hi")
    cap1 <- txt(app, "#pm_a_cap")
    s2 <- step("pm_a_cls_add_lo")
    s3 <- step("pm_a_len_add", list(pm_a_len_new = 58))
    s4 <- step("pm_a_len_rm", list(pm_a_len_pick = "58"))
    s5 <- step("pm_a_cls_rm_hi"); s6 <- step("pm_a_cls_rm_lo")
    hi0 <- max(ax0$cls); lo0 <- min(ax0$cls)
    ok11 <- !nzchar(paste0(s1$e, s2$e, s3$e, s4$e, s5$e, s6$e)) &&
      identical(s1$ax$cls, c(ax0$cls, hi0 + 1L)) &&
      identical(s2$ax$cls, c(lo0 - 1L, ax0$cls, hi0 + 1L)) &&
      identical(s3$ax$len, sort(c(ax0$len, 58L))) && identical(s4$ax$len, ax0$len) &&
      identical(s6$ax$cls, ax0$cls) && identical(s6$ax$len, ax0$len) &&
      identical(trimws(cap0), sprintf("≥ %d cm: priced as %d up to max top 60 cm", hi0, hi0)) &&
      identical(trimws(cap1), sprintf("≥ %d cm: priced as %d up to max top 60 cm", hi0 + 1L, hi0 + 1L)) &&
      isTRUE(h0 == 34 + 24 * length(ax0$cls))
    rec(browser_checks[7], if (ok11) PASS else FAIL,
        sprintf("classes %d-%d -> +larger %d-%d -> +smaller %d-%d -> back %d-%d | lengths %d -> +58: %d -> -58: %d | cap line '%s' -> '%s' | grid height %s px for %d rows%s",
                lo0, hi0, min(s1$ax$cls), max(s1$ax$cls), min(s2$ax$cls), max(s2$ax$cls),
                min(s6$ax$cls), max(s6$ax$cls), length(ax0$len), length(s3$ax$len),
                length(s4$ax$len), trimws(cap0), trimws(cap1), h0, length(ax0$cls),
                err_txt(s1$e, s2$e, s3$e, s4$e, s5$e, s6$e)))
    # 26 (first part). Live tree counts before the run; they follow the
    # species group without a run.
    c26 <- list()
    e26 <- try_drv({
      app$set_inputs(main_nav = "simulator")
      # The box re-renders after the input; wait until its text changes.
      counts_after <- function(...) {
        prev <- txt(app, "#tree_counts")
        app$set_inputs(...)
        try(app$wait_for_js(sprintf("document.querySelector('#tree_counts').innerText.trim() !== %s",
                                    jsonlite::toJSON(trimws(prev), auto_unbox = TRUE)), timeout = 10 * 1000), silent = TRUE)
        app$wait_for_idle(); txt(app, "#tree_counts")
      }
      c26$spruce <- counts_after(abonitet = "17")
      c26$both <- counts_after(species_group = "both")
      c26$back <- counts_after(species_group = "spruce")
    })
    # 28. Altitude slider: starts at the full range; a narrowed band changes
    # the counts to exactly select_nfi_trees() for that band; the full range
    # restores the earlier text (the run in check 12 then uses the full range).
    c28 <- list()
    # The value labels stay within the other inputs' edges (Run button).
    edges28 <- function(app) js(app, paste0(
      "(() => { const b = s => document.querySelector(s).getBoundingClientRect();",
      " const r = b('#run_btn'), f = b('.irs--shiny .irs-from'), t = b('.irs--shiny .irs-to');",
      " return [Math.round(f.left - r.left), Math.round(r.right - t.right)]; })()"))
    e28 <- try_drv({
      c28$start <- app$get_value(input = "hoh_range")
      c28$edges_full <- edges28(app)
      c28$band  <- counts_after(hoh_range = c(200, 400))
      c28$full  <- counts_after(hoh_range = H$HOH_RANGE)
      c28$end   <- app$get_value(input = "hoh_range")
    })
    num_of <- function(x, pat) suppressWarnings(as.integer(gsub(",", "", sub(pat, "\\1", regmatches(x, regexpr(pat, x, perl = TRUE))))))
    # 12. run with B = A (default selection: spruce, HKL 4; bonitet 17)
    busy_disabled <- NA; busy_status <- ""
    e12 <- try_drv({
      # already on the Simulator with bonitet 17 (check 26 above): no set_inputs
      app$click("run_btn")
      app$wait_for_idle(timeout = 120 * 1000)
      busy_disabled <- isTRUE(js(app, "document.getElementById('run_btn').disabled"))
      busy_status   <- txt(app, "#run_status")
      wait_run(app)
    })
    after_enabled <- !isTRUE(js(app, "document.getElementById('run_btn').disabled"))
    cards <- txt(app, "#summary_cards")
    nerr12 <- n_err_notif(app)
    cmp_tab12 <- tab_shown(app, "comparison")
    tabs12 <- result_tabs_txt(app)
    ok12 <- !nzchar(e12) && grepl("Trees with logs", cards, fixed = TRUE) &&
      identical(nerr12, 0L) && !cmp_tab12 && identical(tabs12, RESULT_TABS) && after_enabled
    progress_ok <- isTRUE(busy_disabled) && grepl("[A-Za-z]", busy_status)
    rec(browser_checks[8], if (!ok12) FAIL else if (progress_ok) PASS else REVIEW,
        sprintf("summary rendered: %s | result tabs '%s', comparison tab shown: %s | error notifications: %s | during the run: Run disabled %s, status '%s' | after: Run enabled %s%s",
                grepl("Trees with logs", cards, fixed = TRUE), tabs12, cmp_tab12, nerr12,
                isTRUE(busy_disabled), trimws(busy_status), after_enabled, err_txt(e12)))
    shoot(app, "02_simulator_summary")
    # 13. summary fields
    fields <- c("Run time", "Plots", "Trees", "Trees with logs", "Logs",
                "Commercial volume (m³ u.b.)", "Total value (NOK)", "NOK per m³",
                "Price volume (m³)")
    labels <- js(app, paste0("Array.from(document.querySelectorAll('#summary_cards .ro-label'))",
                             ".map(function(e){ return e.textContent.trim(); }).join('|')"))
    plots_val <- js(app, paste0("(function(){ var f = Array.from(document.querySelectorAll('#summary_cards .ro-field'))",
                                ".find(function(e){ return e.querySelector('.ro-label').textContent.trim() === 'Plots'; });",
                                " return f ? f.querySelector('.ro-value').textContent.trim() : ''; })()"))
    diag_tab <- js(app, "document.querySelectorAll('#result_tabs a[data-value=\"Diagnostics\"], #result_tabs a[data-value=\"diagnostics\"]').length")
    ok13 <- identical(labels, paste(fields, collapse = "|")) && identical(plots_val, "5") &&
      identical(diag_tab, 0L)
    rec(browser_checks[9], if (ok13) PASS else FAIL,
        sprintf("labels: '%s' | plots: '%s' | Diagnostics tabs: %s", labels, plots_val, diag_tab))
    n_logs <- suppressWarnings(as.integer(gsub("[^0-9]", "", js(app, paste0(
      "(function(){ var f = Array.from(document.querySelectorAll('#summary_cards .ro-field'))",
      ".find(function(e){ return e.querySelector('.ro-label').textContent.trim() === 'Logs'; });",
      " return f ? f.querySelector('.ro-value').textContent : ''; })()")))))
    # 14. selected logs: preview of the first 20 and a CSV of all
    read_logs_csv <- function(app) {
      f <- tryCatch(app$get_download("dl_logs"), error = function(e) NULL)
      if (is.null(f)) return(NULL)
      utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    }
    e14 <- try_drv({ app$set_inputs(result_tabs = "logs"); app$wait_for_idle() })
    note14 <- txt(app, "#logs_preview_note")
    hdr14 <- js(app, "Array.from(document.querySelectorAll('#tbl_selected_logs .rt-th')).map(function(e){ return e.textContent.trim(); }).join('|')")
    h14 <- strsplit(as.character(hdr14), "|", fixed = TRUE)[[1]]
    n_rows14 <- n_tbl_rows(app, "tbl_selected_logs")
    # Figures as images: "id:width" for each plot that rendered.
    figs <- function(app, ids) js(app, sprintf(paste0(
      "[%s].map(function(i){ var m = document.querySelector('#' + i + ' img');",
      " return i + ':' + (m && m.complete ? m.naturalWidth : 0); }).join(',')"),
      paste0("'", ids, "'", collapse = ",")))
    fig_ok <- function(f) length(f) == 1L && !is.na(f) && !grepl(":0(,|$)", f)
    # Log tally (2026-10-22, replaces the log figures): rows shown, and the
    # tally CSV's volume equals the logs' commercial volume (waste is 0).
    read_tally_csv <- function(app) {
      f <- tryCatch(app$get_download("dl_tally_csv"), error = function(e) NULL)
      if (is.null(f)) return(NULL)
      utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    }
    tally_state <- function(app, logs_csv) {
      t <- read_tally_csv(app)
      n <- n_tbl_rows(app, "tbl_tally")
      vol_logs <- if (is.null(logs_csv)) NA else
        sum(logs_csv[logs_csv$Scenario == "A", "Commercial volume u.b. (m³)"])
      vol_tally <- if (is.null(t)) NA else sum(t[t$Scenario == "A", "Volume (m3 u.b.)"])
      list(rows = n, scen = if (is.null(t)) "" else paste(unique(t$Scenario), collapse = "/"),
           vol_logs = vol_logs, vol_tally = vol_tally,
           ok = isTRUE(n >= 2L) && isTRUE(abs(vol_logs - vol_tally) < 0.01))
    }
    tally1 <- tally_state(app, read_logs_csv(app))
    c27 <- list(logs1 = if (tally1$ok) "tbl_tally:ok" else "tbl_tally:0")
    c27$len1 <- figs(app, "fig_log_lengths")   # Phase 2: the log length distribution (Logs tab)
    shoot(app, "02b_log_tally")
    ctrl14 <- js(app, paste0("document.querySelectorAll('#tbl_selected_logs .rt-pagination, ",
                             "#tbl_selected_logs [aria-sort], #tbl_selected_logs_sort, #tbl_selected_logs_next').length"))
    csv14 <- read_logs_csv(app)
    ok14 <- !nzchar(e14) && !is.na(n_logs) && identical(n_rows14, min(20L, n_logs)) &&
      grepl("Scenario A", note14, fixed = TRUE) && identical(ctrl14, 0L) &&
      all(c("Plot", "StartPos", "StopPos", "Assortment") %in% h14) &&
      !any(grepl("_", h14, fixed = TRUE)) && !any(c("ProductType", "ProductKey") %in% h14) &&
      isTRUE(js(app, "document.getElementById('dl_hpr_a') === null")) &&   # NFI run: no .hpr export button
      !is.null(csv14) && identical(nrow(csv14), n_logs) && identical(unique(csv14$Scenario), "A") &&
      all(c("Scenario", "Plot", "StartPos", "StopPos") %in% names(csv14))
    rec(browser_checks[10], if (ok14) PASS else FAIL,
        sprintf("label: '%s' | preview rows: %s | sorting/paging controls: %s | logs in run: %s | CSV rows %s, scenarios %s | headers: %s%s",
                trimws(note14), n_rows14, ctrl14, n_logs, if (is.null(csv14)) NA else nrow(csv14),
                if (is.null(csv14)) "" else paste(unique(csv14$Scenario), collapse = "/"), hdr14, err_txt(e14)))
    # 15. Trees and Products tabs after a single run (results restructure,
    # 2026-10-23): figures, the paged trees table (all trees), a row click
    # draws the cutting pattern (A only), the CSVs; Products: the merged
    # table with all five columns, the type table and the contribution
    # figure; none of the removed outputs.
    e15 <- try_drv({ app$set_inputs(result_tabs = "trees"); app$wait_for_idle() })
    c27$trees1 <- figs(app, c("fig_distributions", "fig_dbh_class"))
    # Value by DBH class: the species totals ("All") add up to the run's trees.
    dbh15 <- tryCatch(utils::read.csv(app$get_download("dl_dbh_class"), check.names = FALSE,
                                      stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                      error = function(e) NULL)
    n_trees_rows <- n_tbl_rows(app, "tbl_trees")
    pager15 <- txt(app, "#tbl_trees_info")
    hdr15 <- headers_of(app, "tbl_trees")
    n_trees_run <- suppressWarnings(as.integer(gsub("[^0-9]", "", js(app, paste0(
      "(function(){ var f = Array.from(document.querySelectorAll('#summary_cards .ro-field'))",
      ".find(function(e){ return e.querySelector('.ro-label').textContent.trim() === 'Trees'; });",
      " return f ? f.querySelector('.ro-value').textContent : ''; })()")))))
    read_trees_csv <- function(app) {
      f <- tryCatch(app$get_download("dl_trees"), error = function(e) NULL)
      if (is.null(f)) return(NULL)
      utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    }
    csv15 <- read_trees_csv(app)
    app$run_js("var r = document.querySelector('#tbl_trees .rt-tbody .rt-tr-group:nth-child(2) .rt-td'); if (r) r.click();")
    app$wait_for_idle()
    stem15 <- figs(app, "stem_compare")
    note15 <- txt(app, "#stem_note")
    removed15 <- js(app, paste0("document.querySelectorAll('#tbl_per_m3, #tbl_per_m3_assortment, #tbl_dbh_class, ",
                                "#tree_logs, #trees_preview_note, #bar_volume_share, #bar_per_m3_contribution, ",
                                "#tbl_cmp_delta, #cmp_patterns, #tbl_cmp_trees, #cmp_tree, #bar_value_by_product').length"))
    page_text <- js(app, "document.body.textContent")
    per_ha_hits <- regmatches(page_text, gregexpr("(?i)per ha\\b|ha-expanded", page_text, perl = TRUE))[[1]]
    shoot(app, "03_trees")
    e15b <- try_drv({ app$set_inputs(result_tabs = "products"); app$wait_for_idle() })
    hdr15p <- headers_of(app, "tbl_products")
    n_type15 <- n_tbl_rows(app, "tbl_per_m3_type")
    fig15p <- figs(app, "bar_nok_contribution")
    cmp15p <- txt(app, "#cmp_value_ui")
    shoot(app, "03b_products")
    ok15 <- !nzchar(paste0(e15, e15b)) && fig_ok(c27$trees1) &&
      identical(n_trees_rows, min(15L, n_trees_run)) &&
      grepl(sprintf("of %s", format(n_trees_run, big.mark = ",")), pager15, fixed = TRUE) &&
      !grepl("Status", hdr15, fixed = TRUE) && grepl("Logs", hdr15, fixed = TRUE) &&
      grepl("Value (NOK)", hdr15, fixed = TRUE) && !grepl("Value A", hdr15, fixed = TRUE) &&
      !is.null(csv15) && identical(nrow(csv15), n_trees_run) && identical(unique(csv15$Scenario), "A") &&
      !"Status" %in% names(csv15) && fig_ok(stem15) && !grepl("Scenario A and B", note15, fixed = TRUE) &&
      identical(removed15, 0L) && length(per_ha_hits) == 0L && !is.null(dbh15) &&
      identical(as.integer(sum(dbh15$Trees[dbh15$`DBH class (cm)` == "All"])), n_trees_run) &&
      identical(hdr15p, "Assortment|Logs|Price volume (m³)|Value (NOK)|NOK per m³ (contribution)|Volume share (%)") &&
      isTRUE(n_type15 >= 2L) && fig_ok(fig15p) && !nzchar(trimws(cmp15p))
    rec(browser_checks[11], if (ok15) PASS else FAIL,
        sprintf("Trees: figures %s | table rows %s (pager '%s', run %s trees) | headers: %s | CSV rows %s, scenarios %s | row click: stem figure %s, note '%s' | removed outputs present: %s | per-ha text hits: %d | DBH CSV trees %s | Products: headers '%s', type rows %s, contribution figure %s, value change shown '%s'%s",
                c27$trees1, n_trees_rows, trimws(pager15), n_trees_run, hdr15,
                if (is.null(csv15)) NA else nrow(csv15),
                if (is.null(csv15)) "" else paste(unique(csv15$Scenario), collapse = "/"),
                stem15, trimws(note15), removed15, length(per_ha_hits),
                if (is.null(dbh15)) NA else sum(dbh15$Trees[dbh15$`DBH class (cm)` == "All"]),
                hdr15p, n_type15, fig15p, trimws(cmp15p), err_txt(e15, e15b)))
    stale_before <- txt(app, "#stale_notice")
    # 16. upload B (spruce sawlog x1.2), automatic comparison
    wide_csv <- tempfile(fileext = ".csv")
    g16 <- pm_to_grid(H$APP_PM_DEFAULT, "spruce_sawlog") * 1.2
    utils::write.csv(cbind(spruce_sawlog = rownames(g16), g16), wide_csv, row.names = FALSE)
    e16 <- try_drv({
      app$set_inputs(main_nav = "prices")
      pick(app, "b", "spruce_sawlog")             # the upload is for the selected assortment
      app$upload_file(pm_b_upload = wide_csv)
      app$wait_for_idle()
    })
    msg16 <- txt(app, "#msg_b")
    e16a <- ""
    n_hl  <- n_cells(app, "pm_b", ".pm-diff")
    n_hl_a <- n_cells(app, "pm_a", ".pm-diff")
    b16   <- dl_pm(app, "dl_b_long")
    n_diff16 <- if (is.null(b16)) NA else pm_n_diff(H$APP_PM_DEFAULT, b16)
    shoot(app, "04_prices_B_own")
    e16b <- try_drv({ app$set_inputs(main_nav = "simulator"); run(app) })
    cmp_tab16 <- tab_shown(app, "comparison")
    tabs16 <- result_tabs_txt(app)
    e16c <- ""
    e16d <- try_drv({ app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle() })
    changed16 <- js(app, paste0("(function(){ var f = Array.from(document.querySelectorAll('#summary_cards .ro-field'))",
                                ".find(function(e){ return e.querySelector('.ro-label').textContent.trim() === 'Changed cutting patterns'; });",
                                " return f ? f.querySelector('.ro-value').textContent.trim() : ''; })()"))
    sum16 <- js(app, paste0("Array.from(document.querySelectorAll('#summary_cards .section-hdr, #summary_cards .ro-label'))",
                            ".map(function(e){ return e.textContent.trim(); }).join('|')"))
    shoot(app, "04b_summary_A_and_B")
    # The run's cards (first row): every label on one line (not cut off) and
    # one height (2026-10-23).
    c16 <- list()
    c16$cards <- js(app, paste0("(function(){ var r = document.querySelector('#summary_cards .ro-row');",
      " if (!r) return ''; var f = Array.from(r.querySelectorAll(':scope > .ro-field'));",
      " return f.map(function(e){ var l = e.querySelector('.ro-label');",
      " return (l.scrollWidth <= l.clientWidth + 1 ? 1 : 0) + ':' + e.offsetHeight; }).join(','); })()"))
    cards16 <- strsplit(or_default(c16$cards, ""), ",", fixed = TRUE)[[1]]
    c16$cards_ok <- length(cards16) >= 4L && all(startsWith(cards16, "1:")) &&
      length(unique(sub("^[01]:", "", cards16))) == 1L
    grid16 <- js(app, paste0("Array.from(document.querySelectorAll('#summary_cards .summary-scen .ro-row'))",
                             ".map(function(r){ return getComputedStyle(r).gridTemplateColumns.split(' ').length; }).join(',')"))
    csv16 <- read_logs_csv(app)
    tcsv16 <- read_trees_csv(app)
    exp16 <- paste(c("Run time", "Plots", "Trees", "Trees with logs (A / B)",
                     "Changed cutting patterns", "Scenario A", "Total value (NOK)",
                     "Commercial volume (m³ u.b.)",
                     "NOK per m³", "Price volume (m³)", "Scenario B", "Total value (NOK)",
                     "Commercial volume (m³ u.b.)", "NOK per m³", "Price volume (m³)"), collapse = "|")
    e27 <- try_drv({
      app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(tally_scenario = "diff", wait_ = FALSE); app$wait_for_idle()
      tally2 <- tally_state(app, csv16)
      c27$tally_diff_hdr <- txt(app, "#tally_scenario_ui")
      c27$tally_signed <- isTRUE(js(app, "/[+-][0-9]/.test(document.querySelector('#tbl_tally').innerText)"))
      c27$logs2 <- if (tally2$ok && identical(tally2$scen, "A/B") && grepl("B − A", c27$tally_diff_hdr, fixed = TRUE))
        "tbl_tally:ok" else "tbl_tally:0"
      shoot(app, "05a_log_tally_B_minus_A")
      app$set_inputs(tally_scenario = "A", wait_ = FALSE); app$wait_for_idle()
      c27$len2 <- figs(app, "fig_log_lengths")   # A above B
      c27$len2_h <- js(app, "document.querySelector('#fig_log_lengths').offsetHeight")
      shoot(app, "phase2_02_logs_tab_comparison")
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      c27$trees2 <- figs(app, c("fig_distributions", "fig_dbh_class"))
    })
    # Trees after the comparison: Value A, Value B and Change; sorted by
    # change (largest first); a row click draws A and B. Products: value and
    # contribution A, B, change, volume share A, B; the value-change figure;
    # the products CSV with every column; the changed-trees CSV.
    e16e <- try_drv({
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      c16$hdr <- headers_of(app, "tbl_trees")
      app$set_inputs(tbl_trees_sort = sort_value(app, "tbl_trees", "Change (NOK) ↓"), wait_ = FALSE)
      app$wait_for_idle()
      h <- strsplit(or_default(c16$hdr, ""), "|", fixed = TRUE)[[1]]
      ci <- match("Change (NOK)", h[nzchar(h)])
      rws <- strsplit(strsplit(or_default(rows_txt(app, "tbl_trees"), ""), "|", fixed = TRUE)[[1]], ";", fixed = TRUE)
      c16$change <- suppressWarnings(as.numeric(gsub(",", "", vapply(rws, function(r) r[ci + 1L], ""))))  # + the selection cell
      app$run_js("var r = document.querySelector('#tbl_trees .rt-tbody .rt-tr-group .rt-td:nth-child(3)'); if (r) r.click();")
      app$wait_for_idle()
      c16$stem <- figs(app, "stem_compare")
      c16$note <- txt(app, "#stem_note")
      c16$changed_csv <- tryCatch(nrow(utils::read.csv(app$get_download("dl_changed_trees"), check.names = FALSE)),
                                  error = function(e) NA)
      shoot(app, "05_trees_comparison")
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
      c16$prod_hdr <- headers_of(app, "tbl_products")
      c16$delta_fig <- figs(app, "bar_cmp_delta")
      c16$contrib_fig <- figs(app, "bar_nok_contribution")
      c16$csv <- tryCatch(names(utils::read.csv(app$get_download("dl_products"), check.names = FALSE)),
                          error = function(e) character(0))
      shoot(app, "05b_products_comparison")
    })
    stem_img <- if (fig_ok(c16$stem)) 1L else 0L
    pat_txt <- changed16
    nerr16   <- n_err_notif(app)
    exp_prod16 <- paste(c("Assortment", "Value A (NOK)", "Value B (NOK)", "Value change (NOK)",
                          "NOK per m³ A (contribution)", "NOK per m³ B (contribution)",
                          "NOK per m³ change (contribution)", "Volume share A (%)", "Volume share B (%)"), collapse = "|")
    ch16 <- c16$change[!is.na(c16$change)]
    ok16b <- grepl("Value A (NOK)|Value B (NOK)|Change (NOK)", c16$hdr, fixed = TRUE) &&
      length(ch16) >= 2L && !is.unsorted(rev(ch16)) && grepl("Scenario A and B", c16$note, fixed = TRUE) &&
      isTRUE(c16$changed_csv >= 1L) && identical(c16$prod_hdr, exp_prod16) && fig_ok(c16$delta_fig) &&
      fig_ok(c16$contrib_fig) &&
      all(c("Logs A", "Logs B", "Logs change", "Price volume A (m³)", "Price volume change (m³)",
            "Volume share change (%)") %in% c16$csv)
    # Handsontable renders the visible rows only: the DOM shows some
    # highlighted cells; B's download has all 351 cells changed.
    ok16 <- !nzchar(paste0(e16, e16a, e16b, e16c)) && isTRUE(n_hl > 0L) && isTRUE(n_hl_a > 0L) &&
      isTRUE(n_diff16 == 351) &&
      !cmp_tab16 && identical(tabs16, RESULT_TABS) && ok16b &&
      grepl("^[0-9,]+$", pat_txt) && isTRUE(c16$cards_ok) &&
      isTRUE(stem_img >= 1L) && identical(nerr16, 0L) && identical(sum16, exp16) &&
      !nzchar(paste0(e16d, e16e)) && identical(grid16, "2,2") &&
      !is.null(csv16) && identical(sort(unique(csv16$Scenario)), c("A", "B")) &&
      all(table(csv16$Scenario) > 0) &&
      !is.null(tcsv16) && identical(sort(unique(tcsv16$Scenario)), c("A", "B")) &&
      identical(sum(tcsv16$Scenario == "A"), sum(tcsv16$Scenario == "B"))
    rec(browser_checks[12], if (ok16) PASS else FAIL,
        sprintf("summary: '%s' (cards one-line label:height %s) | grid columns per scenario: %s | logs CSV: %s | trees CSV: %s | highlighted cells B %s, A %s; cells differing in B's download: %s | upload: '%s' | comparison tab: %s, tabs '%s' | changed pattern: '%s' | stem diagram: %s | trees: headers %s, sorted by change %s (%d rows), note '%s', changed-trees CSV %s rows | products: headers '%s', value change figure %s, CSV columns %d | error notifications: %s%s",
                sum16, c16$cards, grid16,
                if (is.null(csv16)) "none" else paste(sprintf("%s %d", names(table(csv16$Scenario)), table(csv16$Scenario)), collapse = ", "),
                if (is.null(tcsv16)) "none" else paste(sprintf("%s %d", names(table(tcsv16$Scenario)), table(tcsv16$Scenario)), collapse = ", "),
                n_hl, n_hl_a, n_diff16, substr(trimws(msg16), 1, 100), cmp_tab16, tabs16,
                trimws(pat_txt), isTRUE(stem_img >= 1L), substr(or_default(c16$hdr, ""), 1, 120),
                length(ch16) >= 2L && !is.unsorted(rev(ch16)), length(ch16), trimws(c16$note), c16$changed_csv,
                c16$prod_hdr, c16$delta_fig, length(c16$csv), nerr16, err_txt(e16, e16a, e16b, e16c, e16e)))
    # 17. Spruce pulpwood removed from B only: B's run lacks it, and the
    # comparison (different assortments) is by type and shared assortments
    flat17 <- ""; st17 <- ""; c17 <- list()
    e17 <- try_drv({
      app$set_inputs(main_nav = "prices")
      pick(app, "b", "spruce_pulpwood"); app$click("asm_remove_b"); app$wait_for_idle()
      app$click("asm_rm_one"); app$wait_for_idle()
      flat17 <- txt(app, "#msg_b")
      # Each table lists its own assortments only; with spruce pulpwood
      # selected (in A), B shows no detail panel and no row for it.
      pick(app, "a", "spruce_pulpwood")
      missing17 <- list(panel_b = isTRUE(js(app, "document.getElementById('asm_detail_b').offsetParent === null")),
                        row_b = grepl("Pulpwood\\s+Spruce", txt(app, "#tbl_asm_b")),
                        struck = js(app, "document.querySelectorAll('.asm-none').length"))
      st17 <- paste("B products:", paste(unique(scen_of(app, "b")$pm$ProductKey), collapse = ","))
      app$set_inputs(main_nav = "simulator"); run(app)
      # Different assortments (2026-10-22; tabs 2026-10-23): Products shows
      # every assortment with "–" where a scenario lacks it, the type table,
      # and the value change of the shared assortments only.
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
      c17$prod <- rows_txt(app, "tbl_products")
      c17$types <- txt(app, "#tbl_per_m3_type")
      c17$shared_fig <- figs(app, "bar_cmp_delta")
      c17$prod_figs <- js(app, "document.querySelectorAll('#bar_nok_contribution img').length")
      c17$vol_fig <- js(app, "document.querySelectorAll('#bar_volume_by_product, #bar_value_by_product').length")
      c17$cmp_tab <- tab_shown(app, "comparison")
      shoot(app, "15_different_assortments_products")
      app$run_js("document.getElementById('cmp_value_ui').scrollIntoView()"); app$wait_for_idle()
      shoot(app, "16_different_assortments_value_change")
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      shoot(app, "17_different_assortments_trees")
      # Log tally: spruce pulpwood (A only) offers A only; spruce sawlog B − A.
      app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(tally_key = "spruce_pulpwood", wait_ = FALSE); app$wait_for_idle()
      radio <- function() js(app, "Array.from(document.querySelectorAll('#tally_scenario input')).map(function(i){ return i.value; }).join(',')")
      c17$radio_pulp <- radio()
      c17$opt_pulp <- js(app, "Array.from(document.querySelectorAll('#tally_key option')).filter(function(o){ return o.value === 'spruce_pulpwood'; }).map(function(o){ return o.textContent; }).join('')")
      shoot(app, "18_different_assortments_tally_A_only")
      app$set_inputs(tally_key = "spruce_sawlog", wait_ = FALSE); app$wait_for_idle()
      c17$radio_saw <- radio()
      c17$note <- txt(app, "#tally_note")
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
    })
    rows17 <- strsplit(or_default(c17$prod, ""), "|", fixed = TRUE)[[1]]
    prod_pulp <- grep("^Spruce pulpwood", rows17, value = TRUE)
    # Products columns: Assortment; Value A; Value B; Value change; ...
    pp <- if (length(prod_pulp)) strsplit(prod_pulp[1], ";")[[1]] else character(0)
    ok17 <- !nzchar(e17) && !isTRUE(c17$cmp_tab) && fig_ok(c17$shared_fig) &&
      !any(grepl("(type)", rows17, fixed = TRUE)) && any(startsWith(rows17, "Spruce sawlog")) &&
      grepl("Sawlog", c17$types, fixed = TRUE) && grepl("Pulpwood", c17$types, fixed = TRUE) &&
      !grepl("Spruce pulpwood", c17$types, fixed = TRUE) &&
      length(pp) >= 4L && isTRUE(as.numeric(gsub(",", "", pp[2])) > 0) && identical(pp[3], "–") &&
      identical(pp[4], "–") &&
      identical(c17$prod_figs, 1L) && identical(c17$vol_fig, 0L) &&
      identical(c17$radio_pulp, "A") && grepl("(A only)", c17$opt_pulp, fixed = TRUE) &&
      identical(c17$radio_saw, "A,B,diff") &&
      identical(trimws(c17$note), "Price-diameter class (cm) vs length class (dm)") &&
      isTRUE(missing17$panel_b) && !isTRUE(missing17$row_b) && identical(missing17$struck, 0L) &&
      !grepl("spruce_pulpwood", st17, fixed = TRUE) && identical(n_err_notif(app), 0L)
    rec(browser_checks[13], if (ok17) PASS else FAIL,
        sprintf("'%s' (%s) | selected in A: B panel hidden %s, row in B %s, struck rows %s | comparison tab %s, shared value-change figure %s | products rows: %s | Products spruce pulpwood: %s; contribution figure %s, value/volume figures %s | tally spruce pulpwood %s ('%s'), spruce sawlog %s, note '%s' | error notifications: %s%s",
                trimws(flat17), trimws(st17), missing17$panel_b, missing17$row_b, missing17$struck,
                isTRUE(c17$cmp_tab), c17$shared_fig, paste(substr(rows17, 1, 40), collapse = " / "),
                paste(pp, collapse = ";"), c17$prod_figs, c17$vol_fig, c17$radio_pulp, c17$opt_pulp, c17$radio_saw, trimws(c17$note),
                n_err_notif(app), err_txt(e17)))
    # 18. stale notice after a price edit (reset A)
    e18 <- try_drv({
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
    })
    stale_after_run <- txt(app, "#stale_notice")
    e18b <- try_drv({
      app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle(); pick(app, "b", "spruce_sawlog")
      app$click("pm_b_reset"); app$wait_for_idle()
      app$set_inputs(main_nav = "simulator"); app$wait_for_idle()
    })
    stale_after_edit <- txt(app, "#stale_notice")
    msg18 <- "Assortments or prices changed since this run"
    ok18 <- !nzchar(paste0(e18, e18b)) && !grepl(msg18, stale_before, fixed = TRUE) &&
      !grepl(msg18, stale_after_run, fixed = TRUE) && grepl(msg18, stale_after_edit, fixed = TRUE)
    rec(browser_checks[14], if (ok18) PASS else FAIL,
        sprintf("after runs: '%s' / '%s' | after resetting B: '%s'%s", trimws(stale_before),
                trimws(stale_after_run), trimws(stale_after_edit), err_txt(e18, e18b)))
    # 19. no "default"/"scenario" words in the page text (both tabs are in
    # the DOM; hidden tabs count too)
    page19 <- js(app, "document.body.textContent")
    hits19 <- unique(regmatches(page19, gregexpr("(?i)\\b(default|price set)\\b", page19, perl = TRUE))[[1]])
    rec(browser_checks[15], if (length(hits19) == 0L) PASS else FAIL,
        if (length(hits19) == 0L) "none found"
        else sprintf("found: %s", paste(hits19, collapse = ", ")))
    # 20. custom assortment through the form
    form20 <- NA; blur20 <- NA
    n_b0 <- n_tbl_rows(app, "tbl_asm_b")   # B's own rows before (no spruce pulpwood since check 17)
    e20 <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$click("asm_add_a"); app$wait_for_idle()
      form20 <<- js(app, paste0("['asm_type','asm_step','asm_adjust'].filter(function(i){",
                                " return !!document.getElementById(i); }).length"))
      blur20 <<- js(app, paste0("(function(){ var m = document.querySelector('#shiny-modal-wrapper .modal');",
                                " return m ? getComputedStyle(m).backdropFilter : 'no modal'; })()"))
      shoot(app, "03b_assortment_dialog")
      app$set_inputs(asm_name = "Poles", asm_species = "spruce",
                     asm_min_top = 12, asm_max_top = 30, asm_price_type = "matrix",
                     asm_min_len = 600, asm_max_len = 1000,
                     asm_method = "Mid", asm_price = 2000,
                     wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
    })
    msg20a <- txt(app, "#msg_a")
    # The form offers Mid (a value outside the choices would not be kept).
    d20 <- scen_of(app, "a")$defs
    method20 <- if (is.null(d20)) NA else d20$VolumeDiameterCategory[d20$Name == "Poles"][1]
    n_asm1 <- n_tbl_rows(app, "tbl_asm_a")
    # The new assortment is selected, its matrix grid shown.
    pm_opts <- paste(head_of(app, "a"), if (shown(app, "#pm_a")) "grid" else "no grid")
    a_before <- dl_pm(app, "dl_a_long")
    e20b <- try_drv({
      pick(app, "a", "spruce_poles")
      app$click("asm_edit_a"); app$wait_for_idle()
      app$set_inputs(asm_max_top = 28, wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
    })
    msg20b <- txt(app, "#msg_a")
    a_after <- dl_pm(app, "dl_a_long")
    kept <- !is.null(a_before) && identical(a_before[a_before$ProductKey == "spruce_poles", ],
                                            a_after[a_after$ProductKey == "spruce_poles", ])
    shoot(app, "06_assortments_custom")
    e20c <- try_drv({
      app$set_inputs(main_nav = "simulator"); run(app)
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
    })
    prod20 <- txt(app, "#tbl_products")
    e20d <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$click("asm_reset_a"); app$wait_for_idle()
    })
    n_asm_reset <- n_tbl_rows(app, "tbl_asm_a")
    a_reset <- scen_of(app, "a")$pm
    base_back <- !is.null(a_reset) && identical(
      pm_minimal(a_reset[a_reset$ProductKey != "spruce_poles", ]), pm_minimal(H$APP_PM_DEFAULT))
    e20e <- try_drv({
      pick(app, "a", "spruce_poles"); app$click("asm_remove_a"); app$wait_for_idle()
      app$click("asm_rm_both"); app$wait_for_idle()
    })
    n_asm_rm <- n_tbl_rows(app, "tbl_asm_a")
    n_asm_rm_b <- n_tbl_rows(app, "tbl_asm_b")
    ok20 <- !nzchar(paste0(e20, e20b, e20c, e20d, e20e)) &&
      grepl("Added Spruce poles", msg20a, fixed = TRUE) && identical(n_asm1, 7L) &&
      identical(method20, "Mid") &&
      identical(pm_opts, "Spruce poles grid") &&
      grepl("Saved Spruce poles", msg20b, fixed = TRUE) && kept &&
      grepl("Spruce poles", prod20, fixed = TRUE) &&
      identical(n_asm_reset, 7L) && base_back && identical(n_asm_rm, 6L) &&
      identical(n_asm_rm_b, n_b0) && identical(form20, 0L) && identical(blur20, "none") &&
      identical(n_err_notif(app), 0L)
    rec(browser_checks[16], if (ok20) PASS else FAIL,
        sprintf("form: removed fields present %s, backdrop filter '%s' | add: '%s', volume method %s, rows %s, selected: %s | edit: '%s', prices kept %s | selected in run: %s | reset: rows %s, base prices back %s | remove from both: rows A %s, B %s (before: %s)%s",
                form20, blur20, trimws(msg20a), method20, n_asm1, pm_opts, trimws(msg20b), kept,
                grepl("Spruce poles", prod20, fixed = TRUE), n_asm_reset, base_back, n_asm_rm, n_asm_rm_b, n_b0,
                err_txt(e20, e20b, e20c, e20d, e20e)))
    # 21. uploaded tree list (template)
    tl_csv <- tempfile(fileext = ".csv")
    tree_list_template(tl_csv)
    e21 <- try_drv({
      app$set_inputs(main_nav = "simulator")
      app$set_inputs(tree_source = "list", wait_ = FALSE)
      app$upload_file(tl_upload = tl_csv)
      app$wait_for_idle()
    })
    tl_panel <- txt(app, "#tl_validation")
    c26$tl <- txt(app, "#tree_counts")
    e21b <- try_drv({
      run(app)
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
    })
    banner21 <- txt(app, "#height_banner")
    trees21  <- txt(app, "#tbl_trees")
    nerr21   <- n_err_notif(app)
    shoot(app, "07_tree_list")
    ok21 <- !nzchar(paste0(e21, e21b)) &&
      grepl("12 trees, 12 within taperNOR's range", tl_panel, fixed = TRUE) &&
      grepl("FinnHoyde (calibrated, n = 4)", tl_panel, fixed = TRUE) &&
      grepl("FinnHoyde, uncalibrated", banner21, fixed = TRUE) &&
      grepl("Height source", trees21, fixed = TRUE) && identical(nerr21, 0L)
    rec(browser_checks[17], if (ok21) PASS else FAIL,
        sprintf("panel: '%s' | banner: '%s' | Height source column: %s | error notifications: %s%s",
                substr(trimws(tl_panel), 1, 160), substr(trimws(banner21), 1, 120),
                grepl("Height source", trees21, fixed = TRUE), nerr21, err_txt(e21, e21b)))
    # 26. live tree counts: NFI box before the first run vs that run's
    # Plots and Trees (spruce, HKL 4, bonitet 17, 5 sampled plots); the
    # species group changes it; the tree list counts its 12 trees.
    sp26 <- num_of(c26$spruce, "([0-9,]+) spruce tree")
    pl26 <- num_of(c26$spruce, "([0-9,]+) plots?")
    tl26 <- sum(num_of(c26$tl, "([0-9,]+) spruce"), num_of(c26$tl, "([0-9,]+) pine"))
    ok26 <- !nzchar(e26) && identical(sp26, n_trees_run) && identical(as.character(pl26), plots_val) &&
      grepl("pine", c26$both, fixed = TRUE) && identical(c26$back, c26$spruce) &&
      grepl("Estimated run time", c26$spruce, fixed = TRUE) && identical(tl26, 12L)
    rec(browser_checks[22], if (ok26) PASS else FAIL,
        sprintf("box: '%s' (run: %s plots, %s trees) | both species: '%s' | back to spruce: same %s | tree list: '%s'%s",
                gsub("\\s+", " ", trimws(c26$spruce)), plots_val, n_trees_run, gsub("\\s+", " ", trimws(c26$both)),
                identical(c26$back, c26$spruce), gsub("\\s+", " ", trimws(c26$tl)), err_txt(e26)))
    # 28. altitude slider (the app samples APTER_N_PLOTS plots, so the band is
    # compared with the selection code's own text for that band)
    in28  <- list(species_group = "spruce", hkl_group = "4", abonitet = "17", fylnr_sel = "__all__")
    sel28 <- select_nfi_trees(H$trees_meta, H$build_run_params(
      c(in28, list(hoh_range = c(200, 400))), n_plots = as.integer(Sys.getenv("APTER_N_PLOTS", "5"))))
    exp28 <- H$tree_counts(sel28$trees$species_group, length(sel28$plots))$text
    ok28 <- !nzchar(e28) && identical(as.numeric(c28$start), as.numeric(H$HOH_RANGE)) &&
      grepl(exp28, c28$band, fixed = TRUE) && !identical(c28$band, c26$spruce) &&
      identical(c28$full, c26$spruce) && identical(as.numeric(c28$end), as.numeric(H$HOH_RANGE)) &&
      length(c28$edges_full) == 2L && all(unlist(c28$edges_full) >= 0)
    rec(browser_checks[24], if (ok28) PASS else FAIL,
        sprintf("start %s m, labels inside the input edges by %s px (left, right) | 200-400 m: '%s' (expected '%s') | full range again: same as before %s, slider %s m%s",
                paste(c28$start, collapse = "-"), paste(unlist(c28$edges_full), collapse = ", "),
                gsub("\\s+", " ", trimws(c28$band)), exp28,
                identical(c28$full, c26$spruce), paste(c28$end, collapse = "-"), err_txt(e28)))
    # 24. Scenario B's own assortments (Phase 4d, independent scenarios)
    # B made equal to A as a user does it: download Scenario A, upload to B.
    a_to_b <- function(app) {
      f <- app$get_download("dl_scen_a")
      app$upload_file(scen_b_upload = f); app$wait_for_idle()
    }
    n24 <- list(); msg24 <- ""
    prod24 <- ""; cmp24 <- FALSE
    e24 <- try_drv({
      app$set_inputs(main_nav = "prices"); a_to_b(app)
      n24$start <- marks(app)
      # a rule of B only: spruce sawlog min top 14 cm
      pick(app, "b", "spruce_sawlog"); app$click("asm_edit_b"); app$wait_for_idle()
      app$set_inputs(asm_min_top = 14, wait_ = FALSE); app$click("asm_save"); app$wait_for_idle()
      n24$diff_a <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-diff').length")
      n24$diff_b <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-diff').length")
      # an assortment added to B only
      app$click("asm_add_b"); app$wait_for_idle()
      app$set_inputs(asm_name = "Poles", asm_species = "spruce", asm_price_type = "flat",
                     asm_min_top = 15, asm_min_len = 800, asm_max_len = 1200, asm_price = 900,
                     asm_both = FALSE, wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
      # The B-only row (Poles) is not highlighted: no highlighted row or cell.
      n24$only_b <- js(app, paste0(
        "Array.from(document.querySelectorAll('#tbl_asm_b .rt-tbody .rt-tr'))",
        ".filter(function(r){ return r.textContent.indexOf('Poles') >= 0; })",
        ".map(function(r){ return r.querySelectorAll('.asm-diff').length + r.querySelectorAll('.asm-only').length",
        " + (r.classList.contains('asm-only') ? 1 : 0); }).reduce(function(a, b){ return a + b; }, 0)"))
      n24$a_rows <- n_tbl_rows(app, "tbl_asm_a")
      # pine pulpwood removed from B only
      pick(app, "b", "pine_pulpwood"); app$click("asm_remove_b"); app$wait_for_idle()
      app$click("asm_rm_one"); app$wait_for_idle()
      n24$struck <- js(app, "document.querySelectorAll('.asm-none').length")
      rows_of <- function(id) js(app, sprintf(paste0(
        "Array.from(document.querySelectorAll('#%s .rt-tbody .rt-tr')).map(function(r){",
        " var c = r.querySelectorAll('.rt-td'); return c[0].textContent.trim() + ' ' + c[1].textContent.trim(); }).join('|')"), id))
      n24$rows_a <- rows_of("tbl_asm_a"); n24$rows_b <- rows_of("tbl_asm_b")
      shoot(app, "08_scenarios_A_and_B")
      app$set_inputs(main_nav = "simulator")
      app$set_inputs(tree_source = "nfi", wait_ = FALSE); app$wait_for_idle()
      run(app)
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
      shoot(app, "09_products_A_and_B")
      prod24 <- txt(app, "#tbl_products")
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      shoot(app, "10_trees_comparison")
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
      cmp24 <- compared_now(app)
    })
    e24b <- try_drv({
      app$set_inputs(main_nav = "prices")
      pick(app, "a", "spruce_sawlog"); app$click("asm_copy_one_a"); app$wait_for_idle()
      msg24 <- txt(app, "#msg_a")
      n24$sawlog_eq <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-diff').length")
      a_to_b(app)
      n24$end <- marks(app)
    })
    ok24 <- !nzchar(paste0(e24, e24b)) && identical(n24$start, 0L) &&
      isTRUE(n24$diff_a >= 1L) && isTRUE(n24$diff_b >= 1L) &&
      identical(n24$only_b, 0L) && identical(n24$a_rows, 6L) && identical(n24$struck, 0L) && cmp24 &&
      identical(n24$rows_a, "Sawlog Spruce|Pulpwood Spruce|Waste Spruce|Sawlog Pine|Pulpwood Pine|Waste Pine") &&
      identical(n24$rows_b, "Sawlog Spruce|Pulpwood Spruce|Waste Spruce|Poles Spruce|Sawlog Pine|Waste Pine") &&
      grepl("Spruce poles", prod24, fixed = TRUE) &&
      grepl("Value A", prod24, fixed = TRUE) && grepl("Spruce sawlog is equal to Scenario B", msg24, fixed = TRUE) &&
      identical(n24$end, 0L) && identical(n_err_notif(app), 0L)
    rec(browser_checks[20], if (ok24) PASS else FAIL,
        sprintf("rows A: %s | rows B: %s | start marks %s | B rule highlighted: A %s, B %s | B-only row highlighted (expected 0): %s (A rows %s) | struck rows %s | compared %s, poles in products %s | Make assortment equal to B (spruce sawlog): '%s' | after Scenario A downloaded and uploaded to B: %s marks%s",
                n24$rows_a, n24$rows_b, n24$start, n24$diff_a, n24$diff_b, n24$only_b, n24$a_rows, n24$struck,
                cmp24, grepl("Spruce poles", prod24, fixed = TRUE), substr(trimws(msg24), 1, 120),
                n24$end, err_txt(e24, e24b)))
    # 27. figures (images with a width) for the single run and the comparison
    ok27 <- !nzchar(e27) && all(vapply(c27[c("logs1", "trees1", "logs2", "trees2", "len1", "len2")],
                                       function(f) isTRUE(fig_ok(f)), logical(1)))
    rec(browser_checks[23], if (ok27) PASS else FAIL,
        paste0(sprintf("single run: tally %s (%s rows, volume %.3f vs logs %.3f m³), figures %s | comparison: tally %s (scenarios %s, B − A signed %s), figures %s (id:image width px)%s",
                c27$logs1, tally1$rows, tally1$vol_tally, tally1$vol_logs, c27$trees1,
                c27$logs2, if (exists("tally2")) tally2$scen else "", isTRUE(c27$tally_signed),
                c27$trees2, err_txt(e27)), sprintf(" | log length figure: single run %s, comparison %s, %s px high",
                c27$len1, c27$len2, c27$len2_h)))
    # 32. Copy from A / Copy from B (2026-10-22): a flat price edited in B
    # (444) makes B differ; "Copy from A" asks first, Cancel keeps B, Copy
    # makes B equal to A (nothing highlighted).
    cancel <- function(app) {
      app$run_js("document.querySelector('.modal-footer [data-bs-dismiss], .modal-footer [data-dismiss]').click()")
      app$wait_for_idle()
    }
    c32 <- list()
    e32 <- try_drv({
      app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
      pick(app, "b", "spruce_pulpwood")
      app$run_js("var i = document.querySelector('#flat_b input'); i.value = '444'; $(i).trigger('change');")
      app$wait_for_idle()
      c32$before <- scen_of(app, "b")
      c32$btn_a <- txt(app, "#scen_copy_a"); c32$btn_b <- txt(app, "#scen_copy_b")
      app$click("scen_copy_b"); app$wait_for_idle()
      c32$title <- txt(app, ".modal-title"); c32$body <- txt(app, ".modal-body")
      shoot(app, "19_copy_from_A_dialog")
      cancel(app)
      c32$after_cancel <- scen_of(app, "b")
      app$click("scen_copy_b"); app$wait_for_idle()
      app$click("scen_copy_ok"); app$wait_for_idle()
      c32$msg <- txt(app, "#msg_b")
      c32$a <- scen_of(app, "a"); c32$b <- scen_of(app, "b")
      c32$marks <- marks(app)
    })
    same_scen <- function(x, y) !is.null(x) && !is.null(y) && isTRUE(all.equal(x$defs, y$defs, check.attributes = FALSE)) &&
      isTRUE(all.equal(pm_minimal(x$pm), pm_minimal(y$pm), check.attributes = FALSE))
    ok32 <- !nzchar(e32) && identical(trimws(c32$btn_a), "Copy from B") && identical(trimws(c32$btn_b), "Copy from A") &&
      grepl("Copy Scenario A to Scenario B", c32$title, fixed = TRUE) &&
      grepl("All assortments and prices in Scenario B are replaced by those of Scenario A.", c32$body, fixed = TRUE) &&
      same_scen(c32$before, c32$after_cancel) && !same_scen(c32$before, c32$a) &&
      same_scen(c32$a, c32$b) && grepl("Scenario B is now a copy of Scenario A.", c32$msg, fixed = TRUE) &&
      identical(c32$marks, 0L) && identical(n_err_notif(app), 0L)
    rec(browser_checks[28], if (ok32) PASS else FAIL,
        sprintf("buttons '%s' (A), '%s' (B) | dialog '%s': '%s' | Cancel keeps B %s | B differed from A before %s | after Copy: B = A %s, '%s', highlighted %s%s",
                trimws(c32$btn_a), trimws(c32$btn_b), trimws(c32$title), substr(trimws(c32$body), 1, 90),
                same_scen(c32$before, c32$after_cancel), !same_scen(c32$before, c32$a),
                same_scen(c32$a, c32$b), trimws(c32$msg), c32$marks, err_txt(e32)))
    # 29. Import from .hpr (last: it replaces Scenario B). The dialog opens
    # from A; a broken file shows its error and cannot be applied; from B
    # the fixture's species and products are prefilled, and Replace
    # Scenario B changes B only.
    c29 <- list()
    own_rows <- function(app, s) js(app, sprintf(paste0(
      "Array.from(document.querySelectorAll('#tbl_asm_%s .rt-tbody .rt-tr'))",
      ".filter(function(r){ return !r.classList.contains('asm-none'); })",
      ".map(function(r){ return r.innerText.replace(/\\s+/g, ' ').trim(); }).join(' | ')"), s))
    e29 <- try_drv({
      app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
      c29$a_before <- own_rows(app, "a")
      app$click("hpr_import_a"); app$wait_for_idle()
      c29$title_a <- txt(app, ".modal-title")
      bad <- tempfile(fileext = ".hpr"); writeLines("not an hpr file", bad)
      app$upload_file(hpr_file = bad); app$wait_for_idle()
      c29$err <- txt(app, "#hpr_check")
      c29$bad_disabled <- isTRUE(js(app, "document.getElementById('hpr_apply').disabled"))
      cancel(app)
      app$click("hpr_import_b"); app$wait_for_idle()
      c29$title_b <- txt(app, ".modal-title")
      app$upload_file(hpr_file = file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr"))
      app$wait_for_idle()
      c29$species <- js(app, paste0("Array.from(document.querySelectorAll('#hpr_species select'))",
                                    ".map(function(s){ return s.value; }).join(',')"))
      c29$ticked <- js(app, paste0("Array.from(document.querySelectorAll('#hpr_products input[type=checkbox]'))",
                                   ".map(function(c){ return c.checked ? 1 : 0; }).join('')"))
      c29$ok_enabled <- !isTRUE(js(app, "document.getElementById('hpr_apply').disabled"))
      c29$dialog <- txt(app, ".modal-content")
      shoot(app, "11_hpr_import_dialog")
      app$click("hpr_apply"); app$wait_for_idle()
      c29$open <- js(app, "document.querySelectorAll('.modal.show').length")
      c29$msg <- txt(app, "#msg_b")
      c29$b_rows <- own_rows(app, "b"); c29$a_after <- own_rows(app, "a")
      c29$marks <- marks(app)
      shoot(app, "12_scenario_B_after_hpr_import")
    })
    ok29 <- !nzchar(e29) && grepl("Scenario A", c29$title_a, fixed = TRUE) &&
      grepl("not a readable XML", c29$err, fixed = TRUE) && c29$bad_disabled &&
      grepl("Scenario B", c29$title_b, fixed = TRUE) && identical(c29$species, "pine,spruce,none,none") &&
      identical(c29$ticked, "111111") && c29$ok_enabled && identical(c29$open, 0L) &&
      grepl("Imported 6 assortments from hpr_example_synthetic.hpr into Scenario B.", c29$msg, fixed = TRUE) &&
      !grepl("Warning", c29$msg, fixed = TRUE) && !grepl("Not taken over", c29$dialog, fixed = TRUE) &&
      !grepl("Only for grades set by the operator", c29$dialog, fixed = TRUE) &&
      grepl("REDACTED23", c29$b_rows, fixed = TRUE) && !grepl("Sawlog Spruce", c29$b_rows, fixed = TRUE) &&
      identical(c29$a_after, c29$a_before) && identical(c29$marks, 0L) && identical(n_err_notif(app), 0L)
    rec(browser_checks[25], if (ok29) PASS else FAIL,
        sprintf("from A: '%s'; broken file: '%s', Replace disabled %s | from B: species %s, ticked %s, Replace enabled %s | after Replace: dialog open %s, '%s' | B rows: %s | A unchanged %s | highlighted %s%s",
                c29$title_a, substr(gsub("\\s+", " ", c29$err), 1, 80), c29$bad_disabled, c29$species,
                c29$ticked, c29$ok_enabled, c29$open, substr(gsub("\\s+", " ", c29$msg), 1, 90),
                substr(c29$b_rows, 1, 160), identical(c29$a_after, c29$a_before), c29$marks, err_txt(e29)))
    # 33. NFI comparison with B imported from .hpr (check 29): A and B share
    # only the Waste rows, so the comparison is by type only (no figure of
    # shared assortments, no assortment rows), and the tally offers B only
    # for an imported assortment.
    c33 <- list()
    e33 <- try_drv({
      app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
      run(app)
      app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
      c33$hdr <- txt(app, "#cmp_value_ui")
      c33$shared_fig <- js(app, "document.querySelectorAll('#bar_cmp_delta img').length")
      c33$type_fig <- n_tbl_rows(app, "tbl_per_m3_type")
      c33$rows <- js(app, paste0("Array.from(document.querySelectorAll('#tbl_products .rt-tbody .rt-tr'))",
                                 ".map(function(r){ return r.querySelector('.rt-td').textContent.trim(); }).join('|')"))
      shoot(app, "20_hpr_B_products")
      app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(tally_key = "spruce_140_sca_sagt_mmer", wait_ = FALSE); app$wait_for_idle()
      c33$radio <- js(app, "Array.from(document.querySelectorAll('#tally_scenario input')).map(function(i){ return i.value; }).join(',')")
      shoot(app, "21_hpr_B_tally_B_only")
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
    })
    rows33 <- strsplit(or_default(c33$rows, ""), "|", fixed = TRUE)[[1]]
    # A and B share only the Waste rows: no value-change figure, the note
    # says so; every assortment of A and B is listed, none as "(type)".
    ok33 <- !nzchar(e33) && grepl("No assortments in both scenarios.", c33$hdr, fixed = TRUE) &&
      identical(c33$shared_fig, 0L) && isTRUE(c33$type_fig >= 2L) &&
      length(rows33) >= 4L && !any(grepl("(type)", rows33, fixed = TRUE)) &&
      any(grepl("140 SCA", rows33, fixed = TRUE)) && any(startsWith(rows33, "Spruce sawlog")) &&
      identical(c33$radio, "B") && identical(n_err_notif(app), 0L)
    rec(browser_checks[29], if (ok33) PASS else FAIL,
        sprintf("value change: '%s' | value-change figure %s | type table rows %s | products rows: %s | tally REDACTED25: %s | error notifications %s%s",
                trimws(c33$hdr), c33$shared_fig, c33$type_fig, paste(rows33, collapse = ", "),
                c33$radio, n_err_notif(app), err_txt(e33)))

    # 31. .hpr stems (hpr Phase B), after the import into B (check 29): the
    # source stays NFI until chosen; then the imported file is used with the
    # confirmed species map (groups with stems: FURU, GRAN, LAUV), the
    # fixture's 13 stems give one line and the dropped-stem warnings; a run
    # compares A and B (B was imported) on the 5 stems, without Plot columns.
    c31 <- list()
    e31 <- try_drv({
      c31$src_before <- app$get_value(input = "tree_source")
      app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(tree_source = "hpr", wait_ = FALSE)
      app$wait_for_js("document.querySelector('#hs_validation .pm-msg-ok, #hs_validation .pm-msg-error') !== null",
                      timeout = 60 * 1000)
      app$wait_for_idle()
      c31$file <- txt(app, "#hs_file_ui")
      c31$species <- js(app, paste0("Array.from(document.querySelectorAll('#hs_species_ui select'))",
                                    ".map(function(s){ return s.value; }).join(',')"))
      c31$valid <- txt(app, "#hs_validation")
      c31$n_warn <- js(app, "(document.querySelector('#hs_validation .pm-msg-warn') || {innerText: ''}).innerText.split('Warning:').length - 1")
      c31$counts <- txt(app, "#tree_counts")
      shoot(app, "13_hpr_stems_panel")
      run(app)
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
      c31$summary <- txt(app, "#summary_cards")
      c31$cmp_tab <- compared_now(app)   # a comparison: the Summary counts changed patterns
      shoot(app, "14_hpr_stems_results")
      app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
      c31$logs_hdr <- js(app, "Array.from(document.querySelectorAll('#tbl_selected_logs .rt-th')).map(function(h){ return h.innerText.trim(); }).join('|')")
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      c31$trees_hdr <- js(app, "Array.from(document.querySelectorAll('#tbl_trees .rt-th')).map(function(h){ return h.innerText.trim(); }).join('|')")
      # .hpr export (Phase 5a): one button per scenario after this comparison;
      # B's file holds the 5 bucked stems with logs and no coordinates.
      app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
      c31$hpr_btn <- js(app, "Array.from(document.querySelectorAll('#dl_hpr_ui a')).map(function(a){ return a.innerText.trim(); }).join('|')")
      f_hpr <- tryCatch(app$get_download("dl_hpr_b"), error = function(e) NULL)
      c31$hpr_ok <- !is.null(f_hpr) && {
        tx <- paste(readLines(f_hpr, warn = FALSE), collapse = "\n")
        length(gregexpr("<StemKey>", tx, fixed = TRUE)[[1L]]) == 5L && grepl("<Log>", tx, fixed = TRUE) &&
          !grepl("Latitude|Longitude|StemCoordinates", tx)
      }
      c31$nerr <- n_err_notif(app)
    })
    hdr_cols <- function(h) strsplit(or_default(h, ""), "|", fixed = TRUE)[[1L]]
    ok31 <- !nzchar(e31) && identical(c31$src_before, "nfi") &&
      grepl("Using hpr_example_synthetic.hpr (imported into Scenario B)", c31$file, fixed = TRUE) &&
      identical(c31$species, "pine,spruce,none") &&
      grepl("hpr_example_synthetic.hpr: 13 stems read, 5 used.", c31$valid, fixed = TRUE) &&
      identical(as.integer(c31$n_warn), 4L) && !grepl("Not used", c31$valid, fixed = TRUE) &&
      !grepl("multi-tree", c31$valid, fixed = TRUE) && grepl("4 spruce · 1 pine stems", c31$counts, fixed = TRUE) &&
      identical(c31$hpr_btn, "Download Scenario A (.hpr)|Download Scenario B (.hpr)") && isTRUE(c31$hpr_ok) &&
      grepl("Stems", c31$summary, fixed = TRUE) && !grepl("Plots", c31$summary, fixed = TRUE) &&
      grepl("Scenario B", c31$summary, fixed = TRUE) && c31$cmp_tab &&
      !"Plot" %in% hdr_cols(c31$logs_hdr) && "Tree" %in% hdr_cols(c31$logs_hdr) &&
      !"Plot" %in% hdr_cols(c31$trees_hdr) && "Measured length (m)" %in% hdr_cols(c31$trees_hdr) &&
      identical(c31$nerr, 0L)
    rec(browser_checks[27], if (ok31) PASS else FAIL,
        sprintf("source before: %s | file: '%s' | species %s | '%s' | %s warnings | counts '%s' | summary: %s | compared %s | logs columns: %s | trees columns: %s | error notifications %s%s",
                c31$src_before, substr(trimws(c31$file), 1, 80), c31$species,
                substr(gsub("\\s+", " ", c31$valid), 1, 60), c31$n_warn, trimws(c31$counts),
                substr(gsub("\\s+", " ", c31$summary), 1, 120), c31$cmp_tab,
                substr(or_default(c31$logs_hdr, ""), 1, 80), substr(or_default(c31$trees_hdr, ""), 1, 100),
                c31$nerr, err_txt(e31)))
    # 40. Several .hpr files (Phase 2, 2026-10-28), after 31: the fixture and
    # a copy with every StemKey + 1000 uploaded at once. One total line, the
    # species table merged by name (3 rows), the comparison runs on 10 stems,
    # the Trees table has a File column, and no file name is in the results.
    fx_src <- file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
    c40 <- list()
    e40 <- try_drv({
      fl <- readLines(fx_src, warn = FALSE, encoding = "UTF-8")
      first <- grep("<Stem>", fl, fixed = TRUE)[1L]
      f40 <- file.path(tempfile("hpr40"), "second_upload.hpr"); dir.create(dirname(f40))
      fl2 <- fl
      for (j in which(seq_along(fl2) >= first & grepl("<StemKey>[0-9]+</StemKey>", fl2))) {
        m <- unique(regmatches(fl2[j], gregexpr("<StemKey>[0-9]+</StemKey>", fl2[j]))[[1L]])
        for (s in m) fl2[j] <- gsub(s, sprintf("<StemKey>%d</StemKey>", 1000L + as.integer(gsub("[^0-9]", "", s))),
                                    fl2[j], fixed = TRUE)
      }
      writeLines(fl2, f40, useBytes = TRUE)
      app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(tree_source = "hpr", wait_ = FALSE); app$wait_for_idle()
      app$upload_file(hs_upload = c(fx_src, f40))
      app$wait_for_js("document.querySelector('#hs_validation') && /2 files/.test(document.querySelector('#hs_validation').innerText)",
                      timeout = 60 * 1000)
      app$wait_for_idle()
      c40$valid <- txt(app, "#hs_validation")
      c40$n_rows <- js(app, "document.querySelectorAll('#hs_species_ui tbody tr').length")
      c40$species <- js(app, paste0("Array.from(document.querySelectorAll('#hs_species_ui select'))",
                                    ".map(function(s){ return s.value; }).join(',')"))
      c40$stems <- js(app, paste0("Array.from(document.querySelectorAll('#hs_species_ui tbody tr'))",
                                  ".map(function(r){ return r.children[1].innerText.trim(); }).join(',')"))
      c40$counts <- txt(app, "#tree_counts")
      app$run_js("window.scrollTo(0, 0);")
      shoot(app, "phase2_01_hpr_two_files_panel")
      run(app)
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
      c40$cmp <- compared_now(app)
      c40$summary <- txt(app, "#summary_cards")
      app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
      c40$trees_hdr <- js(app, "Array.from(document.querySelectorAll('#tbl_trees .rt-th')).map(function(h){ return h.innerText.trim(); }).join('|')")
      c40$page <- paste(txt(app, "#hs_validation"), txt(app, "#summary_cards"), txt(app, "#tbl_trees"))
      c40$nerr <- n_err_notif(app)
      # The tree and log CSVs: a File column with 1 and 2, no file name anywhere.
      dl_csv <- function(id) {
        f <- app$get_download(id)
        list(df = utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
             txt = paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      }
      d_trees <- dl_csv("dl_trees"); d_logs <- dl_csv("dl_logs")
      c40$file_vals <- paste(sort(unique(d_trees$df[["File"]])), collapse = ",")
      c40$file_vals_logs <- paste(sort(unique(d_logs$df[["File"]])), collapse = ",")
      c40$names_in_csv <- grepl("second_upload|hpr_example_synthetic", paste(d_trees$txt, d_logs$txt))
    })
    ok40 <- !nzchar(e40) && grepl("2 files: 26 stems read, 10 used.", c40$valid, fixed = TRUE) &&
      identical(as.integer(c40$n_rows), 3L) && identical(c40$species, "pine,spruce,none") &&
      all(as.integer(strsplit(c40$stems, ",")[[1L]]) %% 2L == 0L) && c40$cmp &&
      grepl("8 spruce · 2 pine stems", c40$counts, fixed = TRUE) &&
      grepl("\\|File\\|", c40$trees_hdr) && !grepl("second_upload|hpr_example_synthetic", c40$page) &&
      identical(c40$nerr, 0L) && identical(c40$file_vals, "1,2") && identical(c40$file_vals_logs, "1,2") &&
      identical(c40$names_in_csv, FALSE)
    rec(browser_checks[36], if (ok40) PASS else FAIL,
        sprintf("summary '%s' | species rows %s: %s (stems %s) | counts '%s' | compared %s | trees columns: %s | file names on the page: %s | File column in the trees / logs CSV: %s / %s, file names in the CSVs: %s | error notifications %s%s",
                substr(gsub("\\s+", " ", or_default(c40$valid, "")), 1, 70), c40$n_rows, c40$species, c40$stems,
                trimws(c40$counts), c40$cmp, substr(or_default(c40$trees_hdr, ""), 1, 90),
                grepl("second_upload|hpr_example_synthetic", or_default(c40$page, "")),
                c40$file_vals, c40$file_vals_logs, c40$names_in_csv, c40$nerr, err_txt(e40)))
    # 41. Stem grades (Phase 6): the invented grades fixture as the .hpr source.
    c41 <- list()
    e41 <- try_drv({
      fx41 <- file.path(root_dir, "inputs", "test_fixtures", "hpr_grades_synthetic.hpr")
      vis <- function(id) isTRUE(js(app, sprintf(
        "(function(){ var e = document.getElementById('%s'); return !!(e && e.offsetParent !== null); })()", id)))
      app$set_inputs(tree_source = "nfi", wait_ = FALSE); app$wait_for_idle()
      app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
      c41$vis_nfi <- vis("grade_use_a") || vis("grade_cut_a") || vis("grade_use_b") || vis("grade_cut_b")
      app$set_inputs(tree_source = "hpr", wait_ = FALSE); app$wait_for_idle()
      c41$vis_hpr <- vis("grade_use_a") && vis("grade_cut_a") && vis("grade_use_b") && vis("grade_cut_b")
      c41$off <- !any(unlist(app$get_values(input = c("grade_use_a", "grade_cut_a", "grade_use_b", "grade_cut_b"))$input))
      c41$labels <- js(app, paste0("['grade_use_a', 'grade_cut_a'].map(function(id){ ",
                                   "return document.getElementById(id).closest('.checkbox, .form-check').innerText.trim(); }).join(' | ')"))
      app$set_inputs(grade_use_a = TRUE, grade_cut_a = TRUE, wait_ = FALSE); app$wait_for_idle()
      app$run_js("document.getElementById('grade_use_a').scrollIntoView({block: 'center'});")
      shoot(app, "phase6_01_grade_options")
      app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
      app$upload_file(hs_upload = fx41)
      app$wait_for_js("document.querySelector('#hs_validation') && /5 stems read/.test(document.querySelector('#hs_validation').innerText)",
                      timeout = 60 * 1000)
      app$wait_for_idle()
      run(app)
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
      c41$cmp <- compared_now(app)
      c41$summary <- txt(app, "#summary_cards")
      c41$changed <- suppressWarnings(as.integer(sub("(?s).*Changed stems\\s*([0-9]+).*", "\\1", c41$summary, perl = TRUE)))
      shoot(app, "phase6_02_grades_comparison")
      c41$nerr <- n_err_notif(app)
      # Copy from A: B's boxes become A's.
      app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
      app$click("scen_copy_b"); app$wait_for_idle()
      app$click("scen_copy_ok"); app$wait_for_idle()
      v <- app$get_values(input = c("grade_use_a", "grade_cut_a", "grade_use_b", "grade_cut_b"))$input
      c41$copied <- isTRUE(v$grade_use_b) && isTRUE(v$grade_cut_b) && isTRUE(v$grade_use_a) && isTRUE(v$grade_cut_a)
    })
    ok41 <- !nzchar(e41) && identical(c41$vis_nfi, FALSE) && isTRUE(c41$vis_hpr) && isTRUE(c41$off) &&
      isTRUE(c41$cmp) && !is.na(c41$changed) && c41$changed >= 1L && identical(c41$nerr, 0L) && isTRUE(c41$copied) &&
      grepl("Use stem grades", c41$labels, fixed = TRUE) && grepl("Cut at grade changes", c41$labels, fixed = TRUE)
    rec(browser_checks[37], if (ok41) PASS else FAIL,
        sprintf("boxes visible with NFI: %s, with .hpr: %s | off by default: %s | labels '%s' | compared %s, changed stems %s | error notifications %s | B's boxes after Copy from A equal A's: %s%s",
                c41$vis_nfi, c41$vis_hpr, c41$off, substr(gsub("\\s+", " ", or_default(c41$labels, "")), 1, 60),
                c41$cmp, c41$changed, c41$nerr, c41$copied, err_txt(e41)))
    # 30. The kept .hpr upload is deleted when the session ends: its path
    # (exported in test mode) exists during the session; after the browser
    # closes the session, the file and its folder are gone.
    c30 <- list()
    e30 <- try_drv({
      c30$path <- app$get_values(export = TRUE)$export$hpr_kept_path
      c30$during <- !is.null(c30$path) && file.exists(c30$path)
      app$run_js("Shiny.shinyapp.$socket.close();")
      for (i in 1:40) { if (!file.exists(or_default(c30$path, ""))) break; Sys.sleep(0.25) }
      c30$after_file <- file.exists(or_default(c30$path, ""))
      c30$after_dir <- dir.exists(dirname(or_default(c30$path, "")))
    })
    ok30 <- !nzchar(e30) && isTRUE(c30$during) && identical(c30$after_file, FALSE) &&
      identical(c30$after_dir, FALSE)
    rec(browser_checks[26], if (ok30) PASS else FAIL,
        sprintf("kept file %s during the session: %s | after the session ended: file %s, folder %s%s",
                basename(or_default(c30$path, "(none)")), c30$during,
                if (isTRUE(c30$after_file)) "still there" else "deleted",
                if (isTRUE(c30$after_dir)) "still there" else "deleted", err_txt(e30)))
    rec(browser_checks[18],
        if (shot_state$attempted > 0L && length(shots) == shot_state$attempted) PASS else FAIL,
        shot_summary(length(shots), shot_state$attempted, shot_state$errors, shot_dir))
    try(app$stop(), silent = TRUE)
    t_el_en <- as.numeric((proc.time() - t_browser)["elapsed"])

    # ==========================================================================
    # Checks 34-39: a second session in Norsk (Phase 1, 2026-10-27)
    # ==========================================================================
    section("34-39. Norsk")
    Sys.unsetenv("APTER_LANG")                       # the app's default language
    if (!exists("i18n_table", mode = "function")) {
      source(file.path(root_dir, "R", "helpers_utils.R")); source(file.path(root_dir, "R", "helpers_i18n.R"))
    }
    tr_tbl <- i18n_table(file.path(root_dir, "inputs", "i18n", "translations.csv"))
    # English texts of the table that must not show in Norsk: those whose
    # Norsk differs, with two or more words or six or more letters, without
    # placeholders (template texts are checked by their fixed parts below).
    en_rows <- tr_tbl[tr_tbl$en != tr_tbl$no & !grepl("%", tr_tbl$en, fixed = TRUE) &
                        (grepl(" ", trimws(tr_tbl$en)) | nchar(tr_tbl$en) >= 6L) &
                        !tr_tbl$en %in% c("Simulator", "Scenario"), , drop = FALSE]
    fixed_parts <- unique(unlist(lapply(tr_tbl$en[tr_tbl$en != tr_tbl$no & grepl(.I18N_PH, tr_tbl$en, perl = TRUE)],
      function(s) { p <- trimws(strsplit(s, .I18N_PH, perl = TRUE)[[1L]]); p[nchar(p) >= 12L & grepl(" [a-z]", p)] })))
    english_left <- function(text) {
      if (is.na(text) || !nzchar(text)) return(character(0))
      esc <- function(x) gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", x, perl = TRUE)
      hit <- vapply(en_rows$en, function(e) grepl(paste0("(^|[^A-Za-z])", esc(e), "($|[^A-Za-z])"), text, perl = TRUE), logical(1))
      c(unique(en_rows$en[hit]), fixed_parts[vapply(fixed_parts, function(p) grepl(p, text, fixed = TRUE), logical(1))])
    }
    shorten_txt <- function(x) if (!length(x)) "none" else
      paste(c(utils::head(x, 5L), if (length(x) > 5L) sprintf("... (%d)", length(x))), collapse = " | ")
    vis_text <- function(app, sel = "body") js(app, sprintf(
      "(function(){ var e = document.querySelector('%s'); return e ? e.innerText : ''; })()", sel))
    lang_dir <- shot_dir
    lshot <- function(app, name) shoot(app, paste0("no_", name), dir = lang_dir, norsk = TRUE)
    t_no <- proc.time()
    appn <- audit_app_driver(app_dir, name = "audit17_no", load_timeout = 60 * 1000,
                             timeout = 120 * 1000, width = 1400, height = 1000)
    if (!inherits(appn, "AppDriver")) {
      for (ck in browser_checks[30:35]) rec(ck, FAIL, paste("Norsk session did not start:", appn))
    } else {
      app <- appn
      app$wait_for_js("document.fonts.status === 'loaded' && !!document.querySelector('#tbl_asm_a .rt-th')",
                      timeout = 30 * 1000)
      nav_txt <- function(app) js(app, "Array.from(document.querySelectorAll('#main_nav .nav-link')).map(function(a){ return a.textContent.trim(); }).join('|')")
      lang_state <- function(app) js(app, "document.documentElement.lang + '|' + (document.querySelector('.lang-btn.is-active') || {dataset:{}}).dataset.lang")
      switch_to <- function(app, l) {
        app$run_js(sprintf("document.querySelector('.lang-btn[data-lang=\"%s\"]').click();", l))
        app$wait_for_idle()
      }
      hdr_a <- function(app) headers_of(app, "tbl_asm_a")

      # 34. Norsk by default; switch to English and back without a reload.
      c34 <- list()
      e34 <- try_drv({
        c34$state <- lang_state(app); c34$nav <- nav_txt(app)
        c34$run <- trimws(txt(app, "#run_btn")); c34$title <- js(app, "document.title")
        c34$hdr <- hdr_a(app); c34$row1 <- rows_txt(app, "tbl_asm_a")
        app$run_js("window.__apter17 = 1;")
        lshot(app, "01_prices_no")
        switch_to(app, "en")
        c34$state_en <- lang_state(app); c34$nav_en <- nav_txt(app); c34$run_en <- trimws(txt(app, "#run_btn"))
        c34$hdr_en <- hdr_a(app); c34$title_en <- js(app, "document.title")
        lshot(app, "02_prices_en")
        switch_to(app, "no")
        c34$state_back <- lang_state(app); c34$hdr_back <- hdr_a(app)
        c34$marker <- js(app, "window.__apter17 === 1")
      })
      ok34 <- !nzchar(e34) && identical(c34$state, "nb|no") && identical(c34$nav, "Sortiment og priser|Simulator") &&
        identical(c34$run, "Kjør optimering") && grepl("apteringssimulator", or_default(c34$title, "")) &&
        grepl("^Sortiment\\|Treslag\\|Topp-Ø \\(cm\\)", or_default(c34$hdr, "")) &&
        grepl("Sagtømmer;Gran", or_default(c34$row1, ""), fixed = TRUE) &&
        identical(c34$state_en, "en|en") && identical(c34$nav_en, "Assortments and prices|Simulator") &&
        identical(c34$run_en, "Run optimizer") && grepl("^Assortment\\|Species\\|Top Ø \\(cm\\)", or_default(c34$hdr_en, "")) &&
        grepl("Bucking simulator", or_default(c34$title_en, "")) &&
        identical(c34$state_back, "nb|no") && identical(c34$hdr_back, c34$hdr) && isTRUE(c34$marker)
      rec(browser_checks[30], if (ok34) PASS else FAIL,
          sprintf("start: %s, tabs '%s', Run '%s', title '%s', table '%s', first row '%s' | English: %s, tabs '%s', Run '%s', table '%s' | back: %s, same table %s, no reload %s%s",
                  c34$state, c34$nav, c34$run, c34$title, substr(or_default(c34$hdr, ""), 1, 60),
                  substr(or_default(c34$row1, ""), 1, 40), c34$state_en, c34$nav_en, c34$run_en,
                  substr(or_default(c34$hdr_en, ""), 1, 60), c34$state_back,
                  identical(c34$hdr_back, c34$hdr), c34$marker, err_txt(e34)))

      # 35. Both tabs and the sidebar in Norsk; no English on the page.
      c35 <- list()
      e35 <- try_drv({
        c35$left_prices <- english_left(vis_text(app))
        # The price grid with a decimal comma; a price typed as "430,5" is stored as 430.5.
        c35$cell <- js(app, "document.querySelector('#pm_a .ht_master tbody td').textContent")
        app$run_js(paste0("var h = HTMLWidgets.find('#pm_a').hot; h.selectCell(0,0); var ed = h.getActiveEditor();",
                          " ed.beginEditing(); ed.TEXTAREA.value = '430,5'; ed.finishEditing();"))
        app$wait_for_idle()
        pa <- dl_pm(app, "dl_a_long")
        c35$typed <- pa$PriceNOKperM3[pa$ProductKey == "spruce_sawlog" & pa$PriceDiameterClass_cm %in% 12 &
                                       pa$LengthClass_dm %in% 31]
        app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
        app$wait_for_js("!!document.querySelector('#tree_counts .tree-counts-n')", timeout = 60 * 1000)
        c35$sidebar <- vis_text(app, ".bslib-sidebar-layout > .sidebar")
        c35$counts <- trimws(txt(app, "#tree_counts"))
        c35$species <- js(app, "$('#species_group')[0].selectize.getItem($('#species_group')[0].selectize.getValue()).text()")
        c35$county <- js(app, "$('#fylnr_sel')[0].selectize.getItem('__all__').text()")
        c35$left_sim <- english_left(vis_text(app))
        c35$slider <- js(app, "document.querySelector('.irs-to').textContent")
        lshot(app, "03_simulator_no")
      })
      ok35 <- !nzchar(e35) && !length(c35$left_prices) && !length(c35$left_sim) &&
        identical(c35$cell, "422,00") && isTRUE(all.equal(c35$typed, 430.5)) &&
        identical(c35$slider, "1 100 m") &&
        identical(c35$species, "Gran") && identical(c35$county, "Alle fylker") &&
        all(vapply(c("Datakilde", "Landsskogtakseringen", "Hogstklasse", "HKL 4 – eldre produksjonsskog",
                     "Bonitet (H40)", "Fylke", "Høyde over havet (moh.)"),
                   function(w) grepl(w, or_default(c35$sidebar, ""), fixed = TRUE), logical(1))) &&
        grepl("prøveflater .* graner", or_default(c35$counts, "")) && grepl("Beregnet kjøretid", or_default(c35$counts, ""))
      rec(browser_checks[31], if (ok35) PASS else FAIL,
          sprintf("grid cell '%s', '430,5' typed -> %s, slider '%s' | English left: prices tab %s, simulator %s | species '%s', county '%s' | counts '%s'%s",
                  c35$cell, paste(c35$typed, collapse = ","), c35$slider,
                  shorten_txt(c35$left_prices), shorten_txt(c35$left_sim), c35$species, c35$county,
                  substr(gsub("\\s+", " ", or_default(c35$counts, "")), 1, 90), err_txt(e35)))

      # 36. Dialogs and messages in Norsk.
      c36 <- list()
      modal_txt <- function(app) vis_text(app, ".modal-content")
      close_modal <- function(app) {
        app$run_js("var b = document.querySelector('.modal-footer button[data-dismiss], .modal-footer button[data-bs-dismiss]'); if (b) b.click();")
        app$wait_for_js("!document.querySelector('.modal.show')", timeout = 10 * 1000)
      }
      e36 <- try_drv({
        app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
        app$click("asm_add_a"); app$wait_for_js("!!document.querySelector('.modal.show')", timeout = 10 * 1000)
        c36$add <- modal_txt(app); lshot(app, "04_add_no"); close_modal(app)
        app$click("scen_copy_b"); app$wait_for_js("!!document.querySelector('.modal.show')", timeout = 10 * 1000)
        c36$copy <- modal_txt(app); close_modal(app)
        pick(app, "b", "spruce_pulpwood"); app$wait_for_idle()
        app$click("asm_remove_b"); app$wait_for_js("!!document.querySelector('.modal.show')", timeout = 10 * 1000)
        c36$remove <- modal_txt(app); close_modal(app)
        app$click("hpr_import_a"); app$wait_for_js("!!document.querySelector('.modal.show')", timeout = 10 * 1000)
        app$upload_file(hpr_file = file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr"))
        app$wait_for_js("!!document.querySelector('.hpr-summary')", timeout = 60 * 1000)
        c36$hpr <- modal_txt(app); lshot(app, "05_hpr_import_no"); close_modal(app)
        app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
        app$set_inputs(tree_source = "list", wait_ = FALSE); app$wait_for_idle()
        bad_tl <- tempfile(fileext = ".csv")
        utils::write.csv(data.frame(tree = 1:3, art = "spruce"), bad_tl, row.names = FALSE)
        app$upload_file(tl_upload = bad_tl); app$wait_for_idle()
        c36$tl <- vis_text(app, "#tl_validation"); lshot(app, "06_tree_list_errors_no")
        app$set_inputs(tree_source = "nfi", wait_ = FALSE); app$wait_for_idle()
      })
      c36$left <- unique(unlist(lapply(c36[c("add", "copy", "remove", "hpr", "tl")], english_left)))
      ok36 <- !nzchar(e36) && !length(c36$left) &&
        grepl("Legg til sortiment i scenario A", or_default(c36$add, ""), fixed = TRUE) &&
        grepl("Minste toppdiameter (cm)", or_default(c36$add, ""), fixed = TRUE) &&
        grepl("Kopier scenario A til scenario B", or_default(c36$copy, ""), fixed = TRUE) &&
        grepl("Fjern Gran massevirke", or_default(c36$remove, ""), fixed = TRUE) &&
        grepl("treslagsgrupper ·", or_default(c36$hpr, ""), fixed = TRUE) &&
        grepl("Forbudte celler", or_default(c36$hpr, ""), fixed = TRUE) &&
        grepl("kan ikke brukes:", or_default(c36$tl, ""), fixed = TRUE) &&
        grepl("Mangler påkrevde kolonner", or_default(c36$tl, ""), fixed = TRUE)
      rec(browser_checks[32], if (ok36) PASS else FAIL,
          sprintf("add '%s' | copy '%s' | remove '%s' | .hpr '%s' | tree list '%s' | English left: %s%s",
                  substr(gsub("\\s+", " ", or_default(c36$add, "")), 1, 60),
                  substr(gsub("\\s+", " ", or_default(c36$copy, "")), 1, 40),
                  substr(gsub("\\s+", " ", or_default(c36$remove, "")), 1, 40),
                  substr(gsub("\\s+", " ", or_default(c36$hpr, "")), 1, 80),
                  substr(gsub("\\s+", " ", or_default(c36$tl, "")), 1, 100), shorten_txt(c36$left), err_txt(e36)))

      # 37. A comparison in Norsk (B: spruce pulpwood at 420 NOK/m3).
      c37 <- list()
      e37 <- try_drv({
        app$run_js("Shiny.setInputValue('flat_edit', {side: 'B', key: 'spruce_pulpwood', value: '420'}, {priority: 'event'});")
        app$wait_for_idle()
        app$set_inputs(main_nav = "simulator", wait_ = FALSE); app$wait_for_idle()
        run(app)
        c37$tabs <- result_tabs_txt(app)
        c37$summary <- vis_text(app, "#summary_cards"); lshot(app, "07_summary_no")
        c37$left <- english_left(vis_text(app))
        app$set_inputs(result_tabs = "products", wait_ = FALSE); app$wait_for_idle()
        c37$products <- headers_of(app, "tbl_products"); c37$type_rows <- rows_txt(app, "tbl_per_m3_type")
        c37$left <- c(c37$left, english_left(vis_text(app))); lshot(app, "08_products_no")
        app$set_inputs(result_tabs = "trees", wait_ = FALSE); app$wait_for_idle()
        c37$trees <- headers_of(app, "tbl_trees"); c37$pager <- trimws(txt(app, "#tbl_trees_info"))
        c37$left <- c(c37$left, english_left(vis_text(app))); lshot(app, "09_trees_no")
        app$set_inputs(result_tabs = "logs", wait_ = FALSE); app$wait_for_idle()
        c37$tally <- trimws(txt(app, "#tally_note")); c37$logs <- headers_of(app, "tbl_selected_logs")
        c37$left <- c(c37$left, english_left(vis_text(app))); lshot(app, "10_logs_no")
        c37$imgs <- js(app, paste0("['bar_nok_contribution','fig_distributions','fig_dbh_class','stem_compare']",
                                   ".filter(function(i){ var e = document.querySelector('#' + i + ' img'); return e && e.naturalWidth > 0; }).length"))
        c37$nerr <- n_err_notif(app)
      })
      nok_val <- regmatches(or_default(c37$summary, ""), regexpr("Kr per m³\\s*\\n?\\s*[0-9  ]+,[0-9]", or_default(c37$summary, "")))
      ok37 <- !nzchar(e37) && identical(c37$tabs, "Sammendrag|Sortimenter|Trær|Stokker") &&
        all(vapply(c("Endrede apteringsmønstre", "Totalverdi (kr)", "Salgbart volum (m³fub)", "Trær med stokker (A / B)"),
                   function(w) grepl(w, or_default(c37$summary, ""), fixed = TRUE), logical(1))) &&
        length(nok_val) == 1L &&
        grepl("^Sortiment\\|Verdi A \\(kr\\)\\|Verdi B \\(kr\\)\\|Verdiendring \\(kr\\)", or_default(c37$products, "")) &&
        grepl("Totalt;", or_default(c37$type_rows, ""), fixed = TRUE) &&
        grepl("Prøveflate|Tre|Treslag|BHD (cm)|Høyde (m)|Høydekilde|Stokker A", or_default(c37$trees, ""), fixed = TRUE) &&
        grepl(" av ", or_default(c37$pager, ""), fixed = TRUE) &&
        identical(c37$tally, "Prisdiameterklasse (cm) mot lengdeklasse (dm)") &&
        grepl("Sortiment", or_default(c37$logs, ""), fixed = TRUE) &&
        identical(as.integer(c37$imgs), 4L) && identical(as.integer(c37$nerr), 0L) && !length(unique(c37$left))
      rec(browser_checks[33], if (ok37) PASS else FAIL,
          sprintf("tabs '%s' | NOK/m³ '%s' | products '%s' | trees '%s' | pager '%s' | tally '%s' | figures %s of 4 | errors %s | English left: %s%s",
                  c37$tabs, gsub("\\s+", " ", paste(nok_val, collapse = "")), substr(or_default(c37$products, ""), 1, 70),
                  substr(or_default(c37$trees, ""), 1, 70), c37$pager, c37$tally, c37$imgs, c37$nerr,
                  shorten_txt(unique(c37$left)), err_txt(e37)))

      # 38. Downloads in Norsk.
      c38 <- list()
      e38 <- try_drv({
        f_p <- app$get_download("dl_products"); c38$f_p <- basename(f_p)
        raw_p <- readBin(f_p, "raw", file.size(f_p))
        c38$bom <- identical(raw_p[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
        lines_p <- strsplit(rawToChar(raw_p[-(1:3)]), "\r?\n")[[1L]]; Encoding(lines_p) <- "UTF-8"
        c38$head_p <- lines_p[1L]; c38$dec <- any(grepl(";-?[0-9]+,[0-9]", lines_p[-1L]))
        f_t <- app$get_download("dl_trees"); c38$f_t <- basename(f_t)
        c38$head_t <- names(utils::read.csv2(f_t, fileEncoding = "UTF-8-BOM", check.names = FALSE, nrows = 1))
        f_x <- app$get_download("dl_tally_xlsx"); c38$f_x <- basename(f_x); c38$sheets <- readxl::excel_sheets(f_x)
        f_s <- app$get_download("dl_scen_a"); c38$f_s <- basename(f_s); c38$scen_sheets <- readxl::excel_sheets(f_s)
        c38$scen_ok <- isTRUE(scenario_import(f_s)$ok)
        c38$f_scen <- f_s
      })
      ok38 <- !nzchar(e38) && identical(c38$f_p, "optBuck_sortimenter.csv") && isTRUE(c38$bom) &&
        grepl('^"Sortiment";"Stokker A";', or_default(c38$head_p, "")) && isTRUE(c38$dec) &&
        identical(c38$f_t, "optBuck_tredata.csv") && identical(c38$head_t[1:3], c("Scenario", "Prøveflate", "Tre")) &&
        identical(c38$f_x, "optBuck_stokkfordeling.xlsx") && "liste" %in% c38$sheets &&
        identical(c38$f_s, "optBuck_scenario_A.xlsx") && "assortments" %in% c38$scen_sheets && isTRUE(c38$scen_ok)
      rec(browser_checks[34], if (ok38) PASS else FAIL,
          sprintf("products %s (BOM %s, header '%s', decimal comma %s) | trees %s '%s' | tally %s sheets '%s' | scenario %s sheets '%s', imports %s%s",
                  c38$f_p, c38$bom, substr(or_default(c38$head_p, ""), 1, 60), c38$dec, c38$f_t,
                  paste(utils::head(c38$head_t, 4), collapse = "|"), c38$f_x, paste(c38$sheets, collapse = "|"),
                  c38$f_s, paste(c38$scen_sheets, collapse = "|"), c38$scen_ok, err_txt(e38)))

      # 39. A switch keeps everything; a Norsk scenario file loads in English;
      # the choice survives a reload.
      c39 <- list()
      digits <- function(x) gsub("[^0-9]", "", x)
      e39 <- try_drv({
        app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
        c39$no <- vis_text(app, "#summary_cards")
        switch_to(app, "en")
        c39$en <- vis_text(app, "#summary_cards"); lshot(app, "11_summary_en_after_switch")
        c39$tabs_en <- result_tabs_txt(app)
        app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
        pick(app, "b", "spruce_pulpwood"); app$wait_for_idle()
        c39$flat_b <- js(app, "document.querySelector('#flat_price_b') ? document.querySelector('#flat_price_b').value : ''")
        app$upload_file(scen_b_upload = c38$f_scen); app$wait_for_idle()
        c39$msg <- trimws(txt(app, "#msg_b"))
        c39$stored <- js(app, "window.localStorage.getItem('apter.lang')")
        app$run_js("location.reload();")
        Sys.sleep(2)
        app$wait_for_js("!!document.querySelector('.lang-btn.is-active') && !!document.querySelector('#tbl_asm_a .rt-th')",
                        timeout = 60 * 1000)
        c39$after_reload <- lang_state(app); c39$nav_reload <- nav_txt(app)
      })
      ok39 <- !nzchar(e39) && grepl("Endrede apteringsmønstre", or_default(c39$no, ""), fixed = TRUE) &&
        grepl("Changed cutting patterns", or_default(c39$en, ""), fixed = TRUE) &&
        identical(digits(c39$no), digits(c39$en)) && identical(c39$tabs_en, "Summary|Products|Trees|Logs") &&
        identical(c39$flat_b, "420") &&
        grepl("^Loaded optBuck_scenario_A\\.xlsx: 6 assortment\\(s\\) with prices\\.", or_default(c39$msg, "")) &&
        identical(c39$stored, "en") && identical(c39$after_reload, "en|en") &&
        identical(c39$nav_reload, "Assortments and prices|Simulator")
      t_el_no <- as.numeric((proc.time() - t_no)["elapsed"])
      rec(browser_checks[35], if (!ok39) FAIL else if (t_el_no < 240) PASS else REVIEW,
          sprintf("summary numbers same after the switch %s ('%s') | tabs '%s' | B's flat price '%s' | Norsk file in English: '%s' | stored '%s', after reload %s, tabs '%s' | Norsk session %.1f s (threshold 240 s)%s",
                  identical(digits(c39$no), digits(c39$en)), substr(gsub("\\s+", " ", or_default(c39$en, "")), 1, 80),
                  c39$tabs_en, c39$flat_b, c39$msg, c39$stored, c39$after_reload, c39$nav_reload, t_el_no, err_txt(e39)))
      try(app$stop(), silent = TRUE)
      n_shots_no <- shot_state$attempted_no - length(shot_state$errors_no)
      rec("Check 22b: Screenshots saved for review (Norsk session)",
          if (shot_state$attempted_no > 0L && length(shot_state$errors_no) == 0L) PASS else FAIL,
          shot_summary(n_shots_no, shot_state$attempted_no, shot_state$errors_no, lang_dir))
    }
  } else {
    # every browser check except 05 (recorded above) and the runtime (recorded below)
    for (ck in browser_checks[setdiff(seq_along(browser_checks), c(1L, 19L))])
      rec(ck, FAIL, "skipped: app did not start")
  }
  # 23. runtime (the English session; the Norsk session has its own, check 39)
  t_el <- if (exists("t_el_en")) t_el_en else as.numeric((proc.time() - t_browser)["elapsed"])
  rec(browser_checks[19], if (t_el < 300) PASS else REVIEW,
      sprintf("browser checks took %.1f s (threshold: 300 s)", t_el))
}, error = function(e) {
  # An uncaught driver error (Chrome's connection dropped, a timeout outside
  # try_drv()): every check not recorded yet fails with the error text, and the
  # audit still writes its outputs below.
  msg <- gsub("\\s+", " ", conditionMessage(e))
  cat("\nBrowser section aborted:", msg, "\n", file = audit_txt, append = TRUE)
  todo <- setdiff(c(browser_checks, "Check 22b: Screenshots saved for review (Norsk session)"), E$results$check)
  for (ck in todo)
    rec(ck, FAIL, paste("not run: the browser section aborted:", substr(msg, 1L, 300L)))
}, finally = {
  audit_browser_stop(if (exists("app", inherits = FALSE) && inherits(app, "AppDriver")) app else list())
})
# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 17 complete.\n")
cat("Results written to:", out_dir, "\n")
