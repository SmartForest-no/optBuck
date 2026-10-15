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
#        d. the default assortment table gives the base ProductData; matrix
#           choices are spruce sawlog, pine sawlog and the flat table
#  5.  AppDriver$new("app") starts without error (load timeout 60 s)
#  6.  Layout: two tabs, "Assortments and prices" first and open, then
#      "Simulator"; no Home, tree-source tiles, AVAILABLE/PLANNED labels or
#      release footer; the stripe is shown; the window title and the
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
#      one copy button per section ("Make assortment equal to B/A",
#      "Make matrix equal to B/A"); no whole-scenario buttons
#  9.  Independent scenarios (Phase 4d): both grids editable (no read-only
#      cells), nothing greyed, no highlighted differences at the start, no
#      "Same as A" dropdown and no "B equals/differs" notes
# 10.  Matrix selectors are independent (choosing in B does not change A);
#      spruce pulpwood edited to a price matrix in B only: B lists its
#      matrix, A does not, and the Pricing cell is highlighted on both
#      sides; "Make assortment equal to A" in B restores it (no highlights)
# 11.  Rows and columns of A (spruce sawlog, from the downloaded A): a
#      larger and a smaller class added and removed at the ends, a length
#      (58 dm) added and removed; the download matches each step
# 12.  Run with B = A: one run, no comparison tab, no error notification;
#      Run is disabled with progress while the task runs (REVIEW if the run
#      finished before the driver looked)
# 13.  Summary shows the nine fields (run time, plots, trees, trees with
#      logs, logs, commercial volume, total value, NOK per m3, price
#      volume); no Diagnostics tab
# 14.  Selected logs: server-side pages ("1–25 of N", N = logs); readable
#      headers (Plot, StartPos, StopPos; no ProductType, plot_id_github or
#      underscores)
# 15.  Per m3 tab: tables render, the collapsed Trees card expands to tree
#      rows, selecting a tree shows its logs below; no "per ha" /
#      "ha-expanded" text
# 16.  Automatic comparison: B's price matrix uploaded (spruce sawlog x1.2,
#      wide CSV) -> differing cells highlighted in both grids, 351 cells
#      differ in B's download; Run -> comparison tab shown, difference
#      table, pattern line and stem diagram render, no error notification;
#      the Summary shows the run once and Scenario A and B side by side
# 17.  Spruce pulpwood removed from B only: the comparison run's B has no
#      spruce pulpwood; "Make assortment equal to A" brings it back
# 18.  Stale notice: absent right after a run; after a price edit,
#      "Assortments or prices changed since this run" shows
# 19.  No "default" or "price set" (as words) anywhere in the page text
#      (Phase 4d: "Scenario A/B" is the user-facing term)
# 20.  Custom assortment through the form in A (spruce poles, price matrix,
#      12-30 cm, 600-1000 cm, 2000 NOK/m3): the form has no Type, Length step
#      or Diameter rounding and no blurred backdrop; listed, in the matrix
#      choices, edit keeps its prices; a run selects it; Reset to defaults
#      keeps it and restores the base rows; Remove (from both) drops it
# 21.  Tree list (Phase 5): upload the template -> validation panel lists 12
#      trees and the height methods; Run -> the FinnHoyde height banner shows
#      and the Trees table has a "Height source" column; no error notifications
# 22.  Screenshots of both tabs, the results and the comparison saved to
#      outputs/app_smoke/screenshots/ (for review)
# 23.  Total browser runtime under 300 s (REVIEW if above)
# 24.  Scenario B's assortments (Phase 4d): a B rule change (spruce sawlog
#      min top 14 cm) is highlighted on both sides; an assortment added to B
#      only is highlighted in B and listed as "Not in A" in A; one removed
#      from B only is listed as "Not in B"; Run -> comparison with B's
#      products (poles); both tables list the same assortments in the same
#      order (species, sawlog, pulpwood, waste, custom), "Not in" lines
#      included; "Make matrix equal to B" in A copies prices; Scenario A
#      downloaded and uploaded to B -> no highlighted differences left
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
  keys <- assortment_table_keys(defs0)
  ids  <- c(keys[defs0$PriceType == "matrix"], PM_FLAT_ID)
  ok_d <- identical(p_d$pd, H$APP_PRODUCTS) &&
    identical(ids, c("spruce_sawlog", "pine_sawlog", "flat_prices"))
  rec("Check 04d: Default assortment table gives the base products; matrix choices",
      if (ok_d) PASS else FAIL,
      sprintf("ProductData identical: %s | matrices: %s",
              identical(p_d$pd, H$APP_PRODUCTS), paste(ids, collapse = ", ")))
} else {
  rec("Check 04: Pure app helpers", FAIL,
      paste("sourcing backend/app files failed:", helpers_err))
}


# ==============================================================================
# Checks 05-23: drive the app with shinytest2
# ==============================================================================
browser_checks <- c(
  "Check 05: AppDriver starts the app",
  "Check 06: Two tabs (Assortments and prices first), no Home or footer, stripe, optBuck, texts",
  "Check 07: Bootstrap 5 and self-hosted Inter font loaded",
  "Check 08: Compact assortment table: base rows, columns, '–' for no limit",
  "Check 09: Both scenarios editable, nothing greyed, no differences at the start",
  "Check 10: Independent matrix selectors; price type per scenario; Make assortment equal to A",
  "Check 11: Diameter classes and lengths added and removed",
  "Check 12: Run with B = A: single run, no comparison tab; Run disabled while running",
  "Check 13: Summary shows the nine fields; no Diagnostics tab",
  "Check 14: Selected logs paged on the server, readable headers",
  "Check 15: Per m3 tab renders; Trees card expands, a selected tree shows its logs; no per-ha text",
  "Check 16: Comparison runs automatically when B differs",
  "Check 17: Assortment removed from B only drops it from B's run",
  "Check 18: Stale notice after a price edit",
  "Check 19: No 'default'/'price set' in the page text",
  "Check 20: Custom assortment (simplified form) added, edited, run, kept by Reset to defaults, removed",
  "Check 21: Uploaded tree list validates, runs and reports height sources",
  "Check 22: Screenshots saved for review",
  "Check 23: Browser runtime under 300 s",
  "Check 24: Scenario B assortments: own rule, B-only, removed from B only, row order, comparison, A downloaded and uploaded to B")

if (!browser_ok) {
  for (ck in browser_checks)
    rec(ck, FAIL, "skipped: browser dependencies missing (see Check 01)")
} else {
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
  shot_dir <- file.path(out_dir, "screenshots")
  ensure_dir(shot_dir)
  unlink(list.files(shot_dir, pattern = "\\.png$", full.names = TRUE))
  shots <- character(0)
  shoot <- function(app, name) {
    f <- file.path(shot_dir, paste0(name, ".png"))
    ok <- tryCatch({ app$get_screenshot(f); file.exists(f) }, error = function(e) FALSE)
    if (ok) shots <<- c(shots, f)
  }
  # A's or B's prices as the app would download them (long CSV).
  dl_pm <- function(app, id) {
    f <- tryCatch(app$get_download(id), error = function(e) NULL)
    if (is.null(f)) return(NULL)
    pm_minimal(as.data.frame(readr::read_csv(f, show_col_types = FALSE)))
  }
  axes_of <- function(pm, id) {
    s <- pm[pm$ProductKey == id & !is.na(pm$LengthClass_dm), , drop = FALSE]
    list(cls = sort(unique(s$PriceDiameterClass_cm)), len = sort(unique(s$LengthClass_dm)))
  }

  # AppDriver$new() skips (as "On CRAN") outside interactive sessions unless set.
  Sys.setenv(NOT_CRAN = "true")
  # NFI runs sample 5 plots instead of all (setup.R), to keep the audit short.
  Sys.setenv(APTER_N_PLOTS = "5")
  # chromote waits 10 s for Chrome by default; cold starts on the remote
  # desktop can exceed that.
  options(chromote.timeout = 60)
  t_browser <- proc.time()
  app <- tryCatch(
    shinytest2::AppDriver$new(app_dir, name = "audit17",
                              load_timeout = 60 * 1000,
                              timeout = 120 * 1000,
                              width = 1400, height = 1000),
    error = function(e) conditionMessage(e))
  app_ok <- inherits(app, "AppDriver")
  rec(browser_checks[1], if (app_ok) PASS else FAIL,
      if (app_ok) "app started" else paste("start error:", app))

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

    # 06-07. layout and theme
    app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30 * 1000)
    tabs <- js(app, paste0("Array.from(document.querySelectorAll('#main_nav .nav-link'))",
                           ".map(function(a){ return a.textContent.trim(); }).join('|')"))
    active <- js(app, "document.querySelector('#main_nav .nav-link.active').textContent.trim()")
    body0 <- js(app, "document.body.textContent")
    n_old <- js(app, "document.querySelectorAll('.status-tile, .apter-footer, .apter-hero').length")
    stripe <- js(app, "document.querySelectorAll('.banner-strip').length")
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
    shot_nav <- function(app, name) {
      f <- file.path(shot_dir, paste0(name, ".png"))
      ok <- tryCatch({ app$get_screenshot(f, selector = ".navbar"); file.exists(f) }, error = function(e) FALSE)
      if (ok) shots <<- c(shots, f)
    }
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
      isTRUE(stripe >= 1L) && grepl("optBuck", title, fixed = TRUE) && identical(brand, "optBuck") &&
      !grepl("Apter", paste(title, brand), fixed = TRUE) &&
      grepl("NFI data", texts6, fixed = TRUE) && !grepl("NFI sample", texts6, fixed = TRUE) &&
      !grepl("(^|[^0-9.])2021([^0-9]|$)", texts6) && !grepl("wide or long", texts6, fixed = TRUE) &&
      !grepl("runs use Scenario A only|runs compare both", texts6) &&
      intro6 && logo_ok && narrow_ok && !nzchar(paste0(e6, e6b, e6c)) &&
      grepl("Data source", texts6, fixed = TRUE) && !grepl("Tree source", texts6, fixed = TRUE)
    rec(browser_checks[2], if (ok6) PASS else FAIL,
        sprintf("tabs: '%s' | open: '%s' | Home/tiles/footer elements: %s | stripe: %s | title: '%s' | brand: '%s' | intro line: %s | logo height: %s px | 'NFI data': %s | '2021': %s | 'wide or long': %s",
                tabs, active, n_old, stripe, title, brand, intro6,
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
    copy8 <- js(app, "['pm_a_copy_all','pm_b_copy_all','asm_copy_all_a','asm_copy_all_b'].filter(function(i){ return !!document.getElementById(i); }).length")
    copy1 <- js(app, paste0("['pm_a_copy_one','pm_b_copy_one','asm_copy_one_a','asm_copy_one_b'].map(function(i){",
                            " var b = document.getElementById(i); return b ? b.textContent.trim() : ''; }).join('|')"))
    n_eq8 <- js(app, "Array.from(document.querySelectorAll('button')).filter(function(b){ return /^Make [AB] equal to [AB]$/.test(b.textContent.trim()); }).length")
    ok8 <- hdr_ok && identical(n_asm0, 6L) && grepl("–", asm_txt, fixed = TRUE) &&
      !grepl("Inf", asm_txt, fixed = TRUE) && identical(sortable8, 0L) && identical(copy8, 0L) &&
      identical(copy1, "Make matrix equal to B|Make matrix equal to A|Make assortment equal to B|Make assortment equal to A") &&
      identical(n_eq8, 0L)
    rec(browser_checks[4], if (ok8) PASS else FAIL,
        sprintf("rows %s | all visible columns: %s | '–' shown: %s | 'Inf' shown: %s | sortable headers: %s | copy buttons: '%s' (whole-scenario buttons: %s, 'Make A/B equal to' buttons: %s)",
                n_asm0, hdr_ok, grepl("–", asm_txt, fixed = TRUE),
                grepl("Inf", asm_txt, fixed = TRUE), sortable8, copy1, copy8, n_eq8))
    shoot(app, "01_assortments_and_prices")

    # 09. Both scenarios editable (Handsontable marks read-only cells
    # htDimmed), nothing greyed or highlighted at the start.
    marks <- function(app) js(app, paste0("document.querySelectorAll('.pm-diff, .asm-diff, .asm-only, ",
                                          ".asm-none, .pm-same').length"))
    a_cells <- n_cells(app, "pm_a", ""); a_dim <- n_cells(app, "pm_a", ".htDimmed")
    b_cells <- n_cells(app, "pm_b", ""); b_dim <- n_cells(app, "pm_b", ".htDimmed")
    marks9 <- marks(app)
    dropdown9 <- js(app, "document.querySelectorAll('#pm_b_state, #pm_status').length")
    ok9 <- isTRUE(a_cells > 0L) && identical(a_dim, 0L) && isTRUE(b_cells > 0L) &&
      identical(b_dim, 0L) && identical(marks9, 0L) && identical(dropdown9, 0L)
    rec(browser_checks[5], if (ok9) PASS else FAIL,
        sprintf("A cells %s (read-only %s) | B cells %s (read-only %s) | highlighted or greyed: %s | B dropdown / status note: %s",
                a_cells, a_dim, b_cells, b_dim, marks9, dropdown9))

    # 10. Independent selectors; price type per scenario
    opts <- function(app, id) js(app, sprintf(paste0(
      "(function(){ var s = document.getElementById('%s');",
      " return s && s.selectize ? Object.keys(s.selectize.options).join(',')",
      " : Array.from(s.options).map(function(o){ return o.value; }).join(','); })()"), id))
    e10 <- try_drv({
      app$set_inputs(pm_id_b = "flat_prices", wait_ = FALSE); app$wait_for_idle()
    })
    sel_a10 <- app$get_value(input = "pm_id_a")
    e10b <- try_drv({
      app$set_inputs(asm_pick_b = "spruce_pulpwood", wait_ = FALSE); app$click("asm_edit_b"); app$wait_for_idle()
      app$set_inputs(asm_price_type = "matrix", asm_price = 300, asm_max_class = 30, wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
    })
    ids_a10 <- opts(app, "pm_id_a"); ids_b10 <- opts(app, "pm_id_b")
    diff_a10 <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-diff').length")
    diff_b10 <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-diff').length")
    shoot(app, "01b_price_type_in_B")
    e10c <- try_drv({
      app$set_inputs(asm_pick_b = "spruce_pulpwood", wait_ = FALSE); app$click("asm_copy_one_b"); app$wait_for_idle()
    })
    marks10 <- marks(app); msg10 <- txt(app, "#msg_b")
    ok10 <- !nzchar(paste0(e10, e10b, e10c)) && identical(sel_a10, "spruce_sawlog") &&
      grepl("spruce_pulpwood", ids_b10, fixed = TRUE) && !grepl("spruce_pulpwood", ids_a10, fixed = TRUE) &&
      isTRUE(diff_a10 >= 1L) && isTRUE(diff_b10 >= 1L) && identical(marks10, 0L) &&
      grepl("equal to Scenario A", msg10, fixed = TRUE)
    rec(browser_checks[6], if (ok10) PASS else FAIL,
        sprintf("A's selector after choosing flat prices in B: '%s' | matrices A: %s | B: %s | highlighted cells A %s, B %s | after Make assortment equal to A: %s marks, '%s'%s",
                sel_a10, ids_a10, ids_b10, diff_a10, diff_b10, marks10, trimws(msg10),
                err_txt(e10, e10b, e10c)))

    # 11. rows and columns of A (spruce sawlog)
    ax0 <- axes_of(dl_pm(app, "dl_a_long"), "spruce_sawlog")
    step <- function(btn, set = NULL) {
      e <- try_drv({
        if (!is.null(set)) do.call(app$set_inputs, c(set, list(wait_ = FALSE)))
        app$click(btn); app$wait_for_idle()
      })
      list(e = e, ax = axes_of(dl_pm(app, "dl_a_long"), "spruce_sawlog"))
    }
    s1 <- step("pm_a_cls_add_hi"); s2 <- step("pm_a_cls_add_lo")
    s3 <- step("pm_a_len_add", list(pm_a_len_new = 58))
    s4 <- step("pm_a_len_rm", list(pm_a_len_pick = "58"))
    s5 <- step("pm_a_cls_rm_hi"); s6 <- step("pm_a_cls_rm_lo")
    hi0 <- max(ax0$cls); lo0 <- min(ax0$cls)
    ok11 <- !nzchar(paste0(s1$e, s2$e, s3$e, s4$e, s5$e, s6$e)) &&
      identical(s1$ax$cls, c(ax0$cls, hi0 + 1L)) &&
      identical(s2$ax$cls, c(lo0 - 1L, ax0$cls, hi0 + 1L)) &&
      identical(s3$ax$len, sort(c(ax0$len, 58L))) && identical(s4$ax$len, ax0$len) &&
      identical(s6$ax$cls, ax0$cls) && identical(s6$ax$len, ax0$len)
    rec(browser_checks[7], if (ok11) PASS else FAIL,
        sprintf("classes %d-%d -> +larger %d-%d -> +smaller %d-%d -> back %d-%d | lengths %d -> +58: %d -> -58: %d%s",
                lo0, hi0, min(s1$ax$cls), max(s1$ax$cls), min(s2$ax$cls), max(s2$ax$cls),
                min(s6$ax$cls), max(s6$ax$cls), length(ax0$len), length(s3$ax$len),
                length(s4$ax$len), err_txt(s1$e, s2$e, s3$e, s4$e, s5$e, s6$e)))

    # 12. run with B = A (default selection: spruce, HKL 4; bonitet 17)
    busy_disabled <- NA; busy_status <- ""
    e12 <- try_drv({
      app$set_inputs(main_nav = "simulator")
      app$set_inputs(abonitet = "17")
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
    ok12 <- !nzchar(e12) && grepl("Trees with logs", cards, fixed = TRUE) &&
      identical(nerr12, 0L) && !cmp_tab12 && after_enabled
    progress_ok <- isTRUE(busy_disabled) && grepl("[A-Za-z]", busy_status)
    rec(browser_checks[8], if (!ok12) FAIL else if (progress_ok) PASS else REVIEW,
        sprintf("summary rendered: %s | comparison tab shown: %s | error notifications: %s | during the run: Run disabled %s, status '%s' | after: Run enabled %s%s",
                grepl("Trees with logs", cards, fixed = TRUE), cmp_tab12, nerr12,
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

    # 14. selected logs pagination
    e14 <- try_drv({ app$set_inputs(result_tabs = "logs"); app$wait_for_idle() })
    page_info <- txt(app, "#tbl_selected_logs_info")
    hdr14 <- js(app, "Array.from(document.querySelectorAll('#tbl_selected_logs .rt-th')).map(function(e){ return e.textContent.trim(); }).join('|')")
    h14 <- strsplit(as.character(hdr14), "|", fixed = TRUE)[[1]]
    n_rows14 <- n_tbl_rows(app, "tbl_selected_logs")
    ok14 <- !nzchar(e14) && !is.na(n_logs) &&
      grepl(sprintf("^1–%d of %s$", min(25L, n_logs), format(n_logs, big.mark = ",")), trimws(page_info)) &&
      identical(n_rows14, min(25L, n_logs)) &&
      all(c("Plot", "StartPos", "StopPos", "Assortment") %in% h14) &&
      !any(grepl("_", h14, fixed = TRUE)) && !any(c("ProductType", "ProductKey") %in% h14)
    rec(browser_checks[10], if (ok14) PASS else FAIL,
        sprintf("page info: '%s' | rows on the page: %s | logs in run: %s | headers: %s%s",
                trimws(page_info), n_rows14, n_logs, hdr14, err_txt(e14)))

    # 15. Per m3 tab and tree detail on demand
    e15 <- try_drv({ app$set_inputs(result_tabs = "per_m3"); app$wait_for_idle() })
    n_pm3   <- n_tbl_rows(app, "tbl_per_m3")
    n_asort <- n_tbl_rows(app, "tbl_per_m3_assortment")
    trees_card_js <- paste0(
      "Array.from(document.querySelectorAll('details.apter-card'))",
      ".find(function(e){ return e.querySelector('summary').textContent.trim() === 'Trees'; })")
    trees_closed <- isTRUE(js(app, paste0("!", trees_card_js, ".open")))
    open_trees <- function(app) {
      app$run_js(paste0("var d = ", trees_card_js, ";",
                        "if (d) { d.open = true; d.dispatchEvent(new Event('toggle')); }"))
      app$wait_for_idle()
    }
    open_trees(app)
    n_trees_rows <- n_tbl_rows(app, "tbl_trees")
    app$run_js("var r = document.querySelector('#tbl_trees .rt-tbody .rt-tr'); if (r) r.click();")
    app$wait_for_idle()
    n_detail <- js(app, "document.querySelectorAll('#tree_logs .reactable, #tree_logs .note-muted').length")
    page_text <- js(app, "document.body.textContent")
    per_ha_hits <- regmatches(page_text, gregexpr("(?i)per ha\\b|ha-expanded", page_text, perl = TRUE))[[1]]
    shoot(app, "03_per_m3")
    ok15 <- !nzchar(e15) && isTRUE(n_pm3 >= 2L) && isTRUE(n_asort >= 1L) && trees_closed &&
      isTRUE(n_trees_rows >= 1L) && isTRUE(n_detail >= 1L) && length(per_ha_hits) == 0L
    rec(browser_checks[11], if (ok15) PASS else FAIL,
        sprintf("per-m3 rows: %s | assortment rows: %s | Trees card starts closed: %s | tree rows: %s | log detail shown: %s | per-ha text hits: %d%s",
                n_pm3, n_asort, trees_closed, n_trees_rows, isTRUE(n_detail >= 1L),
                length(per_ha_hits), err_txt(e15)))
    stale_before <- txt(app, "#stale_notice")

    # 16. upload B (spruce sawlog x1.2), automatic comparison
    wide_csv <- tempfile(fileext = ".csv")
    g16 <- pm_to_grid(H$APP_PM_DEFAULT, "spruce_sawlog") * 1.2
    utils::write.csv(cbind(spruce_sawlog = rownames(g16), g16), wide_csv, row.names = FALSE)
    e16 <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$upload_file(pm_b_upload = wide_csv)
      app$wait_for_idle()
    })
    msg16 <- txt(app, "#msg_b")
    e16a <- try_drv({ app$set_inputs(pm_id_b = "spruce_sawlog", wait_ = FALSE); app$wait_for_idle() })
    n_hl  <- n_cells(app, "pm_b", ".pm-diff")
    n_hl_a <- n_cells(app, "pm_a", ".pm-diff")
    b16   <- dl_pm(app, "dl_b_long")
    n_diff16 <- if (is.null(b16)) NA else pm_n_diff(H$APP_PM_DEFAULT, b16)
    shoot(app, "04_prices_B_own")
    e16b <- try_drv({ app$set_inputs(main_nav = "simulator"); run(app) })
    cmp_tab16 <- tab_shown(app, "comparison")
    e16c <- try_drv({ app$set_inputs(result_tabs = "comparison"); app$wait_for_idle() })
    n_delta  <- n_tbl_rows(app, "tbl_cmp_delta")
    e16d <- try_drv({ app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle() })
    sum16 <- js(app, paste0("Array.from(document.querySelectorAll('#summary_cards .section-hdr, #summary_cards .ro-label'))",
                            ".map(function(e){ return e.textContent.trim(); }).join('|')"))
    shoot(app, "04b_summary_A_and_B")
    exp16 <- paste(c("Run time", "Plots", "Trees", "Scenario A", "Total value (NOK)", "Commercial volume (m³ u.b.)",
                     "NOK per m³", "Price volume (m³)", "Scenario B", "Total value (NOK)",
                     "Commercial volume (m³ u.b.)", "NOK per m³", "Price volume (m³)"), collapse = "|")
    e16e <- try_drv({ app$set_inputs(result_tabs = "comparison", wait_ = FALSE); app$wait_for_idle() })
    pat_txt  <- txt(app, "#cmp_patterns")
    stem_img <- js(app, "document.querySelectorAll('#stem_compare img').length")
    nerr16   <- n_err_notif(app)
    shoot(app, "05_price_matrix_comparison")
    # Handsontable renders the visible rows only: the DOM shows some
    # highlighted cells; B's download has all 351 cells changed.
    ok16 <- !nzchar(paste0(e16, e16a, e16b, e16c)) && isTRUE(n_hl > 0L) && isTRUE(n_hl_a > 0L) &&
      isTRUE(n_diff16 == 351) &&
      cmp_tab16 && isTRUE(n_delta >= 1L) &&
      grepl("trees changed their cutting pattern", pat_txt, fixed = TRUE) &&
      isTRUE(stem_img >= 1L) && identical(nerr16, 0L) && identical(sum16, exp16) &&
      !nzchar(paste0(e16d, e16e))
    rec(browser_checks[12], if (ok16) PASS else FAIL,
        sprintf("summary: '%s' | highlighted cells B %s, A %s; cells differing in B's download: %s | upload: '%s' | comparison tab: %s | delta rows %s | patterns: '%s' | stem diagram: %s | error notifications: %s%s",
                sum16, n_hl, n_hl_a, n_diff16, substr(trimws(msg16), 1, 100), cmp_tab16, n_delta,
                trimws(pat_txt), isTRUE(stem_img >= 1L), nerr16, err_txt(e16, e16a, e16b, e16c)))

    # 17. Spruce pulpwood removed from B only
    flat17 <- ""; st17 <- ""
    e17 <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$set_inputs(asm_pick_b = "spruce_pulpwood", wait_ = FALSE); app$click("asm_remove_b"); app$wait_for_idle()
      app$click("asm_rm_one"); app$wait_for_idle()
      flat17 <- txt(app, "#msg_b")
      st17 <- paste("B products:", paste(unique(dl_pm(app, "dl_b_long")$ProductKey), collapse = ","))
      app$set_inputs(main_nav = "simulator"); run(app)
      app$set_inputs(result_tabs = "comparison", wait_ = FALSE); app$wait_for_idle()
    })
    cmp17 <- txt(app, "#tbl_cmp_products")
    rows17 <- js(app, paste0("Array.from(document.querySelectorAll('#tbl_cmp_products .rt-tbody .rt-tr'))",
                             ".map(function(r){ return Array.from(r.querySelectorAll('.rt-td')).map(function(c){ return c.textContent.trim(); }).join(';'); }).join('\\n')"))
    pulp <- grep("^Spruce pulpwood", strsplit(as.character(rows17), "\n")[[1]], value = TRUE)
    # Columns: Assortment; Value A (NOK); Value B (NOK); ...
    b_val <- if (length(pulp)) suppressWarnings(as.numeric(strsplit(pulp[1], ";")[[1]][3])) else NA
    a_val <- if (length(pulp)) suppressWarnings(as.numeric(strsplit(pulp[1], ";")[[1]][2])) else NA
    e17b <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$set_inputs(asm_pick_b = "spruce_pulpwood", wait_ = FALSE); app$click("asm_copy_one_b"); app$wait_for_idle()
    })
    back17 <- "spruce_pulpwood" %in% dl_pm(app, "dl_b_long")$ProductKey
    ok17 <- !nzchar(paste0(e17, e17b)) && isTRUE(a_val > 0) && isTRUE(b_val == 0) && back17 &&
      !grepl("spruce_pulpwood", st17, fixed = TRUE) && identical(n_err_notif(app), 0L)
    rec(browser_checks[13], if (ok17) PASS else FAIL,
        sprintf("'%s' (%s) | spruce pulpwood value A %s, B %s | back in B after Make assortment equal to A: %s | error notifications: %s%s",
                trimws(flat17), trimws(st17), a_val, b_val, back17, n_err_notif(app), err_txt(e17, e17b)))

    # 18. stale notice after a price edit (reset A)
    e18 <- try_drv({
      app$set_inputs(result_tabs = "summary", wait_ = FALSE); app$wait_for_idle()
    })
    stale_after_run <- txt(app, "#stale_notice")
    e18b <- try_drv({
      app$set_inputs(main_nav = "prices"); app$click("pm_b_reset"); app$wait_for_idle()
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
                     asm_method = "All diameters (solid volume)", asm_price = 2000,
                     wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
    })
    msg20a <- txt(app, "#msg_a")
    n_asm1 <- n_tbl_rows(app, "tbl_asm_a")
    pm_opts <- js(app, paste0(
      "(function(){ var s = document.getElementById('pm_id_a');",
      " return s && s.selectize ? Object.keys(s.selectize.options).join(',')",
      " : Array.from(s.options).map(function(o){ return o.value; }).join(','); })()"))
    a_before <- dl_pm(app, "dl_a_long")
    e20b <- try_drv({
      app$set_inputs(asm_pick_a = "spruce_poles", wait_ = FALSE)
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
    a_reset <- dl_pm(app, "dl_a_long")
    base_back <- !is.null(a_reset) && identical(
      pm_minimal(a_reset[a_reset$ProductKey != "spruce_poles", ]), pm_minimal(H$APP_PM_DEFAULT))
    e20e <- try_drv({
      app$set_inputs(asm_pick_a = "spruce_poles", wait_ = FALSE); app$click("asm_remove_a"); app$wait_for_idle()
      app$click("asm_rm_both"); app$wait_for_idle()
    })
    n_asm_rm <- n_tbl_rows(app, "tbl_asm_a")
    n_asm_rm_b <- n_tbl_rows(app, "tbl_asm_b")
    ok20 <- !nzchar(paste0(e20, e20b, e20c, e20d, e20e)) &&
      grepl("Added Spruce poles", msg20a, fixed = TRUE) && identical(n_asm1, 7L) &&
      grepl("spruce_poles", pm_opts, fixed = TRUE) &&
      grepl("Saved Spruce poles", msg20b, fixed = TRUE) && kept &&
      grepl("Spruce poles", prod20, fixed = TRUE) &&
      identical(n_asm_reset, 7L) && base_back && identical(n_asm_rm, 6L) &&
      identical(n_asm_rm_b, 6L) && identical(form20, 0L) && identical(blur20, "none") &&
      identical(n_err_notif(app), 0L)
    rec(browser_checks[16], if (ok20) PASS else FAIL,
        sprintf("form: removed fields present %s, backdrop filter '%s' | add: '%s', rows %s, matrix choices %s | edit: '%s', prices kept %s | selected in run: %s | reset: rows %s, base prices back %s | remove from both: rows A %s, B %s%s",
                form20, blur20, trimws(msg20a), n_asm1, pm_opts, trimws(msg20b), kept,
                grepl("Spruce poles", prod20, fixed = TRUE), n_asm_reset, base_back, n_asm_rm, n_asm_rm_b,
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
    e21b <- try_drv({
      run(app)
      app$set_inputs(result_tabs = "per_m3", wait_ = FALSE); app$wait_for_idle()
      open_trees(app)
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

    # 24. Scenario B's own assortments (Phase 4d, independent scenarios)
    # B made equal to A as a user does it: download Scenario A, upload to B.
    a_to_b <- function(app) {
      f <- app$get_download("dl_scen_a")
      app$upload_file(scen_b_upload = f); app$wait_for_idle()
    }
    n24 <- list(); msg24 <- ""
    e24 <- try_drv({
      app$set_inputs(main_nav = "prices"); a_to_b(app)
      n24$start <- marks(app)
      # a rule of B only: spruce sawlog min top 14 cm
      app$set_inputs(asm_pick_b = "spruce_sawlog", wait_ = FALSE); app$click("asm_edit_b"); app$wait_for_idle()
      app$set_inputs(asm_min_top = 14, wait_ = FALSE); app$click("asm_save"); app$wait_for_idle()
      n24$diff_a <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-diff').length")
      n24$diff_b <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-diff').length")
      # an assortment added to B only
      app$click("asm_add_b"); app$wait_for_idle()
      app$set_inputs(asm_name = "Poles", asm_species = "spruce", asm_price_type = "flat",
                     asm_min_top = 15, asm_min_len = 800, asm_max_len = 1200, asm_price = 900,
                     asm_both = FALSE, wait_ = FALSE)
      app$click("asm_save"); app$wait_for_idle()
      n24$only_b <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-only').length")
      n24$none_a <- js(app, "document.querySelectorAll('#tbl_asm_a .asm-none').length")
      n24$a_rows <- n_tbl_rows(app, "tbl_asm_a")
      # pine pulpwood removed from B only
      app$set_inputs(asm_pick_b = "pine_pulpwood", wait_ = FALSE); app$click("asm_remove_b"); app$wait_for_idle()
      app$click("asm_rm_one"); app$wait_for_idle()
      n24$none_b <- js(app, "document.querySelectorAll('#tbl_asm_b .asm-none').length")
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
      app$set_inputs(result_tabs = "comparison", wait_ = FALSE); app$wait_for_idle()
      shoot(app, "10_scenario_comparison")
    })
    prod24 <- txt(app, "#tbl_products")
    cmp24 <- tab_shown(app, "comparison")
    e24b <- try_drv({
      app$set_inputs(main_nav = "prices")
      app$set_inputs(pm_id_a = "spruce_sawlog", wait_ = FALSE); app$click("pm_a_copy_one"); app$wait_for_idle()
      msg24 <- txt(app, "#msg_a")
      a_to_b(app)
      n24$end <- marks(app)
    })
    ok24 <- !nzchar(paste0(e24, e24b)) && identical(n24$start, 0L) &&
      isTRUE(n24$diff_a >= 1L) && isTRUE(n24$diff_b >= 1L) &&
      isTRUE(n24$only_b >= 1L) && isTRUE(n24$none_a >= 1L) && identical(n24$a_rows, 7L) &&
      isTRUE(n24$none_b >= 1L) && cmp24 &&
      identical(n24$rows_a, n24$rows_b) &&
      identical(n24$rows_a, "Sawlog Spruce|Pulpwood Spruce|Waste Spruce|Poles Spruce|Sawlog Pine|Pulpwood Pine|Waste Pine") && grepl("Spruce poles", prod24, fixed = TRUE) &&
      grepl("Value A", prod24, fixed = TRUE) && grepl("Prices equal to Scenario B", msg24, fixed = TRUE) &&
      identical(n24$end, 0L) && identical(n_err_notif(app), 0L)
    rec(browser_checks[20], if (ok24) PASS else FAIL,
        sprintf("rows A: %s | rows B: %s | start marks %s | B rule highlighted: A %s, B %s | only in B: %s (A shows 'Not in A': %s, A rows %s) | removed from B: 'Not in B' %s | comparison tab %s, poles in products %s | Make matrix equal to B: '%s' | after Scenario A downloaded and uploaded to B: %s marks%s",
                n24$rows_a, n24$rows_b, n24$start, n24$diff_a, n24$diff_b, n24$only_b, n24$none_a, n24$a_rows, n24$none_b,
                cmp24, grepl("Spruce poles", prod24, fixed = TRUE), substr(trimws(msg24), 1, 120),
                n24$end, err_txt(e24, e24b)))
    rec(browser_checks[18], if (length(shots) == 15L) PASS else REVIEW,
        sprintf("%d of 15 screenshots in %s", length(shots), shot_dir))
    try(app$stop(), silent = TRUE)
  } else {
    for (ck in browser_checks[c(2:18, 20)]) rec(ck, FAIL, "skipped: app did not start")
  }

  # 23. runtime
  t_el <- as.numeric((proc.time() - t_browser)["elapsed"])
  rec(browser_checks[19], if (t_el < 300) PASS else REVIEW,
      sprintf("browser checks took %.1f s (threshold: 300 s)", t_el))
}


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 17 complete.\n")
cat("Results written to:", out_dir, "\n")
