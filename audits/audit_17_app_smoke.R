# ==============================================================================
# audits/audit_17_app_smoke.R
#
# Purpose : Headless smoke audit for the Shiny app (app/). Checks the split
#           file layout, root resolution and pure app helpers without a
#           browser, then drives the running app through shinytest2.
#           Assertions are explicit; no shinytest2 snapshot files are used.
#
# Checks  :
#  1.  shinytest2 and chromote are installed and a Chrome is found
#  2.  Split app files exist (app.R, app_ui.R, app_server.R, setup.R,
#      theme.R, helpers_*.R, www/apter.css, www/apter-icon.svg) and app/
#      has no ui.R/server.R (Shiny would treat those as a second app format;
#      shinytest2 refuses to start)
#  3.  Root resolution (no browser):
#        a. APTER_ROOT unset              -> parent of app/ (project root)
#        b. APTER_ROOT = empty temp dir   -> clear error
#        c. APTER_ROOT = project root     -> project root
#  4.  Pure helpers (no browser):
#        a. write_scenario_pm() writes a scenario that uploads back unchanged
#        b. build_run_params(): HKL "4" -> c(41L, 42L), "__all__" -> NULL
#        c. count_eligible_plots() matches a direct filter of plots_prod
#        d. price-matrix editor setup: three matrices (spruce sawlog, pine
#           sawlog, flat prices) and the interpolated 31 dm cells marked
#  5.  AppDriver$new("app") starts without error (load timeout 60 s)
#  6.  After switching to the Simulator page: run_status shows the idle
#      text; window title contains "Apter"
#  7.  Main run (spruce, HKL 4, abonitet 17, 5 plots, seed 42):
#      run_status contains "Run complete", no error notification
#  8.  summary_cards is non-empty; tbl_products has >= 1 row
#  9.  Scenario comparison with an unchanged scenario: cmp_status says
#      "Same plots sampled"
# 10.  Scenario upload (Phase 4a): a wide CSV with spruce sawlog x1.2 on the
#      Prices page -> 351 cells differ; Run -> difference table, the
#      cutting-pattern line and the stem diagram render, no error
# 11.  Home view (Phase 2): serif "Apter" title, three tree-source tiles,
#      footer "Release <VERSION>"
# 12.  Theme: Bootstrap 5 loaded, self-hosted Inter font loaded, no Google
#      Fonts request
# 13.  Run step list: all backend stages (RUN_STAGE_LABELS, 11 since the
#      Phase 3 bark stage) shown as done after a run
# 14.  Selected logs table is paginated ("1 - 10 of N", N = selected logs)
# 15.  Screenshots of Home, Simulator, Products, Scenario comparison,
#      Prices and Per m3 saved to outputs/app_smoke/screenshots/ (for visual
#      review against docs/design_reference/)
# 16.  Per m3 tab (Phase 3): NOK/m3 tables render, the collapsed Trees card
#      expands to tree rows and a tree's logs; no "per ha" / "ha-expanded"
#      text anywhere in the page
# 17.  Price-matrix editor: default and scenario grids render side by side,
#      uploaded changes are highlighted, Reset brings the count back to 0
# 18.  Stale-scenario notice: absent right after a run; after the scenario
#      is edited (reset), "Scenario prices changed since last run" shows on
#      the Run card and the Scenario comparison tab
# 19.  Total browser runtime under 180 s (REVIEW if above)
# 20.  Custom assortment (Phase 4b): added through the Add assortment form
#      (spruce poles, scenario only, grid at 2000 NOK/m3) -> listed in
#      tbl_assortments and in the matrix choices; editing it (one length
#      replaced) reports the scenario's 38 remaining cells kept; a comparison
#      run shows
#      'poles' in the per-m3 comparison, without error notifications
#
# Verdict :
#   PASS                   -- all checks pass
#   ACCEPTED WITH REVIEW   -- only the runtime check is REVIEW
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

app_files <- c("app.R", "app_ui.R", "app_server.R", "setup.R", "theme.R",
               "helpers_root.R", "helpers_ui.R", "helpers_validation.R",
               "helpers_upload.R", "www/apter.css", "www/apter-icon.svg")
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
helpers_err <- tryCatch({
  suppressPackageStartupMessages(library(shiny))
  source(file.path(root_dir, "scripts", "app_backend",
                   "01_run_sampled_plot_bucking.R"), local = FALSE)
  for (f in c("setup.R", "theme.R", "helpers_ui.R", "helpers_validation.R",
             "helpers_upload.R"))
    source(file.path(app_dir, f), local = H)
  ""
}, error = function(e) conditionMessage(e))

if (!nzchar(helpers_err)) {
  # a. write_scenario_pm round trip
  pm_a <- H$APP_PM_DEFAULT
  pm_a$PriceNOKperM3[grepl("sawlog", pm_a$ProductKey)] <-
    pm_a$PriceNOKperM3[grepl("sawlog", pm_a$ProductKey)] * 1.2
  f_a  <- H$write_scenario_pm(pm_a)
  up_a <- apply_price_upload(f_a, f_a, H$APP_PM_DEFAULT, H$APP_PM_DEFAULT,
                             H$APP_PRODUCTS, H$APP_LC)
  file.remove(f_a)
  ok_a <- isTRUE(up_a$ok) && identical(up_a$pm, pm_minimal(pm_a))
  rec("Check 04a: write_scenario_pm() writes a scenario that uploads back unchanged",
      if (ok_a) PASS else FAIL,
      sprintf("upload ok: %s | identical: %s | cells differing from default: %d",
              isTRUE(up_a$ok), ok_a, pm_n_diff(H$APP_PM_DEFAULT, pm_a)))

  # b. build_run_params
  p_all <- H$build_run_params(list(species_group = "spruce", hkl_group = "4",
                                   abonitet = c("14", "17"),
                                   fylnr_sel = "__all__", n_plots = "5",
                                   seed = 42))
  some_fyl <- H$.DATA_FYLNR[1L]
  p_fyl <- H$build_run_params(list(species_group = "pine",
                                   hkl_group = c("3", "5"), abonitet = "11",
                                   fylnr_sel = some_fyl, n_plots = "10",
                                   seed = 7))
  # hogstkl carries names from unlist(); only the codes matter.
  ok_b <- identical(unname(p_all$hogstkl), c(41L, 42L)) &&
          is.null(p_all$fylnr) &&
          identical(p_all$abonitet, c(14L, 17L)) &&
          identical(p_all$n_plots, 5L) && identical(p_all$seed, 42L) &&
          identical(unname(p_fyl$hogstkl), c(31L, 32L, 51L, 52L)) &&
          identical(p_fyl$fylnr, some_fyl)
  rec("Check 04b: build_run_params() maps HKL groups and county",
      if (ok_b) PASS else FAIL,
      sprintf("HKL '4' -> %s; '__all__' -> %s; HKL '3','5' -> %s; fylnr '%s' -> %s",
              paste(p_all$hogstkl, collapse = ","),
              if (is.null(p_all$fylnr)) "NULL" else p_all$fylnr,
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

  # d. editor setup
  ch_d  <- unname(H$PM_CHOICES)
  ik    <- H$APP_PM_INTERP_KEYS
  ok_d  <- identical(ch_d, c("spruce_sawlog", "pine_sawlog", "flat_prices")) &&
    length(ik) > 0L && all(grepl(" 31 ", ik, fixed = TRUE))
  rec("Check 04d: Editor matrices and interpolated 31 dm cells",
      if (ok_d) PASS else FAIL,
      sprintf("matrices: %s | interpolated cells: %d (all 31 dm: %s)",
              paste(ch_d, collapse = ", "), length(ik),
              all(grepl(" 31 ", ik, fixed = TRUE))))
} else {
  rec("Check 04: Pure app helpers", FAIL,
      paste("sourcing backend/app files failed:", helpers_err))
}


# ==============================================================================
# Checks 05-11: drive the app with shinytest2
# ==============================================================================
browser_checks <- c(
  "Check 05: AppDriver starts the app",
  "Check 06: Simulator run_status idle and title contains 'Apter'",
  "Check 07: Main run completes without error notification",
  "Check 08: summary_cards non-empty and tbl_products has rows",
  "Check 09: Scenario comparison 'same' samples the same plots",
  "Check 10: Uploaded scenario runs; comparison, pattern line and stem diagram render",
  "Check 11: Home view shows Apter title, source tiles and release footer",
  "Check 12: Bootstrap 5 and self-hosted Inter font loaded",
  "Check 13: Run step list shows all stages done",
  "Check 14: Selected logs table is paginated",
  "Check 15: Screenshots saved for visual review",
  "Check 16: Per m3 tab renders; Trees card expands; no per-ha text",
  "Check 17: Price-matrix editor: grids side by side, highlights, reset",
  "Check 18: Stale-scenario notice after editing prices",
  "Check 19: Browser runtime under 180 s",
  "Check 20: Custom assortment added in the UI runs in the comparison")

if (!browser_ok) {
  for (ck in browser_checks)
    rec(ck, FAIL, "skipped: browser dependencies missing (see Check 01)")
} else {
  section("05-16. Browser checks")

  txt <- function(app, sel)
    tryCatch(paste(app$get_text(sel), collapse = " "), error = function(e) "")
  n_err_notif <- function(app)
    app$get_js("document.querySelectorAll('.shiny-notification-error').length")
  n_tbl_rows <- function(app, id)
    app$get_js(sprintf(
      "document.querySelectorAll('#%s .rt-tbody .rt-tr-group').length", id))
  shot_dir <- file.path(out_dir, "screenshots")
  ensure_dir(shot_dir)
  shots <- character(0)
  shoot <- function(app, name) {
    f <- file.path(shot_dir, paste0(name, ".png"))
    if (file.exists(f)) file.remove(f)   # don't count a stale file from a past run
    ok <- tryCatch({ app$get_screenshot(f); file.exists(f) },
                   error = function(e) {
                     message("screenshot ", name, " failed: ", conditionMessage(e))
                     FALSE
                   })
    if (ok) shots <<- c(shots, f)
  }
  app_version <- trimws(readLines(file.path(root_dir, "VERSION"), n = 1L,
                                  warn = FALSE))

  # AppDriver$new() skips (as "On CRAN") outside interactive sessions unless set.
  Sys.setenv(NOT_CRAN = "true")
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

    # 11-12. Home view and theme (Home is the landing page)
    app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30 * 1000)
    home_title <- txt(app, ".apter-title")
    n_tiles    <- app$get_js("document.querySelectorAll('.status-tile').length")
    footer     <- txt(app, ".apter-footer")
    ok11 <- identical(trimws(home_title), "Apter") && n_tiles >= 3L &&
            grepl(sprintf("Release %s", app_version), footer, fixed = TRUE)
    det11 <- sprintf("title: '%s' | status tiles: %d | footer: '%s'",
                     trimws(home_title), n_tiles, trimws(footer))
    bs_ver <- app$get_js(
      "typeof bootstrap !== 'undefined' ? bootstrap.Tooltip.VERSION : ''")
    inter_loaded <- app$get_js(paste0(
      "Array.from(document.fonts).some(f => f.family.replace(/[\"']/g, '') ",
      "=== 'Inter' && f.status === 'loaded')"))
    n_gfonts <- app$get_js(
      "document.querySelectorAll('link[href*=\"fonts.googleapis\"]').length")
    ok12 <- startsWith(bs_ver, "5") && isTRUE(inter_loaded) && n_gfonts == 0L
    det12 <- sprintf("Bootstrap %s | Inter loaded: %s | Google Fonts links: %d",
                     bs_ver, isTRUE(inter_loaded), n_gfonts)
    shoot(app, "01_home")

    # 06. idle state and title (outputs on hidden pages don't render)
    app$set_inputs(main_nav = "Simulator")
    st0   <- txt(app, "#run_status")
    title <- app$get_js("document.title")
    ok6   <- grepl("Select parameters and click Run optimizer", st0, fixed = TRUE) &&
             grepl("Apter", title, fixed = TRUE)
    rec(browser_checks[2], if (ok6) PASS else FAIL,
        sprintf("run_status: '%s' | title: '%s'", st0, title))

    # 07. main run
    run_err <- tryCatch({
      # spruce / HKL 4 are the defaults: no output changes, so don't wait.
      app$set_inputs(species_group = "spruce", hkl_group = "4", wait_ = FALSE)
      app$set_inputs(abonitet = "17", n_plots = "5", seed = 42)
      app$click("run_btn")
      app$wait_for_idle(timeout = 120 * 1000)
      ""
    }, error = function(e) conditionMessage(e))
    st1  <- txt(app, "#run_status")
    nerr <- n_err_notif(app)
    ok7  <- !nzchar(run_err) && grepl("Run complete", st1, fixed = TRUE) &&
            nerr == 0L
    rec(browser_checks[3], if (ok7) PASS else FAIL,
        sprintf("run_status: '%s' | error notifications: %d%s", st1, nerr,
                if (nzchar(run_err)) paste(" | driver error:", run_err) else ""))
    n_steps_done <- app$get_js(
      "document.querySelectorAll('#run_steps .step-icon:not(.is-pending)').length")
    n_steps      <- app$get_js("document.querySelectorAll('#run_steps .step-row').length")
    n_sel_logs   <- suppressWarnings(as.integer(
      sub(".*\\|\\s*(\\d+) selected logs.*", "\\1", st1)))
    shoot(app, "02_simulator_run")

    # 08. summary cards and product table (tabs render only when visible)
    cards <- txt(app, "#summary_cards")
    app$set_inputs(result_tabs = "Products")
    app$wait_for_idle()
    n_prod <- n_tbl_rows(app, "tbl_products")
    shoot(app, "03_products")
    rec(browser_checks[4], if (nzchar(trimws(cards)) && n_prod >= 1L) PASS else FAIL,
        sprintf("summary_cards chars: %d | tbl_products rows: %d",
                nchar(trimws(cards)), n_prod))

    # 09. scenario comparison, same prices
    cmp_err <- tryCatch({
      # These inputs only feed isolate()d code or client-side panels, so no
      # output updates; waiting for one would block until the driver timeout.
      app$set_inputs(compare_on = TRUE, wait_ = FALSE)
      app$set_inputs(result_tabs = "Scenario comparison")
      app$click("run_btn")
      app$wait_for_idle(timeout = 120 * 1000)
      ""
    }, error = function(e) conditionMessage(e))
    cmp <- txt(app, "#cmp_status")
    rec(browser_checks[5],
        if (!nzchar(cmp_err) && grepl("Same plots sampled", cmp, fixed = TRUE))
          PASS else FAIL,
        sprintf("cmp_status: '%s'%s", cmp,
                if (nzchar(cmp_err)) paste(" | driver error:", cmp_err) else ""))

    # 10. scenario upload on the Prices page, then a comparison run
    wide_csv <- tempfile(fileext = ".csv")
    g10 <- pm_to_grid(H$APP_PM_DEFAULT, "spruce_sawlog") * 1.2
    utils::write.csv(cbind(spruce_sawlog = rownames(g10), g10), wide_csv,
                     row.names = FALSE)
    up_err <- tryCatch({
      app$set_inputs(main_nav = "Prices and assortments")
      app$upload_file(pm_upload = wide_csv)
      app$wait_for_idle()
      ""
    }, error = function(e) conditionMessage(e))
    pm_status_up <- txt(app, "#pm_status")
    upload_msg   <- txt(app, "#pm_upload_msg")
    n_hl <- app$get_js("document.querySelectorAll('#pm_scenario td.pm-diff').length")
    grids_side <- app$get_js(paste0(
      "(function(){ var a = document.querySelector('#pm_default .handsontable'),",
      " b = document.querySelector('#pm_scenario .handsontable');",
      " if (!a || !b) return false;",
      " var ra = a.getBoundingClientRect(), rb = b.getBoundingClientRect();",
      " return rb.left > ra.right - 5 && Math.abs(ra.top - rb.top) < 40; })()"))
    shoot(app, "05_prices")
    run10_err <- tryCatch({
      app$set_inputs(main_nav = "Simulator")
      app$click("run_btn")
      app$wait_for_idle(timeout = 120 * 1000)
      ""
    }, error = function(e) conditionMessage(e))
    n_delta   <- n_tbl_rows(app, "tbl_cmp_delta")
    n_per_m3c <- n_tbl_rows(app, "tbl_cmp_per_m3")
    pat_txt   <- txt(app, "#cmp_patterns")
    stem_img  <- app$get_js("document.querySelectorAll('#stem_compare img').length")
    nerr2     <- n_err_notif(app)
    shoot(app, "04_scenario_comparison")
    stale_before <- paste(txt(app, "#scenario_stale_run"), txt(app, "#scenario_stale_cmp"))
    ok10 <- !nzchar(up_err) && !nzchar(run10_err) &&
      grepl("351 cell(s) differ", pm_status_up, fixed = TRUE) &&
      n_delta >= 1L && n_per_m3c >= 1L &&
      grepl("trees changed their cutting pattern", pat_txt, fixed = TRUE) &&
      stem_img >= 1L && nerr2 == 0L
    rec(browser_checks[6], if (ok10) PASS else FAIL,
        sprintf("status after upload: '%s' | delta rows %d | per-m3 rows %d | patterns: '%s' | stem diagram: %s | error notifications: %d%s",
                trimws(pm_status_up), n_delta, n_per_m3c, trimws(pat_txt),
                stem_img >= 1L, nerr2,
                paste0(if (nzchar(up_err)) paste(" | upload error:", up_err) else "",
                       if (nzchar(run10_err)) paste(" | run error:", run10_err) else "")))

    # 14. Selected logs pagination
    app$set_inputs(result_tabs = "Selected logs")
    page_info <- txt(app, "#tbl_selected_logs .rt-page-info")
    ok14 <- !is.na(n_sel_logs) &&
      grepl(sprintf("^1 - %d of %d$", min(10L, n_sel_logs), n_sel_logs),
            trimws(page_info))
    det14 <- sprintf("page info: '%s' | selected logs in run: %s",
                     trimws(page_info), n_sel_logs)

    # 16. Per m3 tab and tree detail on demand
    per_m3_err <- tryCatch({
      app$set_inputs(result_tabs = "Per m\u00b3")
      app$wait_for_idle()
      ""
    }, error = function(e) conditionMessage(e))
    n_pm3   <- n_tbl_rows(app, "tbl_per_m3")
    n_asort <- n_tbl_rows(app, "tbl_per_m3_assortment")
    trees_card_js <- paste0(
      "Array.from(document.querySelectorAll('details.apter-card'))",
      ".find(function(e){ return e.querySelector('summary').textContent.trim() === 'Trees'; })")
    trees_closed <- isTRUE(app$get_js(paste0("!", trees_card_js, ".open")))
    app$run_js(paste0(
      "var d = Array.from(document.querySelectorAll('details.apter-card'))",
      ".find(function(e){ return e.querySelector('summary').textContent.trim() === 'Trees'; });",
      "if (d) { d.open = true; d.dispatchEvent(new Event('toggle')); }"))
    app$wait_for_idle()
    n_trees_rows <- n_tbl_rows(app, "tbl_trees")
    app$run_js(paste0(
      "var b = document.querySelector('#tbl_trees .rt-expander-button');",
      "if (b) b.click();"))
    app$wait_for_idle()
    n_detail <- app$get_js(
      "document.querySelectorAll('#tbl_trees .tree-logs, #tbl_trees .note-muted').length")
    page_text <- app$get_js("document.body.textContent")
    per_ha_hits <- regmatches(page_text, gregexpr("(?i)per ha\\b|ha-expanded",
                                                  page_text, perl = TRUE))[[1]]
    shoot(app, "06_per_m3")
    ok16 <- !nzchar(per_m3_err) && n_pm3 >= 2L && n_asort >= 1L &&
      trees_closed && n_trees_rows >= 1L && n_detail >= 1L &&
      length(per_ha_hits) == 0L
    det16 <- sprintf("per-m3 rows: %d | assortment rows: %d | Trees card starts closed: %s | tree rows: %d | log detail shown: %s | per-ha text hits: %d%s",
                     n_pm3, n_asort, trees_closed, n_trees_rows, n_detail >= 1L,
                     length(per_ha_hits),
                     if (nzchar(per_m3_err)) paste(" | driver error:", per_m3_err) else "")

    # 17. editor: reset the uploaded matrix
    reset_err <- tryCatch({
      app$set_inputs(main_nav = "Prices and assortments")
      app$click("pm_reset")
      app$wait_for_idle()
      ""
    }, error = function(e) conditionMessage(e))
    pm_status_reset <- txt(app, "#pm_status")
    n_hl_reset <- app$get_js("document.querySelectorAll('#pm_scenario td.pm-diff').length")
    ok17 <- isTRUE(grids_side) && n_hl > 0L && !nzchar(reset_err) &&
      grepl("^0 cell\\(s\\) differ", trimws(pm_status_reset)) && n_hl_reset == 0L
    # 18. stale-scenario notice after the reset (prices changed since the run)
    stale_err <- tryCatch({
      app$set_inputs(main_nav = "Simulator")
      app$set_inputs(result_tabs = "Scenario comparison")
      app$wait_for_idle()
      ""
    }, error = function(e) conditionMessage(e))
    stale_run <- txt(app, "#scenario_stale_run")
    stale_cmp <- txt(app, "#scenario_stale_cmp")
    msg18 <- "Scenario prices changed since last run"
    ok18 <- !nzchar(stale_err) && !grepl(msg18, stale_before, fixed = TRUE) &&
      grepl(msg18, stale_run, fixed = TRUE) && grepl(msg18, stale_cmp, fixed = TRUE)
    det18 <- sprintf("before edit: '%s' | after reset, Run card: '%s' | comparison tab: '%s'%s",
                     trimws(stale_before), trimws(stale_run), trimws(stale_cmp),
                     if (nzchar(stale_err)) paste(" | driver error:", stale_err) else "")
    det17 <- sprintf("grids side by side: %s | highlighted cells after upload: %d | after reset: '%s', highlighted %d | upload message: '%s'%s",
                     isTRUE(grids_side), n_hl, trimws(pm_status_reset), n_hl_reset,
                     substr(trimws(upload_msg), 1, 120),
                     if (nzchar(reset_err)) paste(" | driver error:", reset_err) else "")

    rec(browser_checks[7], if (ok11) PASS else FAIL, det11)
    rec(browser_checks[8], if (ok12) PASS else FAIL, det12)
    n_expected <- if (is.data.frame(H$RUN_STAGE_LABELS)) nrow(H$RUN_STAGE_LABELS) else NA_integer_
    rec(browser_checks[9],
        if (isTRUE(n_steps == n_expected) && n_steps_done == n_steps) PASS else FAIL,
        sprintf("%d of %d steps done (expected %s)", n_steps_done, n_steps, n_expected))
    rec(browser_checks[10], if (ok14) PASS else FAIL, det14)
    rec(browser_checks[11], if (length(shots) == 6L) PASS else REVIEW,
        sprintf("%d of 6 screenshots in %s", length(shots), shot_dir))
    rec(browser_checks[12], if (ok16) PASS else FAIL, det16)
    rec(browser_checks[13], if (ok17) PASS else FAIL, det17)
    rec(browser_checks[14], if (ok18) PASS else FAIL, det18)

    # 20. custom assortment through the form, then a comparison run
    asm_err <- tryCatch({
      app$set_inputs(main_nav = "Prices and assortments")
      app$click("asm_add")
      app$wait_for_idle()
      app$set_inputs(asm_name = "Poles", asm_class = "poles", asm_species = "spruce",
                     asm_use_in = "scenario", asm_min_top = 12, asm_max_top = 30,
                     asm_lengths = "600;800;1000",
                     asm_method = "All diameters (solid volume)",
                     asm_price_type = "matrix", asm_price = 2000, wait_ = FALSE)
      app$click("asm_save")
      app$wait_for_idle()
      # Edit it (one length replaced): prices of remaining cells are kept.
      app$click("asm_edit")
      app$wait_for_idle()
      app$set_inputs(asm_lengths = "600;800;1200", wait_ = FALSE)
      app$click("asm_save")
      app$wait_for_idle()
      ""
    }, error = function(e) conditionMessage(e))
    edit_msg <- txt(app, "#asm_msg")
    n_asm   <- n_tbl_rows(app, "tbl_assortments")
    asm_txt <- txt(app, "#tbl_assortments")
    pm_opts <- app$get_js(paste0(
      "(function(){ var s = document.getElementById('pm_id');",
      " return s && s.selectize ? Object.keys(s.selectize.options).join(',')",
      " : Array.from(s.options).map(function(o){ return o.value; }).join(','); })()"))
    shoot20 <- tryCatch({ app$get_screenshot(file.path(shot_dir, "07_assortments.png")); TRUE },
                        error = function(e) FALSE)
    run20_err <- tryCatch({
      app$set_inputs(main_nav = "Simulator")
      # Check 18 left this tab open: setting it again changes no output, so
      # don't wait for one (the driver would wait out its full timeout).
      app$set_inputs(result_tabs = "Scenario comparison", wait_ = FALSE)
      app$click("run_btn")
      app$wait_for_idle(timeout = 120 * 1000)
      ""
    }, error = function(e) conditionMessage(e))
    cmp20  <- txt(app, "#tbl_cmp_per_m3")
    nerr20 <- n_err_notif(app)
    ok20 <- !nzchar(asm_err) && !nzchar(run20_err) && n_asm == 1L &&
      grepl("Saved Poles (Scenario only): ", edit_msg, fixed = TRUE) &&
      grepl("38 kept in the scenario", edit_msg, fixed = TRUE) &&
      grepl("Poles", asm_txt, fixed = TRUE) &&
      grepl("spruce_poles", pm_opts, fixed = TRUE) &&
      grepl("poles", cmp20, fixed = TRUE) && nerr20 == 0L
    rec(browser_checks[16], if (ok20) PASS else FAIL,
        sprintf("assortment rows %d | after edit: '%s' | matrix choices: %s | 'poles' in per-m3 comparison: %s | error notifications: %d%s",
                n_asm, trimws(edit_msg), pm_opts, grepl("poles", cmp20, fixed = TRUE), nerr20,
                paste0(if (nzchar(asm_err)) paste(" | form error:", asm_err) else "",
                       if (nzchar(run20_err)) paste(" | run error:", run20_err) else "")))

    try(app$stop(), silent = TRUE)
  } else {
    for (ck in browser_checks[c(2:14, 16)])
      rec(ck, FAIL, "skipped: app did not start")
  }

  # 19. runtime
  t_el <- as.numeric((proc.time() - t_browser)["elapsed"])
  rec(browser_checks[15], if (t_el < 180) PASS else REVIEW,
      sprintf("browser checks took %.1f s (threshold: 180 s)", t_el))
}


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 17 complete.\n")
cat("Results written to:", out_dir, "\n")
