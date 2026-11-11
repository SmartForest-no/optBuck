# ==============================================================================
# scripts/restructure_screenshots.R (restructure, 2026-10-30)
#
# Review screenshots of the restructured app, one language per run:
#   Rscript scripts/restructure_screenshots.R en     (or: no)
# into outputs/app_smoke/review/restructure/ (gitignored). Uses the invented
# tuning fixture (inputs/test_fixtures/hpr_tuning_synthetic.hpr) only.
# ==============================================================================
args <- commandArgs(TRUE); lang <- if (length(args)) args[1L] else "en"
root <- normalizePath(getwd(), winslash = "/")
stopifnot(file.exists(file.path(root, "R", "helpers_paths.R")), lang %in% c("en", "no"))
Sys.setenv(OPTBUCK_ROOT = root, NOT_CRAN = "true", OPTBUCK_N_PLOTS = "5", OPTBUCK_TUNE_MAX_ITER = "3")
if (lang == "en") Sys.setenv(OPTBUCK_LANG = "en") else Sys.unsetenv("OPTBUCK_LANG")
options(chromote.timeout = 60)
suppressMessages(pkgload::load_all(root, quiet = TRUE))   # the package (R/, inst/); source(file.path(root, "audits", "helpers_audit.R"))
dir <- file.path(root, "outputs", "app_smoke", "review", "restructure"); dir.create(dir, recursive = TRUE, showWarnings = FALSE)
fx <- file.path(root, "inst", "extdata", "inputs", "test_fixtures", "hpr_tuning_synthetic.hpr")
key <- "spruce_140_sagtommer_gran"
audit_browser_start()
res <- tryCatch({
  app <- audit_app_driver(file.path(root, "inst", "app"), name = paste0("shots_", lang), load_timeout = 60000, timeout = 180000, width = 1400, height = 1000)
  stopifnot(inherits(app, "AppDriver"))
  shot <- function(name) { f <- file.path(dir, sprintf("%s_%s.png", name, lang)); unlink(f); app$get_screenshot(f); cat("saved", basename(f), "\n") }
  nav <- function(v) { app$set_inputs(main_nav = v, wait_ = FALSE); app$wait_for_idle(); Sys.sleep(0.8) }
  app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30000)
  nav("stems"); shot("01_stems_nfi")
  app$set_inputs(tree_source = "hpr", wait_ = FALSE); app$wait_for_idle()
  app$upload_file(hs_upload = fx)
  app$wait_for_js("document.querySelector('#hs_validation') && /120/.test(document.querySelector('#hs_validation').innerText)", timeout = 90000)
  app$wait_for_idle(); Sys.sleep(1); shot("02_stems_hpr")
  nav("prices"); shot("03_assortments_and_prices")
  app$upload_file(scen_a_upload = fx); app$wait_for_idle(); Sys.sleep(1); shot("04_import_dialog")
  app$click("hpr_apply"); app$wait_for_idle(); Sys.sleep(1)
  app$run_js(sprintf("Shiny.setInputValue('asm_pick', '%s', {priority: 'event'});", key))
  # select the assortment by clicking its row in A's table
  app$run_js("var r = Array.from(document.querySelectorAll('#tbl_asm_a .rt-tbody .rt-tr')).find(function(x){ return /140/.test(x.innerText); }); if (r) r.click();")
  app$wait_for_idle(); Sys.sleep(1)
  app$set_inputs(pm_a_view = "target", wait_ = FALSE); app$wait_for_idle()
  app$wait_for_js("!!document.querySelector('#dt_a .ht_master td')", timeout = 30000)
  app$click("dt_a_even"); app$wait_for_idle(); Sys.sleep(1); shot("05_target_distribution_view")
  app$set_inputs(pm_a_view = "prices", wait_ = FALSE); app$wait_for_idle()
  nav("simulator"); shot("06_simulator")
  app$click("run_btn")
  app$wait_for_js("!document.querySelector('#run_status .status-running') && !document.getElementById('run_btn').disabled && !!document.querySelector('#summary_cards .ro-value')", timeout = 120000)
  app$wait_for_idle(); Sys.sleep(1); shot("07_simulator_results")
  nav("tuning"); app$wait_for_js("!!document.getElementById('tn_run')", timeout = 30000)
  shot("08_tuning_before")
  app$click("tn_run")
  app$wait_for_js("!!document.querySelector('#tn_tbl .rt-tr') && !!document.querySelector('#tn_matrix .rt-tr')", timeout = 240000)
  app$wait_for_idle(); Sys.sleep(1)
  for (v in c("target", "orig", "tuned", "change")) {
    app$set_inputs(tn_matrix_view = v, wait_ = FALSE); app$wait_for_idle(); Sys.sleep(0.8); shot(paste0("09_tuning_", v))
  }
  "done"
}, error = function(e) paste("ERROR:", conditionMessage(e)), finally = audit_browser_stop())
cat("result:", res, "\n")
