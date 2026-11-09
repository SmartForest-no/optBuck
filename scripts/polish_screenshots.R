# ==============================================================================
# scripts/polish_screenshots.R (R4, 2026-11-02)
#
# Review screenshots of every tab in one language: the Stems panel for the
# built-in and the .hpr source, 2 Assortment specifications and prices, the Simulator summary
# and result tabs, Price tuning with each matrix view, and Help. The images go
# to outputs/app_smoke/review/polish/ (gitignored). Uses the synthetic tuning
# fixture (120 invented stems); nothing private.
#
# Usage: Rscript scripts/polish_screenshots.R en|no
# ==============================================================================
lang <- commandArgs(TRUE)[1]; stopifnot(lang %in% c("en", "no"))
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir, OPTBUCK_TUNE_MAX_ITER = "3")
if (lang == "en") Sys.setenv(OPTBUCK_LANG = "en") else Sys.unsetenv("OPTBUCK_LANG")
Sys.unsetenv("OPTBUCK_N_PLOTS"); Sys.setenv(NOT_CRAN = "true")
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))
out <- file.path(root_dir, "outputs", "app_smoke", "review", "polish"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
fx <- file.path(root_dir, "inst", "extdata", "inputs", "test_fixtures", "hpr_tuning_synthetic.hpr")
audit_browser_start()
on.exit(audit_browser_stop(), add = TRUE)
app <- audit_app_driver(file.path(root_dir, "inst", "app"), name = paste0("polish_", lang), load_timeout = 60 * 1000,
                        timeout = 180 * 1000, width = 1400, height = 1000)
stopifnot(inherits(app, "AppDriver"))
js <- function(code) tryCatch(app$get_js(code), error = function(e) NA)
shot <- function(name) {
  Sys.sleep(1)
  f <- file.path(out, sprintf("%s_%s.png", lang, name)); app$get_screenshot(f); cat("saved", basename(f), "\n")
}
nav <- function(v) { app$set_inputs(main_nav = v, wait_ = FALSE); app$wait_for_idle(); Sys.sleep(0.5) }
pick <- function(side, key) {
  app$run_js(sprintf(paste0(
    "(function(){ var id = 'tbl_asm_%s', rows = Reactable.getInstance(id).rows;",
    " var i = rows.findIndex(function(r){ return r.original['.key'] === '%s'; });",
    " var g = document.querySelectorAll('#' + id + ' .rt-tbody .rt-tr-group')[i];",
    " if (g) g.querySelector('.rt-td').click(); })()"), side, key))
  app$wait_for_idle()
}
wait_run <- function() {
  app$wait_for_idle(timeout = 300 * 1000)
  app$wait_for_js("!document.querySelector('#run_status .status-running') && !document.getElementById('run_btn').disabled", timeout = 300 * 1000)
  app$wait_for_idle(timeout = 300 * 1000)
}

# 1. Stems, built-in source: the task's selection and the panel
nav("stems")
app$set_inputs(tree_source = "nfi", species_group = "both", hkl_group = "5", fylnr_sel = "11", wait_ = FALSE); app$wait_for_idle()
app$set_inputs(abonitet = c("11", "14", "17"), hoh_range = c(0, 1000), wait_ = FALSE); app$wait_for_idle(); Sys.sleep(1)
shot("01a_stems_builtin_before")
app$click("stems_use_nfi"); app$wait_for_idle()
app$wait_for_js("Array.from(document.querySelectorAll('#fig_stem_dists img')).some(function(e){ return e.offsetParent !== null && e.naturalWidth > 0; })", timeout = 60 * 1000)
shot("01b_stems_builtin_panel")
# 2. Assortment specifications and prices; a difference in B for a comparison
nav("prices")
app$run_js("Shiny.setInputValue('flat_edit', {side: 'B', key: 'spruce_pulpwood', value: '420'}, {priority: 'event'});")
app$wait_for_idle(); shot("02_prices")
# 3. Simulator: empty state is shown with a tree list without data; then the run
nav("simulator")
app$click("run_btn"); wait_run()
shot("03a_simulator_summary")
for (t in c("products", "trees", "logs")) { app$set_inputs(result_tabs = t, wait_ = FALSE); app$wait_for_idle(); shot(paste0("03_simulator_", t)) }
app$set_inputs(tree_source = "list", wait_ = FALSE)
nav("stems"); app$set_inputs(tree_source = "list", wait_ = FALSE); app$wait_for_idle()
nav("simulator"); shot("03z_simulator_empty_state")
nav("tuning"); shot("04z_tuning_empty_state")
# 4. Stems, .hpr source
nav("stems"); app$set_inputs(tree_source = "hpr", wait_ = FALSE); app$wait_for_idle()
app$upload_file(hs_upload = fx)
app$wait_for_js("document.querySelector('#hs_validation') && /120/.test(document.querySelector('#hs_validation').innerText)", timeout = 90 * 1000)
app$wait_for_idle()
app$wait_for_js("Array.from(document.querySelectorAll('#fig_stem_dists img')).some(function(e){ return e.offsetParent !== null && e.naturalWidth > 0; })", timeout = 60 * 1000)
shot("05_stems_hpr")
# 5. The fixture's products into A with an Even target; the tuning tab
nav("prices")
app$upload_file(scen_a_upload = fx); app$wait_for_idle()
app$click("hpr_apply"); app$wait_for_idle()
key <- "spruce_140_sagt_mmer_gran"
pick("a", key)
app$set_inputs(pm_a_view = "target", wait_ = FALSE); app$wait_for_idle()
app$wait_for_js("!!document.querySelector('#dt_a .ht_master td')", timeout = 30 * 1000)
app$click("dt_a_even"); app$wait_for_idle()
shot("06_prices_target_view")
app$set_inputs(pm_a_view = "prices", wait_ = FALSE); app$wait_for_idle()
nav("tuning")
app$wait_for_js("!!document.getElementById('tn_run')", timeout = 30 * 1000)
shot("07a_tuning_list")
app$click("tn_run")
app$wait_for_js("!!document.querySelector('#tn_tbl_hits .rt-tr') && !!document.querySelector('#tn_matrix .rt-tr') && !!document.querySelector('#tn_plot img')", timeout = 300 * 1000)
app$wait_for_idle(); Sys.sleep(2)
shot("07b_tuning_results")
for (v in c("target", "orig", "tuned", "change")) {
  app$set_inputs(tn_matrix_view = v, wait_ = FALSE); app$wait_for_idle(); Sys.sleep(1)
  app$run_js("document.getElementById('tn_matrix').scrollIntoView();")
  shot(paste0("07c_tuning_matrix_", v))
}
# 6. Help
nav("help"); shot("08_help")
cat("done\n")
