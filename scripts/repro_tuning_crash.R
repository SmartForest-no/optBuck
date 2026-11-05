# ==============================================================================
# scripts/repro_tuning_crash.R (restructure R1.9, 2026-10-30)
#
# Tries to reproduce the crash the owner saw once: import file5 into Scenario A
# with the import dialog, load file5 as .hpr stems, open the Price tuning tab,
# open the product dropdown, run tuning. The app runs as its own R process
# (log in the scratch folder given as argument 1); a headless Chrome with its
# own profile drives it. Reads file5 in place (C:/apter_private/hpr), writes
# nothing into the repository.
#
# Usage: Rscript scripts/repro_tuning_crash.R <scratch folder> [file]
# ==============================================================================
args <- commandArgs(TRUE)
scratch <- if (length(args) >= 1L) args[1L] else tempdir()
f5 <- if (length(args) >= 2L) args[2L] else "C:/apter_private/hpr/REDACTED28"
root <- normalizePath(getwd(), winslash = "/")
stopifnot(file.exists(file.path(root, "R", "helpers_paths.R")), file.exists(f5))
Sys.setenv(APTER_ROOT = root, NOT_CRAN = "true", APTER_LANG = "en", APTER_TUNE_MAX_ITER = "3")
suppressMessages(pkgload::load_all(root, quiet = TRUE))   # the package (R/, inst/); source(file.path(root, "audits", "helpers_audit.R"))
log <- file.path(scratch, "repro_app.log")
srv <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
  c("-e", sprintf("shiny::runApp('%s', port = 7791, launch.browser = FALSE)", file.path(root, "inst", "app"))),
  stdout = log, stderr = "2>&1", cleanup_tree = TRUE)
on.exit(try(srv$kill_tree(), silent = TRUE), add = TRUE)
for (i in 1:60) { if (file.exists(log) && any(grepl("Listening on", readLines(log, warn = FALSE)))) break; Sys.sleep(1) }
audit_browser_start()
res <- tryCatch({
  options(chromote.timeout = 60)
  app <- shinytest2::AppDriver$new("http://127.0.0.1:7791", name = "repro", load_timeout = 90000, timeout = 600000, width = 1400, height = 1000)
  step <- function(txt) cat(format(Sys.time(), "%H:%M:%S"), txt, "\n")
  step("import file5 into scenario A (upload field + dialog)")
  app$set_inputs(main_nav = "prices", wait_ = FALSE); app$wait_for_idle()
  app$upload_file(scen_a_upload = f5); app$wait_for_idle(timeout = 600000)
  app$wait_for_js("!!document.getElementById('hpr_apply') && !document.getElementById('hpr_apply').disabled", timeout = 600000)
  app$click("hpr_apply"); app$wait_for_idle(timeout = 600000)
  step("load file5 as .hpr stems")
  app$set_inputs(main_nav = "stems", wait_ = FALSE); app$set_inputs(tree_source = "hpr", wait_ = FALSE); app$wait_for_idle()
  app$upload_file(hs_upload = f5)
  app$wait_for_js("document.querySelector('#hs_validation') && /stems read/.test(document.querySelector('#hs_validation').innerText)", timeout = 900000)
  app$wait_for_idle(timeout = 600000)
  step("open the Price tuning tab, the product dropdown")
  app$set_inputs(main_nav = "tuning", wait_ = FALSE); app$wait_for_idle()
  app$wait_for_js("!!document.getElementById('tn_product')", timeout = 120000)
  app$run_js("document.getElementById('tn_product').selectize.open();"); app$wait_for_idle()
  opts <- app$get_js("Array.from(document.querySelectorAll('body > .selectize-dropdown .option')).map(function(o){ return o.innerText.trim(); }).join(' | ')")
  step(paste("options:", substr(opts, 1, 200)))
  app$run_js("document.getElementById('tn_product').selectize.close();")
  step("run tuning (spruce 140 SCA)")
  app$set_inputs(tn_side = "A", tn_product = "spruce_140_sca_sagt_mmer", wait_ = FALSE); app$wait_for_idle()
  app$click("tn_run")
  app$wait_for_js("!!document.querySelector('#tn_tbl .rt-tr')", timeout = 420000)
  step("tuning finished; table shown")
  "finished"
}, error = function(e) paste("ERROR:", conditionMessage(e)), finally = audit_browser_stop())
cat("result:", res, "\nserver alive:", srv$is_alive(), "\n")
cat("---- app log (errors) ----\n"); lg <- readLines(log, warn = FALSE)
print(utils::tail(lg[grepl("Error|error|Warning: Error|crash|Execution halted|terminated", lg)], 20))
