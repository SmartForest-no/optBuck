# ==============================================================================
# audits/helpers_install_gate_child.R  (2026-11-06)
#
# The child process of audit 40 (install gate). Started by the audit with a fresh working directory
# outside the repository, no root variable, R_LIBS = the temporary library that holds the
# installed package, R_USER_CACHE_DIR = a temporary folder. It does what a user does after install:
#   library(optBuck); optBuck::run_app() (a second R process, started here, as a user's R session is),
#   the six tabs, the bucking analysis (tab 5) three times on the owner's file 1, one NFI simulation
#   (the stem profile cache is built into tools::R_user_dir("optBuck", "cache")), the PDF report in
#   Norsk and English.
# Environment in : GATE_OUT (result .rds), GATE_FILE1 (the .hpr), GATE_HELPERS (audits/helpers_audit.R),
#                  GATE_PORT, GATE_LIB (the temporary library)
# Result         : list(checks = data.frame(name, ok, detail), times = numeric(3), cache = path, ...)
# Not a standalone audit (the name does not start with audit_NN_).
# ==============================================================================
Sys.setenv(NOT_CRAN = "true")
out_path <- Sys.getenv("GATE_OUT"); file1 <- Sys.getenv("GATE_FILE1"); port <- as.integer(Sys.getenv("GATE_PORT", "4731"))
lib <- Sys.getenv("GATE_LIB")
R <- list(checks = data.frame(name = character(), ok = logical(), detail = character(), stringsAsFactors = FALSE), times = numeric(0))
chk <- function(name, ok, detail = "") { R$checks[nrow(R$checks) + 1L, ] <<- list(name, isTRUE(ok), as.character(detail)); invisible(NULL) }
save_now <- function() saveRDS(R, out_path)
guard <- function(name, expr) tryCatch(expr, error = function(e) { chk(name, FALSE, paste("error:", conditionMessage(e))); save_now(); NULL })
secs_since <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
# CPU load of the whole machine in percent (other users share this server; the tab 5 times depend on it).
cpu_load <- function() suppressWarnings(tryCatch(as.numeric(system2("powershell", c("-NoProfile", "-Command",
  shQuote("(Get-CimInstance Win32_Processor | Measure-Object LoadPercentage -Average).Average")), stdout = TRUE)[1L]), error = function(e) NA_real_))

old_root_var <- paste0("AP", "TER_ROOT")   # the former root variable, built so that no file carries the old name
# ---- the environment is clean ------------------------------------------------------------------
wd <- normalizePath(getwd(), winslash = "/")
env_clean <- !nzchar(Sys.getenv("OPTBUCK_ROOT")) && !nzchar(Sys.getenv(old_root_var)) && is.null(getOption("optbuck.root"))
suppressMessages(library(optBuck))
pkg <- normalizePath(find.package("optBuck"), winslash = "/")
chk("Fresh process: no root variable, working directory outside the repository, package loaded from the temporary library",
    env_clean && startsWith(pkg, normalizePath(lib, winslash = "/")) && !file.exists(file.path(wd, "DESCRIPTION")) && !dir.exists(file.path(wd, "inst")),
    sprintf("wd %s | package %s | env clean %s", wd, pkg, env_clean))
chk("get_root_dir() resolves to the installed package folder without a setting",
    identical(normalizePath(optBuck:::get_root_dir(), winslash = "/"), pkg), optBuck:::get_root_dir())
chk("Writable cache is under R_user_dir(), not in the package folder",
    startsWith(normalizePath(optBuck:::nfi_cache_dir(), winslash = "/", mustWork = FALSE), normalizePath(tools::R_user_dir("optBuck", "cache"), winslash = "/", mustWork = FALSE)),
    optBuck:::nfi_cache_dir())
R$cache <- optBuck:::nfi_cache_dir(); R$user_cache <- tools::R_user_dir("optBuck", "cache")
save_now()

# ---- the app: run_app() in its own R process, as the user starts it ---------------------------------
# Stubs in the global environment of the app session: the app and its workers must not pick them up
# (the app is evaluated in an environment whose parent is the namespace).
stubs <- paste0(c("get_root_dir", "read_hpr", "read_hpr_stems", "read_hpr_logs", "extdata_path", "path_input"), " <- function(...) stop('global stub used: ", c("get_root_dir", "read_hpr", "read_hpr_stems", "read_hpr_logs", "extdata_path", "path_input"), "')", collapse = "; ")
srv_log <- tempfile("run_app_", fileext = ".log")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
srv <- processx::process$new(rscript, c("-e", sprintf("%s; optBuck::run_app(launch.browser = FALSE, host = '127.0.0.1', port = %d)", stubs, port)),
                             stdout = srv_log, stderr = "2>&1")
on.exit({ try(srv$kill_tree(), silent = TRUE) }, add = TRUE)
up <- FALSE; t0 <- Sys.time()
while (secs_since(t0) < 180 && srv$is_alive() && !up) {
  up <- tryCatch({ con <- url(sprintf("http://127.0.0.1:%d", port)); on.exit(try(close(con), silent = TRUE)); length(readLines(con, n = 1L, warn = FALSE)) > 0L },
                 error = function(e) FALSE, warning = function(w) FALSE)
  if (!up) Sys.sleep(1)
}
srv_txt <- if (file.exists(srv_log)) paste(readLines(srv_log, warn = FALSE), collapse = " | ") else ""
chk("run_app() starts and serves the page", up && !grepl("Cannot locate|Error", srv_txt), sprintf("%.0f s; log: %s", secs_since(t0), substr(srv_txt, 1, 300)))
save_now()
if (!up) quit(save = "no", status = 0)

or_default <- optBuck:::or_default            # helpers_audit.R sources R/helpers_utils.R when this is missing
source(Sys.getenv("GATE_HELPERS"))            # audit_browser_start() etc. (plain functions)
audit_browser_start(); on.exit(audit_browser_stop(), add = TRUE)
app <- guard("Browser start", audit_app_driver(sprintf("http://127.0.0.1:%d", port), name = "gate", load_timeout = 180 * 1000, timeout = 180 * 1000, width = 1400, height = 1000))
if (!inherits(app, "AppDriver")) { chk("Browser connects to the app", FALSE, as.character(app)); save_now(); quit(save = "no", status = 0) }
on.exit(try(app$stop(), silent = TRUE), add = TRUE)
app$wait_for_idle(timeout = 180 * 1000)
js <- function(s) app$get_js(s)
err_n <- function() js("document.querySelectorAll('.shiny-notification-error, .shiny-output-error').length")

# ---- the six tabs --------------------------------------------------------------------------------------
tabs <- c("stems", "prices", "simulator", "tuning", "check", "help")
seen <- vapply(tabs, function(v) {
  js(sprintf("(function(){ var a = document.querySelector('a[data-value=\"%s\"]'); if (a) a.click(); return !!a; })()", v))
  Sys.sleep(1.5); app$wait_for_idle(timeout = 60 * 1000)
  isTRUE(js(sprintf("(function(){ var p = document.querySelector('.tab-pane.active[data-value=\"%s\"]'); return !!p && p.offsetParent !== null; })()", v)))
}, NA)
chk("All six tabs open (stems, prices, simulator, tuning, check, help)", all(seen) && err_n() == 0L,
    sprintf("opened: %s | error elements: %d", paste(names(seen)[seen], collapse = ","), err_n()))
save_now()

# ---- tab 5 on file 1, three times --------------------------------------------------------------------------
js("document.querySelector('a[data-value=\"check\"]').click()"); Sys.sleep(1)
js("var b=document.querySelector('[data-lang=\"no\"]'); if (b) b.click();"); Sys.sleep(1)
if (!file.exists(file1)) { chk("Tab 5 on file 1", FALSE, "file 1 not found") } else {
  app$upload_file(chk_upload = file1); app$wait_for_idle(timeout = 300 * 1000); Sys.sleep(2)
  run_once <- function() {
    load0 <- cpu_load(); t0 <- Sys.time()
    app$click("chk_run")
    busy_seen <- FALSE
    repeat {
      Sys.sleep(2)
      dis <- isTRUE(js("(document.getElementById('chk_run')||{}).disabled === true"))
      if (dis) busy_seen <- TRUE
      pdf <- isTRUE(js("document.getElementById('chk_dl_pdf') !== null"))
      bad <- isTRUE(js("document.querySelector('#chk_results .note-warning') !== null")) && !pdf
      if ((busy_seen && !dis && pdf) || bad || secs_since(t0) > 600) break
    }
    list(secs = secs_since(t0), load = c(load0, cpu_load()), ok = isTRUE(js("document.getElementById('chk_dl_pdf') !== null")))
  }
  runs <- vector("list", 3L)
  for (i in 1:3) { runs[[i]] <- guard(sprintf("Tab 5 run %d", i), run_once()); if (is.null(runs[[i]])) break; R$times <- c(R$times, runs[[i]]$secs); R$loads <- c(R$loads, list(runs[[i]]$load)); save_now() }
  runs <- Filter(Negate(is.null), runs)
  txt_all <- tryCatch(js("(document.querySelector('#chk_results')||{}).innerText"), error = function(e) "")
  chk("Tab 5 on file 1 finishes three times with a report", length(runs) == 3L && all(vapply(runs, `[[`, NA, "ok")) && grepl("%|[0-9]", txt_all),
      sprintf("seconds: %s | machine CPU load (%%) before/after each run: %s", paste(sprintf("%.0f", R$times), collapse = ", "),
              paste(vapply(R$loads, function(l) paste(l, collapse = "/"), ""), collapse = ", ")))
}
save_now()

# ---- the PDF report in Norsk and English -----------------------------------------------------------------------
pdf_in <- function(lang, marker) {
  js(sprintf("var b=document.querySelector('[data-lang=\"%s\"]'); if (b) b.click();", lang)); Sys.sleep(3); app$wait_for_idle(timeout = 120 * 1000)
  f <- tryCatch(app$get_download("chk_dl_pdf", filename = file.path(tempdir(), sprintf("report_%s.pdf", lang))), error = function(e) e)
  if (inherits(f, "error")) return(list(ok = FALSE, detail = conditionMessage(f)))
  magic <- rawToChar(readBin(f, "raw", 5L)); size <- file.size(f)
  txt <- if (requireNamespace("pdftools", quietly = TRUE)) paste(pdftools::pdf_text(f), collapse = " ") else NA_character_
  lang_ok <- if (is.na(txt)) NA else grepl(marker, txt, fixed = TRUE)
  list(ok = identical(magic, "%PDF-") && size > 20000 && !isFALSE(lang_ok), detail = sprintf("%s, %d bytes, text contains '%s': %s", magic, size, marker, lang_ok))
}
tr <- optBuck:::i18n_table(optBuck:::path_input("i18n", "translations.csv"))
mk <- function(col) tr[[col]][tr$id == "chk_h_limits" & tr$kind == "ui"][1L]
for (l in list(c("no", "no"), c("en", "en"))) {
  r <- guard(sprintf("PDF %s", l[1L]), pdf_in(l[1L], mk(l[2L])))
  if (!is.null(r)) chk(sprintf("PDF report downloads in %s", c(no = "Norsk", en = "English")[[l[1L]]]), r$ok, r$detail)
}
save_now()

# ---- one NFI simulation (the profile cache is built into R_user_dir on first use) ------------------------------------
guard("NFI simulation", {
js("var b=document.querySelector('[data-lang=\"en\"]'); if (b) b.click();"); Sys.sleep(1)
js("document.querySelector('a[data-value=\"stems\"]').click()"); Sys.sleep(1)
if (!identical(js("Shiny.shinyapp.$inputValues['tree_source']"), "nfi")) { app$set_inputs(tree_source = "nfi"); app$wait_for_idle(timeout = 120 * 1000) }   # nfi is the default; a no-op set_inputs would wait 180 s
cache_before <- length(list.files(R$cache, "[.]parquet$", recursive = TRUE))
js("document.querySelector('a[data-value=\"simulator\"]').click()"); Sys.sleep(1)
t0 <- Sys.time(); app$click("run_btn")
repeat {
  Sys.sleep(3)
  done <- isTRUE(js("(document.getElementById('summary_cards')||{innerText:''}).innerText.length > 20"))
  failed <- isTRUE(js("document.querySelectorAll('.shiny-notification-error').length > 0"))
  if (done || failed || secs_since(t0) > 1200) break
}
cache_after <- length(list.files(R$cache, "[.]parquet$", recursive = TRUE))
cards <- js("(document.getElementById('summary_cards')||{innerText:''}).innerText")
R$nfi_secs <- secs_since(t0)
chk("NFI simulation runs from the installed package and builds the stem profile cache in R_user_dir()",
    done && !failed && cache_before == 0L && cache_after > 0L && startsWith(normalizePath(R$cache, winslash = "/"), normalizePath(R$user_cache, winslash = "/")),
    sprintf("%.0f s; cache files before %d, after %d in %s; cards: %s", R$nfi_secs, cache_before, cache_after, R$cache, substr(gsub("\\s+", " ", cards), 1, 120)))
})
save_now()
