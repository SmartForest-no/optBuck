# ==============================================================================
# scripts/tab5_manual_check.R  (roadmap 8, step 7, 2026-11-04)
#
# Purpose : Drive tab 5 "5 Bucking check" in the real app (shinytest2) on one of the owner's
#           private files (read in place, uploaded for this tab): run the check, time it, read
#           the key figures from the page and take screenshots in Norsk and English into
#           outputs/app_smoke/review/optbuck-merge/ (gitignored; private data shows).
# Usage   : NOT_CRAN=true Rscript scripts/tab5_manual_check.R <file1|file3|file4|file5> [no|en]
# ==============================================================================
Sys.setenv(NOT_CRAN = "true")
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
args <- commandArgs(trailingOnly = TRUE)
which_file <- if (length(args)) args[1L] else "file4"
dirp <- "C:/apter_private/hpr"
path <- switch(which_file,
  file1 = list.files(dirp, "^REDACTED46_REDACTED45.*\\.hpr$", full.names = TRUE)[1L], file3 = file.path(dirp, "REDACTED33"),
  file4 = file.path(dirp, "REDACTED27"), file5 = file.path(dirp, "REDACTED28"), file2 = file.path(dirp, "REDACTED40"))
stopifnot(file.exists(path))
shots <- file.path(root_dir, "outputs", "app_smoke", "review", "optbuck-merge"); dir.create(shots, recursive = TRUE, showWarnings = FALSE)
library(shinytest2)
t_start <- Sys.time()
app <- AppDriver$new(file.path(root_dir, "app"), name = "tab5", seed = 1, timeout = 600000, load_timeout = 120000, width = 1400, height = 1000)
on.exit(app$stop(), add = TRUE)
app$wait_for_idle(); Sys.sleep(2)
app$run_js("document.querySelector('a[data-value=check]').click();"); Sys.sleep(2)
for (lang in if (length(args) > 1L) args[2L] else c("no", "en")) {
  app$run_js(sprintf("var b=document.querySelector('[data-lang=\"%s\"]'); if (b) b.click();", lang)); Sys.sleep(1)
}
app$upload_file(chk_upload = path); app$wait_for_idle(); Sys.sleep(2)
t0 <- Sys.time()
app$click("chk_run")
repeat {
  Sys.sleep(5)
  done <- isTRUE(app$get_js("document.querySelector('.chk-summary') !== null"))
  err <- isTRUE(app$get_js("document.querySelector('#chk_results .note-warning') !== null && document.querySelector('.chk-summary') === null"))
  if (done || err || as.numeric(difftime(Sys.time(), t0, units = "secs")) > 540) break
}
secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("%s: check finished in %.0f s; summary: %s\n", which_file, secs,
            app$get_js("(document.querySelector('.chk-summary')||{}).innerText || (document.querySelector('#chk_results')||{}).innerText")))
Sys.sleep(3)
app$run_js("document.querySelector('.chk-summary') && document.querySelector('.chk-summary').scrollIntoView();")
app$get_screenshot(file.path(shots, sprintf("tab5_%s_%s.png", which_file, if (length(args) > 1L) args[2L] else "no")))
app$run_js("window.scrollTo(0, document.body.scrollHeight / 2);"); Sys.sleep(1)
app$get_screenshot(file.path(shots, sprintf("tab5_%s_%s_b.png", which_file, if (length(args) > 1L) args[2L] else "no")))
cat(sprintf("total %.0f s\n", as.numeric(difftime(Sys.time(), t_start, units = "secs"))))
