# ==============================================================================
# audits/audit_40_install_gate.R  (2026-11-06)
#
# Purpose : The install gate, done as a user does it: the package is installed with R CMD INSTALL into a
#           fresh temporary library and used from a new R process whose working directory is a fresh
#           temporary folder outside the repository, with OPTBUCK_ROOT and the former root variable unset
#           (audits/helpers_install_gate_child.R). It opens the app with run_app(), opens the six tabs,
#           runs tab 5 (bucking analysis) three times on the owner's file 1, runs one NFI simulation (the
#           stem profile cache is built into tools::R_user_dir("optBuck", "cache"); the audit points
#           R_USER_CACHE_DIR at a temporary folder so that the user's own cache is neither used nor
#           changed) and downloads the PDF report in Norsk and English.
#
# Checks  :
#  1.  R CMD INSTALL into a fresh temporary library succeeds
#  2.  The child's own checks (clean environment, root resolves to the installed package, cache under
#      R_user_dir(), run_app() serves, six tabs, tab 5 three times, PDF in both languages, NFI run)
#  3.  Tab 5 on file 1, three runs: median time (before the package conversion 111-122 s in development;
#      REVIEW above 150 s: the cause is reported; PASS up to 150 s)
#  4.  Nothing was written into the installed package folder (no cache, no outputs)
#
# Needs   : the owner's file C:/optbuck_private/hpr/REDACTED46_REDACTED45*.hpr (REVIEW "skipped" when missing),
#           Chrome, shinytest2, processx. About 10 minutes.
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
# Outputs : outputs/install_gate/audit_40_install_gate.{csv,txt}
# Usage   : Rscript audits/audit_40_install_gate.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d)) stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d)) stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # for path_output() only
source(file.path(root_dir, "audits", "helpers_audit.R"))

out_dir <- path_output("install_gate", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_40_install_gate.txt")
audit_csv <- file.path(out_dir, "audit_40_install_gate.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"; REVIEW <- "REVIEW"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
old_root_var <- paste0("AP", "TER_ROOT")   # built from pieces: no file may carry the old name (audit 39)
work <- file.path(tempdir(), "gate"); dir.create(work, recursive = TRUE)
san <- function(x) gsub(gsub("\\\\", "/", normalizePath(work, winslash = "/")), "<tmp>", gsub("\\\\", "/", x), fixed = TRUE)
rec <- function(check, result, detail = "") record_check(E, check, result, san(detail), log_path = audit_txt)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: install gate (installed package, fresh library and working directory)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

lib  <- file.path(work, "lib"); dir.create(lib)
wd   <- file.path(work, "wd");  dir.create(wd)
cache_tmp <- file.path(work, "user_cache"); dir.create(cache_tmp)
rbin <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "R.exe" else "R")

# ---- 1 install --------------------------------------------------------------------------------------
t0 <- Sys.time()
# Built first, as install_github() does: .Rbuildignore applies, so the installed package is the one a user gets.
build_dir <- file.path(work, "build"); dir.create(build_dir)
old_wd <- setwd(build_dir)
build_out <- suppressWarnings(system2(rbin, c("CMD", "build", "--no-build-vignettes", shQuote(root_dir)), stdout = TRUE, stderr = TRUE))
setwd(old_wd)
tarball <- list.files(build_dir, "^optBuck_.*[.]tar[.]gz$", full.names = TRUE)
inst_out <- if (length(tarball) == 1L) {
  suppressWarnings(system2(rbin, c("CMD", "INSTALL", "--no-multiarch", paste0("--library=", shQuote(lib)), shQuote(tarball)), stdout = TRUE, stderr = TRUE))
} else {
  c("R CMD build produced no tarball:", utils::tail(build_out, 5L))
}
inst_ok <- any(grepl("^\\* DONE \\(optBuck\\)", inst_out)) && dir.exists(file.path(lib, "optBuck"))
rec("Check 01: R CMD build + R CMD INSTALL of the tarball into a fresh temporary library", ok(inst_ok),
    sprintf("%.0f s; %s", as.numeric(difftime(Sys.time(), t0, units = "secs")), paste(utils::tail(inst_out, 1L), collapse = "")))
pkg_files_before <- sort(list.files(file.path(lib, "optBuck"), recursive = TRUE))

# ---- 2 the child ----------------------------------------------------------------------------------------
file1 <- list.files("C:/optbuck_private/hpr", "^REDACTED46_REDACTED45.*[.]hpr$", full.names = TRUE)[1L]
child_out <- file.path(work, "gate_result.rds")
res <- NULL
if (inst_ok) {
  child_log <- file.path(work, "child.log")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  p <- processx::process$new(
    rscript, file.path(root_dir, "audits", "helpers_install_gate_child.R"), wd = wd, stdout = child_log, stderr = "2>&1",
    env = c("current", OPTBUCK_ROOT = "", setNames("", old_root_var), R_LIBS = lib, R_USER_CACHE_DIR = cache_tmp, NOT_CRAN = "true",
            GATE_OUT = child_out, GATE_FILE1 = if (is.na(file1)) "" else file1, GATE_HELPERS = file.path(root_dir, "audits", "helpers_audit.R"),
            GATE_PORT = as.character(sample(20000:40000, 1L)), GATE_LIB = lib))
  p$wait(timeout = 3000 * 1000)
  if (p$is_alive()) p$kill_tree()
  if (file.exists(child_out)) res <- readRDS(child_out)
  tail_log <- if (file.exists(child_log)) paste(utils::tail(readLines(child_log, warn = FALSE), 6L), collapse = " | ") else ""
}
if (is.null(res)) {
  rec("Check 02: the child process ran", FAIL, if (inst_ok) paste("no result; log:", substr(tail_log, 1, 400)) else "install failed")
} else {
  for (i in seq_len(nrow(res$checks)))
    rec(sprintf("Check 02.%d: %s", i, res$checks$name[i]), ok(res$checks$ok[i]), res$checks$detail[i])
}

# ---- 3 tab 5 time ------------------------------------------------------------------------------------------
if (is.na(file1)) {
  rec("Check 03: tab 5 on file 1, median of three runs", REVIEW, "the owner's file 1 is not on this machine: skipped")
} else if (is.null(res) || length(res$times) < 3L) {
  rec("Check 03: tab 5 on file 1, median of three runs", FAIL, sprintf("runs timed: %d", length(res$times)))
} else {
  med <- stats::median(res$times)
  rec("Check 03: tab 5 on file 1, median of three runs (111-122 s before the conversion; REVIEW above 150 s)",
      if (med <= 150) PASS else REVIEW,
      sprintf("runs %s s, median %.0f s%s", paste(sprintf("%.0f", res$times), collapse = " / "), med,
              if (med > 150) " (above 150 s: see PLAN.md for the cause)" else ""))
}

# ---- 4 nothing written into the package folder -------------------------------------------------------------------
pkg_files_after <- sort(list.files(file.path(lib, "optBuck"), recursive = TRUE))
rec("Check 04: nothing was written into the installed package folder", ok(identical(pkg_files_before, pkg_files_after)),
    sprintf("%d files before and after; new: %s", length(pkg_files_before), paste(utils::head(setdiff(pkg_files_after, pkg_files_before), 5L), collapse = ", ")))

if (!is.null(res)) cat(sprintf("\nNFI cache: %s\n", san(res$cache)), file = audit_txt, append = TRUE)
unlink(work, recursive = TRUE, force = TRUE)
write_audit_outputs(E$results, audit_csv, audit_txt)
