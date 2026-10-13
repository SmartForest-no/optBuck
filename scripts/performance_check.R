# ==============================================================================
# scripts/performance_check.R
#
# Purpose : Quick check during the Performance phase: runs every case of
#           scripts/performance_cases.R, compares it with the main baseline
#           (outputs/performance/baseline_main_3fa0a9d.rds) and prints the
#           run times. Audit 22 is the formal check; this is the loop used
#           after each optimisation step.
#
# Usage   : Rscript scripts/performance_check.R [workers]
# ==============================================================================

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
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"))
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))

args <- commandArgs(trailingOnly = TRUE)
extra <- if (length(args) > 0L) list(workers = as.integer(args[1L])) else list()

b <- readRDS(file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d.rds"))
cases <- perf_cases(root_dir)
all_ok <- TRUE
for (nm in names(cases)) {
  t0 <- proc.time()
  r  <- do.call(perf_run_case, c(list(cases[[nm]], root_dir), extra))
  el <- (proc.time() - t0)[["elapsed"]]
  cmp <- perf_compare(r, b[[nm]])
  all_ok <- all_ok && !any(cmp == "DIFFERS")
  not_id <- cmp[cmp != "identical"]
  cat(sprintf("%-24s %6.1f s  %s\n", nm, el,
              if (length(not_id) == 0L) "all tables identical"
              else paste(sprintf("%s: %s", names(not_id), not_id), collapse = "; ")))
}
for (nm in names(PERF_COMPARISONS)) {
  ab <- PERF_COMPARISONS[[nm]]
  t0 <- proc.time()
  r  <- do.call(perf_run_comparison, c(list(cases[[ab[1L]]], cases[[ab[2L]]], root_dir), extra))
  el <- (proc.time() - t0)[["elapsed"]]
  for (side in 1:2) {
    cmp <- perf_compare(if (side == 1L) r else r$scenario, b[[ab[side]]])
    all_ok <- all_ok && !any(cmp == "DIFFERS")
    not_id <- cmp[cmp != "identical"]
    cat(sprintf("%-24s %6s    %s vs %s: %s\n", if (side == 1L) nm else "",
                if (side == 1L) sprintf("%.1f s", el) else "", c("A", "B")[side], ab[side],
                if (length(not_id) == 0L) "all tables identical"
                else paste(sprintf("%s: %s", names(not_id), not_id), collapse = "; ")))
  }
}

cat(if (all_ok) "\nOK: every case matches the main baseline.\n"
    else "\nFAILED: a case differs from the main baseline.\n")
if (!all_ok) quit(status = 1L)
