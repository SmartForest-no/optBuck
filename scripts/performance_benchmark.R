# ==============================================================================
# scripts/performance_benchmark.R
#
# Purpose : Benchmark timings for docs/development.md (Performance phase):
#           synthetic tree lists (scripts/performance_tree_lists.R) of 100,
#           1000 and 5000 trees, on 1, 4 and all-but-one workers, as a single
#           run (default 2021 prices) and as a comparison (plus the changed
#           scenario matrix of scripts/performance_cases.R).
#
#           Times are wall-clock seconds of run_tree_list_bucking(): taperNOR,
#           under bark, products and prices, bucking and result tables. The
#           tree list's validation (prepare_tree_list(), done at upload in the
#           app) is not included. Workers are started and warmed up before
#           timing; starting the pool, once per R process, is reported
#           separately.
#
# Output  : outputs/performance/benchmarks.csv
#
# Usage   : Rscript scripts/performance_benchmark.R [reps]   (default 1)
# ==============================================================================

root_dir <- local({
  # OPTBUCK_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))

args <- commandArgs(trailingOnly = TRUE)
reps <- if (length(args) > 0L) as.integer(args[1L]) else 1L

all_w   <- max(1L, parallel::detectCores() - 1L)
WORKERS <- unique(c(1L, 4L, all_w))
SIZES   <- c(100L, 1000L, 5000L)

scn_path <- tempfile(fileext = ".csv")
pm_export(perf_case_inputs(root_dir)$scenario_pm, scn_path)
scenario <- list(price_matrix_path = scn_path)
lists <- setNames(lapply(SIZES, perf_tree_list), SIZES)

secs <- function(expr) {
  t0 <- Sys.time(); force(expr)
  as.numeric(difftime(Sys.time(), t0, units = "secs"))
}

rows <- list()
for (w in WORKERS) {
  t_start <- if (w > 1L) secs({ future::plan(future::sequential); .ensure_plan(w) }) else 0
  # Warm-up: workers load taperNOR and the core once.
  invisible(run_tree_list_bucking(lists[["100"]], root = root_dir, workers = w))
  for (n in SIZES) for (mode in c("single", "comparison")) for (k in seq_len(reps)) {
    s <- secs(run_tree_list_bucking(lists[[as.character(n)]], root = root_dir, workers = w,
                                    scenario = if (mode == "comparison") scenario))
    rows[[length(rows) + 1L]] <- data.frame(
      n_trees = n, workers = w, mode = mode, rep = k, seconds = round(s, 2),
      pool_start_seconds = round(t_start, 2), stringsAsFactors = FALSE)
    cat(sprintf("%5d trees  %2d workers  %-10s %7.2f s\n", n, w, mode, s))
  }
}
out <- do.call(rbind, rows)
out$cores <- parallel::detectCores()
out$machine <- paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
out$R <- R.version.string
out$date <- format(Sys.Date())
dir.create(file.path(root_dir, "outputs", "performance"), showWarnings = FALSE)
utils::write.csv(out, file.path(root_dir, "outputs", "performance", "benchmarks.csv"),
                 row.names = FALSE)
cat("Saved outputs/performance/benchmarks.csv\n")
