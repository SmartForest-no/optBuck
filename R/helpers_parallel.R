# ==============================================================================
# R/helpers_parallel.R
#
# Purpose : Per-tree parallelism for the backend runners (Performance phase,
#           2026-10-13). Trees are split into contiguous chunks; each chunk is
#           bucked by .buck_trees() (R/helpers_bucking_core.R) in a future on
#           a PSOCK worker (future::multisession: no forking, so the same on
#           Windows and Linux). Chunk results are combined in chunk order, so
#           results are identical for any number of workers and any chunking.
#
# Options :
#   apter.workers    number of workers; default detectCores() - 1. That
#                    default assumes one R process on the machine: on a
#                    multi-user server with several R processes, set it so
#                    that processes x workers <= cores. The env var
#                    APTER_WORKERS does the same when the option is not set.
#   apter.chunk_size trees per chunk; default max(25, ceiling(n / (4 * workers))),
#                    about four chunks per worker to balance uneven trees.
#
# Workers : a chunk ships the bucking core as one environment (.core_env()):
#           the core helper files sourced into a fresh environment whose
#           parent is package:stats, so nothing is looked up in a worker's
#           global environment (or_default() comes from helpers_utils.R). Workers
#           need no setup and keep no state between chunks.
# ==============================================================================

# Number of parallel workers: option apter.workers, else the env var
# APTER_WORKERS (how scripts/run_audits.R gives each audit process its share
# of the cores), else detectCores() - 1 (at least 1).
apter_workers <- function() {
  w <- getOption("apter.workers")
  if (is.null(w) && nzchar(Sys.getenv("APTER_WORKERS")))
    w <- suppressWarnings(as.integer(Sys.getenv("APTER_WORKERS")))
  if (is.null(w) || is.na(w)) {
    cores <- suppressWarnings(parallel::detectCores())
    w <- if (is.na(cores)) 1L else cores - 1L
  }
  max(1L, as.integer(w))
}

# Trees per chunk for n trees on `workers` workers.
.chunk_size <- function(n, workers) {
  s <- getOption("apter.chunk_size")
  if (!is.null(s)) return(max(1L, as.integer(s)))
  max(25L, as.integer(ceiling(n / (4 * workers))))
}

# Contiguous chunks of 1..n (list of index vectors).
.chunks <- function(n, size) unname(split(seq_len(n), ceiling(seq_len(n) / size)))

# Use a multisession plan with `workers` workers, keeping the current one
# if it already is one. Workers persist between runs.
.ensure_plan <- function(workers) {
  if (!inherits(future::plan(), "multisession") || future::nbrOfWorkers() != workers)
    future::plan(future::multisession, workers = workers)
  invisible(NULL)
}

# The bucking core as one environment, built once per R process.
.APTER_CORE_FILES <- c("helpers_utils", "helpers_paths", "helpers_volume_methods", "helpers_bark",
                       "helpers_commercial_volume", "helpers_products",
                       "helpers_valuation", "helpers_dp_optimizer",
                       "helpers_taper", "helpers_candidate_generation",
                       "helpers_bucking_core")
.apter_core_cache <- new.env(parent = emptyenv())

# TRUE while the package is loaded from its source folder (pkgload::load_all(): audits, scripts, development).
.optbuck_dev <- function()
  requireNamespace("pkgload", quietly = TRUE) && isTRUE(tryCatch(pkgload::is_dev_package("optBuck"), error = function(e) FALSE))

# Installed package: the namespace itself (a worker loads the installed package when it receives it).
# Development (load_all): the core files sourced into one environment, so workers need no installed copy.
.core_env <- function() {
  if (!is.null(.apter_core_cache$env)) return(.apter_core_cache$env)
  if (!.optbuck_dev()) {
    .apter_core_cache$env <- asNamespace("optBuck")
    return(.apter_core_cache$env)
  }
  dir <- file.path(get_root_dir(), "R")
  e <- new.env(parent = as.environment("package:stats"))
  for (f in .APTER_CORE_FILES)
    sys.source(file.path(dir, paste0(f, ".R")), envir = e, keep.source = FALSE)
  .apter_core_cache$env <- e
  e
}

# Chunk tasks: the expression a worker evaluates and, per chunk, its
# globals. Scripts run them as futures (.chunk_futures(), waiting for a free
# worker); the app runs the same tasks as promises (app/helpers_jobs.R).
#
# Bucking: R/helpers_bucking_core.R's .buck_trees() on a chunk of a job's
# trees. Taper: generate_taper_profiles_for_trees() (R/helpers_taper.R) on
# a chunk of tree_data.
.BUCK_CHUNK_EXPR  <- quote(core$.buck_trees(pv_c, td_c, prep, stump, step))
.TAPER_CHUNK_EXPR <- quote(core$generate_taper_profiles_for_trees(td_c, quiet = TRUE))

.buck_chunk_globals <- function(job, workers,
                                chunk_size = .chunk_size(length(job$pvs), workers)) {
  core   <- .core_env()
  td_ids <- as.character(job$tree_data$treid)
  lapply(.chunks(length(job$pvs), chunk_size), function(ix) {
    pv_c <- job$pvs[ix]
    list(core = core, pv_c = pv_c,
         td_c = job$tree_data[td_ids %in% names(pv_c), , drop = FALSE],
         prep = job$prep, stump = job$stump_cm, step = job$step_cm)
  })
}

.taper_chunk_globals <- function(tree_data, workers,
                                 chunk_size = .chunk_size(nrow(tree_data), workers)) {
  core <- .core_env()
  lapply(.chunks(nrow(tree_data), chunk_size), function(ix)
    list(core = core, td_c = tree_data[ix, , drop = FALSE]))
}

# A future for one chunk task.
.chunk_future <- function(expr, globals)
  future::future(expr, substitute = FALSE, globals = globals,
                 packages = character(0), seed = NULL)

# Start one future per chunk of a job's trees; future() waits for a free
# worker when all are busy. Collect with future::value() in chunk order.
.chunk_futures <- function(job, workers, chunk_size = .chunk_size(length(job$pvs), workers)) {
  .ensure_plan(workers)
  lapply(.buck_chunk_globals(job, workers, chunk_size), function(g)
    .chunk_future(.BUCK_CHUNK_EXPR, g))
}

# taperNOR profiles for a tree list in chunks on the workers. Profiles are
# computed per tree, so the chunks bound in order (.combine_taper()) hold
# the same profiles, flags and error messages as one call.
.taper_profiles_parallel <- function(tree_data, workers,
                                     chunk_size = .chunk_size(nrow(tree_data), workers)) {
  if (workers <= 1L || nrow(tree_data) == 0L)
    return(generate_taper_profiles_for_trees(tree_data, quiet = TRUE))
  .ensure_plan(workers)
  fs <- lapply(.taper_chunk_globals(tree_data, workers, chunk_size), function(g)
    .chunk_future(.TAPER_CHUNK_EXPR, g))
  .combine_taper(lapply(fs, future::value))
}

# Bind taperNOR chunk results in order (attributes profile_ok and
# profile_errors as one generate_taper_profiles_for_trees() call).
.combine_taper <- function(parts) {
  rows <- Filter(function(p) nrow(p) > 0L, parts)
  out  <- if (length(rows) > 0L) do.call(rbind, rows) else data.frame()
  attr(out, "profile_ok")     <- unlist(lapply(parts, attr, "profile_ok"))
  attr(out, "profile_errors") <- unlist(lapply(parts, attr, "profile_errors"))
  out
}

# Combine .buck_trees() results of consecutive chunks into one, in chunk
# order (the same lists as one .buck_trees() call over all trees).
.combine_chunks <- function(outs) {
  n_runs <- length(outs[[1L]]$runs)
  runs <- lapply(seq_len(n_runs), function(r) {
    part <- lapply(outs, function(o) o$runs[[r]])
    list(rows     = do.call(c, lapply(part, `[[`, "rows")),
         sel      = do.call(c, lapply(part, `[[`, "sel")),
         unpriced = do.call(c, lapply(part, `[[`, "unpriced")),
         t_price  = sum(vapply(part, `[[`, 0, "t_price")),
         t_dp     = sum(vapply(part, `[[`, 0, "t_dp")))
  })
  list(runs = runs, t_cand = sum(vapply(outs, `[[`, 0, "t_cand")))
}
