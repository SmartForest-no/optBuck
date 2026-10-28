# app/helpers_jobs.R
#
# Purpose : Runs as background jobs (Performance phase, 2026-10-13), for
#           shiny::ExtendedTask: the app stays responsive and shows progress
#           while a run is on the workers (R/helpers_parallel.R).
#
#           A job is the backend's prepare -> buck -> finish
#           (scripts/app_backend/01_run_sampled_plot_bucking.R), as a chain
#           of promises:
#             prepare  NFI: prepare_sampled_job() on a worker (which sources
#                      the backend once); tree list: taperNOR in chunks on
#                      the workers, then prepare_tree_list_job() here; .hpr
#                      stems: prepare_hpr_stems_job() on a worker
#             buck     one promise per chunk of trees (the backend's chunk
#                      tasks), reporting progress as chunks finish
#             finish   finish_job() here (result tables)
#           Same chunks and combination as the backend's buck_job(), so the
#           results equal a script run with any number of workers.

#' A promise of a future, started when a worker is free (first come, first
#' served) and polled with later, so this process never waits.
#' (promises::future_promise() 1.3.2 calls the plan function directly, which
#' future >= 1.40 does not allow.)
.APP_FUTURE_QUEUE <- new.env(parent = emptyenv())
.APP_FUTURE_QUEUE$jobs <- list()

.async_future <- function(expr, globals, poll = 0.05) {
  promises::promise(function(resolve, reject) {
    q <- .APP_FUTURE_QUEUE
    q$jobs[[length(q$jobs) + 1L]] <- list(expr = expr, globals = globals,
                                          resolve = resolve, reject = reject)
    .async_pump(poll)
  })
}

# Start queued futures while workers are free; each started one is polled.
.async_pump <- function(poll) {
  q <- .APP_FUTURE_QUEUE
  while (length(q$jobs) > 0L && future::nbrOfFreeWorkers() > 0L) {
    j <- q$jobs[[1L]]; q$jobs <- q$jobs[-1L]
    f <- tryCatch(.chunk_future(j$expr, j$globals), error = function(e) e)
    if (inherits(f, "error")) { j$reject(f); next }
    local({
      f <- f; j <- j
      check <- function() {
        if (!future::resolved(f)) return(later::later(check, poll))
        v <- tryCatch(future::value(f), error = function(e) e)
        if (inherits(v, "error")) j$reject(v) else j$resolve(v)
        .async_pump(poll)
      }
      later::later(check, poll)
    })
  }
  if (length(q$jobs) > 0L && is.null(q$waiting)) {
    # All workers busy (e.g. another session's run): look again shortly.
    q$waiting <- TRUE
    later::later(function() { q$waiting <- NULL; .async_pump(poll) }, poll)
  }
  invisible(NULL)
}

#' Run one prepare function of the backend on a worker. The worker sources
#' the backend the first time; options that steer the backend (bark model)
#' are passed on.
.backend_promise <- function(fun_name, args) {
  .async_future(
    quote({
      if (!exists(fun_name, mode = "function")) {
        Sys.setenv(APTER_ROOT = root)
        suppressPackageStartupMessages(
          source(file.path(root, "scripts", "app_backend", "01_run_sampled_plot_bucking.R")))
      }
      options(opts)
      do.call(fun_name, args)
    }),
    globals = list(fun_name = fun_name, args = args, root = ROOT,
                   opts = list(apter.bark_model = getOption("apter.bark_model"))))
}

#' Read the stems of an .hpr file on a worker (read_hpr_stems(),
#' R/helpers_hpr_stems.R): large files take 10-20 s.
read_hpr_stems_async <- function(path, filename) {
  .ensure_plan(apter_workers())
  .backend_promise("read_hpr_stems", list(path, filename))
}

#' Read the stems of several .hpr files on a worker, one after the other
#' (read_hpr_stem_files(), R/helpers_hpr_stems.R).
read_hpr_stem_files_async <- function(paths, filenames) {
  .ensure_plan(apter_workers())
  .backend_promise("read_hpr_stem_files", list(paths, filenames))
}

#' Promises for chunk tasks (R/helpers_parallel.R), resolved in chunk order;
#' on_done(i) is called as chunk i finishes.
.chunk_promises <- function(expr, globals_list, on_done) {
  ps <- lapply(seq_along(globals_list), function(i)
    promises::then(.async_future(expr, globals_list[[i]]),
                   function(v) { on_done(i); v }))
  promises::promise_all(.list = ps)
}

#' Run a job asynchronously.
#'
#' @param spec list(src = "nfi" or "list", params (NFI), tl (prepared tree
#'   list), list_params (tree list), stems and species_map (.hpr stems:
#'   read_hpr_stems() result and the confirmed map), path, pd, lc (the run's
#'   prices and products; NULL pd/lc = defaults), scenario = NULL or list(
#'   price_matrix_path, products, length_classes)).
#' @param progress function(done, total, what) for the progress display.
#' @return A promise of the backend result (with $scenario for a comparison).
run_job_async <- function(spec, progress = function(done, total, what) NULL) {
  workers <- apter_workers()
  .ensure_plan(workers)

  p_job <- if (identical(spec$src, "list")) {
    td <- tree_list_tree_data(spec$tl)
    gl <- .taper_chunk_globals(td, workers)
    n_done <- 0L
    progress(0L, nrow(td), "Taper profiles")
    promises::then(
      .chunk_promises(.TAPER_CHUNK_EXPR, gl, function(i) {
        n_done <<- n_done + nrow(gl[[i]]$td_c)
        progress(n_done, nrow(td), "Taper profiles")
      }),
      function(parts)
        prepare_tree_list_job(spec$tl, spec$path, ROOT, spec$pd, spec$lc,
                              params = spec$list_params, scenario = spec$scenario,
                              profiles = .combine_taper(parts)))
  } else if (identical(spec$src, "hpr")) {
    progress(0L, NA_integer_, "Preparing stem profiles")
    .backend_promise("prepare_hpr_stems_job",
                     list(spec$stems, spec$species_map, spec$path, ROOT, spec$pd, spec$lc,
                          spec$list_params, spec$scenario, spec$grade_opts))
  } else {
    progress(0L, NA_integer_, "Loading NFI data and taper profiles")
    .backend_promise("prepare_sampled_job",
                     list(spec$params, spec$path, ROOT, spec$pd, spec$lc, spec$scenario))
  }

  promises::then(p_job, function(job) {
    gl <- .buck_chunk_globals(job, workers)
    n  <- length(job$pvs); n_done <- 0L
    progress(0L, n, "Bucking trees")
    promises::then(
      .chunk_promises(.BUCK_CHUNK_EXPR, gl, function(i) {
        n_done <<- n_done + length(gl[[i]]$pv_c)
        progress(n_done, n, "Bucking trees")
      }),
      function(outs) {
        progress(n, n, "Summarising results")
        b <- .combine_chunks(outs)
        b$workers <- workers
        finish_job(job, b)
      })
  })
}
