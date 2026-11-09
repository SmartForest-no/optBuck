# ==============================================================================
# R/helpers_audit.R
#
# Purpose : Shared audit utilities for optBuck audit scripts.
#
# Usage   : source("R/helpers_audit.R")
#
# Note    : Existing audit scripts 05-12 are NOT modified. This helper is used
#           only by new audit scripts (audit_14 onwards). Once all audits are
#           migrated, the inline copies in old audits can be removed.
# ==============================================================================


# or_default() (optBuck's "value or default" rule) lives in R/helpers_utils.R,
# shared with the app, the backend and its workers. Audits do not define
# `%||%`: base R's (NULL only) is left alone.
if (!exists("or_default", mode = "function"))
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))


# ------------------------------------------------------------------------------
# Logging
# ------------------------------------------------------------------------------

#' Write lines to the console and to an audit log file simultaneously.
#'
#' @param ...      Character vectors to write; each element is one line.
#' @param log_path Path to the audit text file (writes with append = TRUE).
alog <- function(..., log_path) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat("\n", file = log_path, append = TRUE)  # ensure newline between calls
  cat(lines, sep = "\n", file = log_path, append = TRUE)
}


# The audit files as they were before the run (raw bytes, by log path), so that
# write_audit_outputs() can keep them when only the run timestamp changed.
.AUDIT_PREV <- new.env(parent = emptyenv())
.read_raw <- function(p) if (file.exists(p)) readBin(p, "raw", file.size(p)) else NULL

#' Initialise a fresh audit log file (truncates any existing content). The
#' previous content is remembered for the timestamp rule of
#' write_audit_outputs().
#'
#' @param log_path Path to the audit text file.
init_audit_log <- function(log_path) {
  assign(log_path, list(txt = .read_raw(log_path)), envir = .AUDIT_PREV)
  cat("", file = log_path)
  invisible(log_path)
}

# Timestamp rule (2026-10-28): an audit's .txt/.csv are committed as evidence,
# so a rerun that changes nothing but the "Run: <timestamp>" line must leave
# the files as they were. If the new text equals the previous one apart from
# that line, the previous bytes are written back (the csv too).
.keep_if_only_timestamp <- function(log_path, audit_csv, prev_csv) {
  prev <- get0(log_path, envir = .AUDIT_PREV, inherits = FALSE)$txt
  if (is.null(prev) || !file.exists(log_path)) return(invisible(FALSE))
  lines <- function(raw) {
    l <- strsplit(rawToChar(raw), "\r?\n")[[1L]]
    l[!grepl("^\\s*Run: ", l)]
  }
  if (!identical(lines(prev), lines(.read_raw(log_path)))) return(invisible(FALSE))
  writeBin(prev, log_path)
  if (!is.null(prev_csv)) writeBin(prev_csv, audit_csv)
  invisible(TRUE)
}


# ------------------------------------------------------------------------------
# Check recording
# ------------------------------------------------------------------------------

# The repository's absolute path is machine-specific (and the local folder name is not the package's):
# the committed audit files show it as "<repo>" (2026-11-06).
.audit_repo_root <- function() {
  cand <- c(Sys.getenv("OPTBUCK_ROOT", ""), getOption("optbuck.root", ""))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(d, "audits", "helpers_audit.R"))) { cand <- c(cand, d); break }
    parent <- dirname(d); if (identical(parent, d)) break; d <- parent
  }
  cand <- cand[nzchar(cand)]
  unique(c(cand, normalizePath(cand, winslash = "/", mustWork = FALSE)))
}
# Product names printed from the owner's private files can name buyers and places (2026-11-09): when the
# private word lists exist, their old names are shown by the neutral ones in the committed audit files.
# The lists are read at run time from C:/optbuck_private/example_check/ (the repository does not hold them).
.audit_name_rules <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    d <- "C:/optbuck_private/example_check"
    f1 <- file.path(d, "product_name_map.csv"); f2 <- file.path(d, "name_map.csv")
    rules <- list()
    if (file.exists(f1)) {
      pm <- utils::read.csv(f1, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character(0))
      slug <- function(x) gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(x))))
      ch <- pm[pm$orig_name != pm$new_name, , drop = FALSE]
      pairs <- rbind(data.frame(o = ch$orig_name, n = ch$new_name), data.frame(o = pm$orig_uid, n = pm$new_uid),
                     data.frame(o = slug(ch$orig_name), n = slug(ch$new_name)))
      pairs <- pairs[pairs$o != pairs$n, , drop = FALSE]
      pairs <- pairs[order(-nchar(pairs$o)), , drop = FALSE]
      rules <- c(rules, lapply(seq_len(nrow(pairs)), function(i) list(pairs$o[i], pairs$n[i], TRUE)))
    }
    if (file.exists(f2)) {
      nm <- utils::read.csv(f2, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character(0))
      for (t in unique(nm$token)) {
        if (!nzchar(t)) next
        pat <- paste0("(?<![\\p{L}\\p{N}])", gsub("([.()])", "\\\\\\1", t), "(?![\\p{L}\\p{N}])")
        rules[[length(rules) + 1L]] <- list(pat, "[name]", FALSE)
      }
    }
    cache <<- rules
    rules
  }
})
.redact_names <- function(x) {
  for (r in .audit_name_rules()) {
    x <- if (r[[3L]]) gsub(r[[1L]], r[[2L]], x, fixed = TRUE) else gsub(r[[1L]], r[[2L]], x, perl = TRUE)
  }
  x
}
# The owner's private .hpr files are called file1 ... file5 everywhere in the repository. Their real names (which
# name landowners) are in the private C:/optbuck_private/hpr_files.csv (columns id, file), read at run time (2026-11-09);
# private_word(key) gives other private words (C:/optbuck_private/words.csv, columns key, value), e.g. buyer names that
# a product name of those files contains. NA when the private files are not on the machine.
private_hpr <- function(id, dir = "C:/optbuck_private/hpr", map = "C:/optbuck_private/hpr_files.csv") {
  if (!file.exists(map)) return(rep(NA_character_, length(id)))
  m <- utils::read.csv(map, stringsAsFactors = FALSE, encoding = "UTF-8")
  f <- m$file[match(id, m$id)]
  ifelse(is.na(f), NA_character_, file.path(dir, f))
}
private_word <- function(key, map = "C:/optbuck_private/words.csv") {
  if (!file.exists(map)) return(rep(NA_character_, length(key)))
  m <- utils::read.csv(map, stringsAsFactors = FALSE, encoding = "UTF-8")
  m$value[match(key, m$key)]
}
.redact_root <- function(x) {
  for (r in .audit_repo_root()) x <- gsub(r, "<repo>", x, fixed = TRUE)
  .redact_names(x)
}

#' Append one check result to a results list stored in an environment.
#'
#' Rather than using a global variable (<<-), callers pass an environment
#' that holds the `results` data.frame. This is safer for sourced helpers.
#'
#' @param env    Environment containing a data.frame named `results`.
#' @param check  Character scalar — description of the check.
#' @param result "PASS", "REVIEW", or "FAIL".
#' @param detail Optional detail string.
#' @param log_path If non-NULL, the check line is also written to the log file.
record_check <- function(env, check, result, detail = "", log_path = NULL) {
  detail <- .redact_root(detail)
  check  <- .redact_root(check)
  tag <- sprintf("  [%-6s] %s", result, check)
  if (nchar(detail) > 0L) tag <- paste0(tag, " | ", detail)

  if (!is.null(log_path)) {
    cat(tag, "\n", sep = "", file = log_path, append = TRUE)
  }
  cat(tag, "\n", sep = "")

  new_row <- data.frame(check = check, result = result, detail = detail,
                        stringsAsFactors = FALSE)
  if (is.null(env$results)) {
    env$results <- new_row
  } else {
    env$results <- rbind(env$results, new_row)
  }
  invisible(NULL)
}


# ------------------------------------------------------------------------------
# Diagnostics helper
# ------------------------------------------------------------------------------

#' Look up a scalar value from a key-value diagnostics data.frame.
#'
#' @param diag_df  data.frame with columns `item` and `value`.
#' @param key      Character scalar — the item to look up.
#' @return Character scalar, or NA_character_ if not found.
diag_val <- function(diag_df, key) {
  if (is.null(diag_df) || nrow(diag_df) == 0L) return(NA_character_)
  if (!all(c("item", "value") %in% names(diag_df))) return(NA_character_)
  v <- diag_df$value[diag_df$item == key]
  if (length(v) == 0L) NA_character_ else v[1L]
}


# ------------------------------------------------------------------------------
# Safe min / max
# ------------------------------------------------------------------------------

#' min() with NA removal; returns NA if all values are NA.
safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else min(x)
}

#' max() with NA removal; returns NA if all values are NA.
safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else max(x)
}


# ------------------------------------------------------------------------------
# Verdict and output
# ------------------------------------------------------------------------------

#' Compute the final audit verdict from a results data.frame.
#'
#' Rules:
#'   PASS                 -- all checks PASS
#'   ACCEPTED WITH REVIEW -- only PASS and REVIEW; REVIEW items are known issues
#'   FAIL                 -- at least one FAIL
#'
#' @param results data.frame with a `result` column.
#' @return Character scalar: "PASS", "ACCEPTED WITH REVIEW", or "FAIL".
audit_verdict <- function(results) {
  if (is.null(results) || nrow(results) == 0L) return("PASS")
  n_fail   <- sum(results$result == "FAIL",   na.rm = TRUE)
  n_review <- sum(results$result == "REVIEW", na.rm = TRUE)
  if (n_fail   > 0L) return("FAIL")
  if (n_review > 0L) return("ACCEPTED WITH REVIEW")
  "PASS"
}


#' Write final audit summary to the log file and console, and save the CSV.
#'
#' @param results    data.frame(check, result, detail).
#' @param audit_csv  Path to save the results CSV.
#' @param log_path   Path to the audit text file.
write_audit_outputs <- function(results, audit_csv, log_path) {
  verdict <- audit_verdict(results)

  # Summary table
  tbl <- as.data.frame(table(results$result), stringsAsFactors = FALSE)
  names(tbl) <- c("result", "n")

  header <- paste(rep("=", 61), collapse = "")
  cat(header, "\n", file = log_path, append = TRUE)
  cat("  AUDIT FINAL SUMMARY\n", file = log_path, append = TRUE)
  cat(header, "\n", file = log_path, append = TRUE)
  print(tbl)
  cat(capture.output(print(tbl)), sep = "\n", file = log_path, append = TRUE)
  cat("\n", file = log_path, append = TRUE)

  verdict_line <- sprintf("  Verdict: %s", verdict)
  cat(verdict_line, "\n", file = log_path, append = TRUE)
  cat(header, "\n", file = log_path, append = TRUE)
  cat(verdict_line, "\n")
  cat(header, "\n")

  # Review / fail items
  bad <- results[results$result %in% c("FAIL", "REVIEW"), ]
  if (nrow(bad) > 0L) {
    cat("\n", file = log_path, append = TRUE)
    for (i in seq_len(nrow(bad))) {
      line <- sprintf("  [%s] %s | %s",
                      bad$result[i], bad$check[i], bad$detail[i])
      cat(line, "\n", file = log_path, append = TRUE)
      cat(line, "\n")
    }
  }

  prev_csv <- .read_raw(audit_csv)
  readr::write_csv(results, audit_csv)
  cat(sprintf("\nAudit CSV: %s\n", .redact_root(audit_csv)), file = log_path, append = TRUE)
  .keep_if_only_timestamp(log_path, audit_csv, prev_csv)
  cat(sprintf("\n[%s] Audit complete.\n", verdict))
  invisible(verdict)
}


# ------------------------------------------------------------------------------
# Headless Chrome for the browser audits (17, 29)
# ------------------------------------------------------------------------------
# chromote starts one headless Chrome per R process. Without --user-data-dir it
# uses the default Chrome profile, which the user's own browser, leftover
# headless Chromes and parallel audits share and lock. These helpers give each
# audit process its own throw-away profile and close only the Chrome that this
# process started (the processx handle chromote holds; never a kill by name,
# command line or port). The port is chromote's random one.

.AUDIT_BROWSER <- new.env(parent = emptyenv())

#' Prepare a private Chrome profile for this R process. Call before the first
#' AppDriver$new() (audit_app_driver() does the rest). Registers audit_browser_stop()
#' to run when R exits, as a last resort for a halted script.
audit_browser_start <- function() {
  if (!requireNamespace("chromote", quietly = TRUE))
    stop("chromote is not installed.", call. = FALSE)
  audit_browser_stop()                                   # a start after a start begins clean
  dir <- tempfile("optbuck-chrome-profile-")
  dir.create(dir, recursive = TRUE)
  chromote::set_chrome_args(c(chromote::default_chrome_args(),
                              paste0("--user-data-dir=", normalizePath(dir, winslash = "/")),
                              "--no-first-run", "--no-default-browser-check"))
  .AUDIT_BROWSER$profile <- dir
  if (!isTRUE(.AUDIT_BROWSER$finalizer)) {
    reg.finalizer(.AUDIT_BROWSER, function(e) try(audit_browser_stop(), silent = TRUE), onexit = TRUE)
    .AUDIT_BROWSER$finalizer <- TRUE
  }
  invisible(dir)
}

# Our own Chrome and Chromote (created lazily, so a failed start is an error of
# the caller's AppDriver step), made the default so shinytest2 uses them.
.audit_browser_open <- function() {
  if (is.null(.AUDIT_BROWSER$profile)) audit_browser_start()
  if (!is.null(.AUDIT_BROWSER$chromote) && isTRUE(try(.AUDIT_BROWSER$chromote$is_alive(), silent = TRUE)))
    return(invisible(.AUDIT_BROWSER$chromote))
  br <- chromote::Chrome$new()
  .AUDIT_BROWSER$browser <- br
  cm <- tryCatch(chromote::Chromote$new(browser = br), error = function(e) {
    try(br$get_process()$kill_tree(), silent = TRUE)
    stop(e)
  })
  .AUDIT_BROWSER$chromote <- cm
  chromote::set_default_chromote_object(cm)
  invisible(cm)
}

#' Stop the apps, close this process's Chromote, kill its Chrome tree if it is
#' still alive and delete the profile folder. Safe to call twice.
#'
#' @param apps An AppDriver or a list of them (stopped first).
audit_browser_stop <- function(apps = list()) {
  if (inherits(apps, "AppDriver")) apps <- list(apps)
  for (a in apps) try(a$stop(), silent = TRUE)
  cm <- .AUDIT_BROWSER$chromote
  br <- .AUDIT_BROWSER$browser
  if (!is.null(cm)) try(cm$close(wait = 5), silent = TRUE)
  p <- if (!is.null(br)) try(br$get_process(), silent = TRUE) else NULL
  if (inherits(p, "process") && isTRUE(try(p$is_alive(), silent = TRUE)))
    try(p$kill_tree(), silent = TRUE)
  if (!is.null(.AUDIT_BROWSER$profile))
    try(unlink(.AUDIT_BROWSER$profile, recursive = TRUE, force = TRUE), silent = TRUE)
  .AUDIT_BROWSER$chromote <- NULL
  .AUDIT_BROWSER$browser  <- NULL
  .AUDIT_BROWSER$profile  <- NULL
  invisible(NULL)
}

#' shinytest2::AppDriver$new() on this process's own Chrome, with one retry: on a
#' failed start the Chrome and the profile are replaced and the start is tried
#' again after 5 s. Returns the AppDriver or the error text (the callers'
#' existing convention).
#'
#' @param ... Arguments of AppDriver$new().
audit_app_driver <- function(...) {
  once <- function() tryCatch({
    .audit_browser_open()
    shinytest2::AppDriver$new(...)
  }, error = function(e) e)
  app <- once()
  if (inherits(app, "error")) {
    first <- conditionMessage(app)
    audit_browser_stop()
    audit_browser_start()
    Sys.sleep(5)
    app <- once()
    if (inherits(app, "error"))
      return(paste0(conditionMessage(app), " (first attempt: ", first, ")"))
  }
  app
}


# ------------------------------------------------------------------------------
# Optimizer audits: priced DP inputs per tree
# ------------------------------------------------------------------------------

#' Every tree's priced DP input, built with the per-tree data.frame pipeline
#' (generate_candidate_logs_one_tree() -> eligible with a price volume ->
#' prepare_price_join_keys() -> lookup_prices() -> value_nok), with the
#' backend's own run of the same trees (in this process, workers = 1).
#' Audits 16 and 19 solve these inputs with .optimize_dp_fast() and the
#' reference optimize_one_tree_dp() and compare with the backend's result.
#' Needs the backend (scripts/app_backend/01_run_sampled_plot_bucking.R).
#'
#' @param params Sampled-run params (run_sampled_plot_bucking()).
#' @param root   Project root.
#' @return list(dp_in = named list (treid) of priced candidate data.frames,
#'   trees without priced candidates left out; result = the backend result).
audit_dp_inputs <- function(params, root, price_matrix_path = "default_2021") {
  job <- prepare_sampled_job(params, price_matrix_path, root)
  res <- finish_job(job, buck_job(job, workers = 1L))
  pp  <- job$pps[[1L]]
  dp <- lapply(job$pvs, function(pv) {
    c0 <- generate_candidate_logs_one_tree(pv, pp$pd, pp$lc, job$stump_cm, job$step_cm)
    e  <- c0[c0$eligible & !is.na(c0$price_volume_m3), , drop = FALSE]
    if (nrow(e) == 0L) return(NULL)
    e <- prepare_price_join_keys(e, pp$lookup)
    e$PriceNOKperM3 <- lookup_prices(e, pp$lookup)
    e$value_nok     <- e$price_volume_m3 * e$PriceNOKperM3
    e <- e[!is.na(e$PriceNOKperM3) & !is.na(e$value_nok), , drop = FALSE]
    if (nrow(e) == 0L) NULL else e
  })
  list(dp_in = dp[!vapply(dp, is.null, logical(1))], result = res)
}

# Replace a function inside the optBuck namespace (audits that simulate a failure): since the package
# conversion the functions live in the namespace, not in the global environment.
set_internal <- function(name, value) {
  ns <- asNamespace("optBuck")
  if (bindingIsLocked(name, ns)) { unlockBinding(name, ns) }
  assign(name, value, envir = ns)
  invisible(NULL)
}
