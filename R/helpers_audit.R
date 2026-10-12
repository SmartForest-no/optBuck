# ==============================================================================
# R/helpers_audit.R
#
# Purpose : Shared audit utilities for Apter audit scripts.
#
# Usage   : source("R/helpers_audit.R")
#
# Note    : Existing audit scripts 05-12 are NOT modified. This helper is used
#           only by new audit scripts (audit_14 onwards). Once all audits are
#           migrated, the inline copies in old audits can be removed.
# ==============================================================================


# ------------------------------------------------------------------------------
# Null-coalescing operator
# ------------------------------------------------------------------------------

#' Return b if a is NULL or a scalar NA, otherwise return a.
#'
#' @param a Left-hand side.
#' @param b Right-hand side (fallback).
#' @return a if non-null/non-NA, else b.
`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}


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


#' Initialise a fresh audit log file (truncates any existing content).
#'
#' @param log_path Path to the audit text file.
init_audit_log <- function(log_path) {
  cat("", file = log_path)
  invisible(log_path)
}


# ------------------------------------------------------------------------------
# Check recording
# ------------------------------------------------------------------------------

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

  readr::write_csv(results, audit_csv)
  cat(sprintf("\nAudit CSV: %s\n", audit_csv), file = log_path, append = TRUE)
  cat(sprintf("\n[%s] Audit complete.\n", verdict))
  invisible(verdict)
}
