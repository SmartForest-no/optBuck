# ==============================================================================
# audits/audit_16_sampled_plot_backend_smoke.R
#
# Purpose : Smoke audit for scripts/app_backend/01_run_sampled_plot_bucking.R.
#           Verifies that the sampled-plot backend runs safely on sanitized
#           GitHub data and does not expose original NFI plot identifiers.
#
# Checks  :
#  1.  Backend script exists at scripts/app_backend/01_run_sampled_plot_bucking.R
#  2.  Backend script sources without error
#  3.  Smoke call completes without error
#      params: species_group="spruce", hogstkl=41, abonitet=17, n_plots=5, seed=42
#  4.  Return object has all required top-level names
#  5.  n_plots_sampled == 5 (or REVIEW if fewer eligible plots exist)
#  6.  No returned data.frame contains a column named plot_id
#  7.  Plot- and tree-level data.frames contain plot_id_github
#  8.  No overlapping logs in selected_logs_all
#  9.  NOK per m3 outputs (Phase 3, 2026-10-12): per_m3_summary and
#      per_m3_by_assortment present and non-negative; per scope, assortment
#      contributions add up to the scope's NOK/m3 and volume shares to 1
# 10.  No per-ha, plot-area or plot-level outputs (no plot_summary, no
#      *per_ha* / plot_area names anywhere in the result); label says the
#      results describe the selected trees only
# 11.  Price validation contains no FAIL rows
# 12.  profile_failures == 0 rows (or REVIEW if failures occurred)
# 13.  Runtime is under 120 seconds (REVIEW if above but run completed)
# 14.  The app's .optimize_dp_fast() matches the reference
#      optimize_one_tree_dp() on every tree: dp_best_value within 1e-8 and
#      equal n_selected. Trees come from three shimmed backend runs:
#      spruce and pine (HKL 41/42, abonitet 14/17, 25 plots, seed 42) and
#      "both" (same strata, 10 plots, seed 7). (added 2026-10-12)
# 15.  Same runs: identical cutting pattern (StartPos, StopPos, ProductKey).
#      Strict since Phase 4a (deterministic tie-breaking): any pattern
#      difference is a FAIL.
#
# Verdict :
#   PASS                   -- all 15 checks pass
#   ACCEPTED WITH REVIEW   -- only expected soft items produce REVIEW
#                             (31 dm price gap, zero-price waste, profile
#                              failures in unusual strata, slow runtime)
#   FAIL                   -- backend missing, sourcing error, smoke call
#                             crashes, required structure missing,
#                             plot_id leakage, overlapping logs,
#                             price validation FAIL, or fast/reference
#                             optimizer value mismatch
#
# Outputs :
#   outputs/sampled_plot_backend_smoke/audit_16_sampled_plot_backend_smoke.csv
#   outputs/sampled_plot_backend_smoke/audit_16_sampled_plot_backend_smoke.txt
# ==============================================================================

suppressPackageStartupMessages(library(readr))

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

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "R", "helpers_dp_optimizer.R"))   # for overlap check

out_dir   <- path_output("sampled_plot_backend_smoke", root_dir = root_dir)
ensure_dir(out_dir)

audit_txt <- file.path(out_dir, "audit_16_sampled_plot_backend_smoke.txt")
audit_csv <- file.path(out_dir, "audit_16_sampled_plot_backend_smoke.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)

PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"

cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: sampled-plot backend smoke\n",     file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()),           file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)


# ==============================================================================
# Check 01: backend script exists
# ==============================================================================
cat("\n--- 01. Backend script exists ---\n", file = audit_txt, append = TRUE)
cat("\n--- 01. Backend script exists ---\n")

backend_path <- file.path(root_dir, "scripts", "app_backend",
                           "01_run_sampled_plot_bucking.R")

rec("Check 01: Backend script exists",
    if (file.exists(backend_path)) PASS else FAIL,
    if (file.exists(backend_path))
      paste("found:", backend_path)
    else
      paste("NOT FOUND:", backend_path))


# ==============================================================================
# Check 02: backend sources without error
# ==============================================================================
cat("\n--- 02. Backend sources without error ---\n", file = audit_txt, append = TRUE)
cat("\n--- 02. Backend sources without error ---\n")

source_ok  <- FALSE
source_err <- ""

if (file.exists(backend_path)) {
  result_src <- tryCatch({
    source(backend_path, local = FALSE)
    TRUE
  }, error = function(e) {
    source_err <<- conditionMessage(e)
    FALSE
  })
  source_ok <- isTRUE(result_src)
}

rec("Check 02: Backend sources without error",
    if (source_ok) PASS else FAIL,
    if (source_ok) "source() completed without error"
    else paste("source() error:", source_err))


# ==============================================================================
# Check 03: smoke call completes
# ==============================================================================
cat("\n--- 03. Smoke call completes ---\n", file = audit_txt, append = TRUE)
cat("\n--- 03. Smoke call completes ---\n")

smoke_result <- NULL
smoke_err    <- ""
smoke_time   <- NA_real_

if (source_ok && exists("run_sampled_plot_bucking", mode = "function")) {
  smoke_params <- list(
    species_group = "spruce",
    hogstkl       = 41L,
    abonitet      = 17L,
    n_plots       = 5L,
    seed          = 42L
  )
  t_smoke <- proc.time()
  result_call <- tryCatch({
    r <- run_sampled_plot_bucking(
      params            = smoke_params,
      price_matrix_path = "default_2021",
      root              = root_dir
    )
    list(ok = TRUE, result = r)
  }, error = function(e) {
    list(ok = FALSE, result = NULL, msg = conditionMessage(e))
  })
  smoke_time <- as.numeric((proc.time() - t_smoke)["elapsed"])

  if (isTRUE(result_call$ok)) {
    smoke_result <- result_call$result
    smoke_err    <- ""
  } else {
    smoke_err <- result_call$msg %||% "unknown error"
  }
}

rec("Check 03: Smoke call completes without error",
    if (!is.null(smoke_result)) PASS else FAIL,
    if (!is.null(smoke_result))
      sprintf("completed in %.1f s", smoke_time)
    else
      paste("call error:", smoke_err))


# ==============================================================================
# Check 04: return object has required top-level names
# ==============================================================================
cat("\n--- 04. Return object structure ---\n", file = audit_txt, append = TRUE)
cat("\n--- 04. Return object structure ---\n")

required_names <- c("run_id", "params", "n_eligible_plots", "n_plots_sampled",
                     "n_trees_total", "n_trees_with_logs",
                     "selected_logs_all", "tree_summary", "per_m3_summary",
                     "per_m3_by_assortment", "sampled_plot_ids",
                     "product_summary", "unpriced_summary", "zero_log_trees",
                     "price_validation_checks", "profile_failures",
                     "diagnostics", "label", "elapsed_seconds")

if (!is.null(smoke_result)) {
  present  <- required_names[required_names %in% names(smoke_result)]
  missing  <- setdiff(required_names, names(smoke_result))
  rec("Check 04: Return object has all required names",
      if (length(missing) == 0L) PASS else FAIL,
      if (length(missing) == 0L)
        sprintf("all %d required names present", length(required_names))
      else
        paste("missing:", paste(missing, collapse = ", ")))
} else {
  rec("Check 04: Return object has all required names", FAIL,
      "smoke call did not complete; cannot check structure")
}


# ==============================================================================
# Check 05: n_plots_sampled
# ==============================================================================
cat("\n--- 05. n_plots_sampled ---\n", file = audit_txt, append = TRUE)
cat("\n--- 05. n_plots_sampled ---\n")

if (!is.null(smoke_result)) {
  n_elig    <- smoke_result$n_eligible_plots %||% NA_integer_
  n_sampled <- smoke_result$n_plots_sampled  %||% NA_integer_
  expected  <- min(5L, as.integer(n_elig %||% 5L))

  rec("Check 05: n_plots_sampled",
      if (is.na(n_sampled)) FAIL
      else if (n_sampled == 5L) PASS
      else if (n_sampled == expected && n_elig < 5L) REVIEW
      else FAIL,
      sprintf("n_eligible=%s  n_sampled=%s  expected=%s",
              n_elig, n_sampled, expected))
} else {
  rec("Check 05: n_plots_sampled", FAIL, "smoke call did not complete")
}


# ==============================================================================
# Check 06: no returned data.frame contains column 'plot_id'
# ==============================================================================
cat("\n--- 06. No plot_id leakage ---\n", file = audit_txt, append = TRUE)
cat("\n--- 06. No plot_id leakage ---\n")

if (!is.null(smoke_result)) {
  df_names <- c("selected_logs_all", "tree_summary", "per_m3_summary",
                "per_m3_by_assortment",
                "product_summary", "unpriced_summary", "zero_log_trees",
                "profile_failures")
  leaking <- character(0)
  for (dfn in df_names) {
    df <- smoke_result[[dfn]]
    if (is.data.frame(df) && "plot_id" %in% names(df))
      leaking <- c(leaking, dfn)
  }
  rec("Check 06: No 'plot_id' column in any returned data.frame",
      if (length(leaking) == 0L) PASS else FAIL,
      if (length(leaking) == 0L) "no plot_id leakage detected"
      else paste("LEAKAGE in:", paste(leaking, collapse = ", ")))
} else {
  rec("Check 06: No 'plot_id' column in any returned data.frame", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 07: tree- and plot-level data.frames contain plot_id_github
# ==============================================================================
cat("\n--- 07. plot_id_github present ---\n", file = audit_txt, append = TRUE)
cat("\n--- 07. plot_id_github present ---\n")

if (!is.null(smoke_result)) {
  github_id_dfs <- c("selected_logs_all", "tree_summary",
                     "zero_log_trees", "profile_failures")
  missing_gid <- character(0)
  for (dfn in github_id_dfs) {
    df <- smoke_result[[dfn]]
    if (!is.data.frame(df)) { missing_gid <- c(missing_gid, dfn); next }
    # Only check if the data.frame has rows (empty frames are OK).
    if (nrow(df) > 0L && !"plot_id_github" %in% names(df))
      missing_gid <- c(missing_gid, dfn)
  }
  rec("Check 07: plot_id_github present in tree/plot-level data.frames",
      if (length(missing_gid) == 0L) PASS else FAIL,
      if (length(missing_gid) == 0L)
        sprintf("plot_id_github found in all %d checked data.frames",
                length(github_id_dfs))
      else
        paste("missing plot_id_github in:", paste(missing_gid, collapse = ", ")))
} else {
  rec("Check 07: plot_id_github present in tree/plot-level data.frames", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 08: no overlapping logs in selected_logs_all
# ==============================================================================
cat("\n--- 08. No overlapping logs ---\n", file = audit_txt, append = TRUE)
cat("\n--- 08. No overlapping logs ---\n")

if (!is.null(smoke_result)) {
  sel <- smoke_result$selected_logs_all
  if (is.data.frame(sel) && nrow(sel) > 0L) {
    # check_selected_logs_no_overlap expects treid column; use plot_id_github:treid
    # as the grouping key to handle potential treid collisions across plots.
    sel_check <- sel
    if ("plot_id_github" %in% names(sel_check) && "treid" %in% names(sel_check))
      sel_check$treid <- paste0(sel_check$plot_id_github, ":", sel_check$treid)
    n_overlaps <- check_selected_logs_no_overlap(sel_check)
    rec("Check 08: No overlapping logs in selected_logs_all",
        if (n_overlaps == 0L) PASS else FAIL,
        sprintf("overlapping pairs: %d  (total selected logs: %d)",
                n_overlaps, nrow(sel)))
  } else {
    rec("Check 08: No overlapping logs in selected_logs_all", PASS,
        "selected_logs_all is empty or has one row -- overlap not applicable")
  }
} else {
  rec("Check 08: No overlapping logs in selected_logs_all", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 09: NOK per m3 outputs present and consistent
# ==============================================================================
cat("\n--- 09. NOK per m3 outputs ---\n", file = audit_txt, append = TRUE)
cat("\n--- 09. NOK per m3 outputs ---\n")

if (!is.null(smoke_result)) {
  s  <- smoke_result$per_m3_summary
  ba <- smoke_result$per_m3_by_assortment
  ok_shape <- is.data.frame(s) && is.data.frame(ba) &&
    all(c("scope", "value_nok", "commercial_volume_m3", "nok_per_m3") %in% names(s)) &&
    all(c("scope", "assortment", "nok_per_m3_contribution", "volume_share") %in% names(ba)) &&
    "Total" %in% s$scope
  if (ok_shape) {
    nonneg <- all(c(s$value_nok, s$commercial_volume_m3, ba$value_nok,
                    ba$commercial_volume_m3) >= 0, na.rm = TRUE)
    sum_dev <- max(vapply(unique(ba$scope), function(sc) {
      tot <- s$nok_per_m3[s$scope == sc]
      if (is.na(tot)) return(0)
      abs(sum(ba$nok_per_m3_contribution[ba$scope == sc]) - tot)
    }, numeric(1)))
    share_dev <- max(vapply(unique(ba$scope), function(sc)
      abs(sum(ba$volume_share[ba$scope == sc]) - 1), numeric(1)))
    tot <- s[s$scope == "Total", ]
    rec("Check 09: NOK per m3 outputs present and consistent",
        if (nonneg && sum_dev < 1e-6 && share_dev < 1e-9) PASS else FAIL,
        sprintf("total %.1f NOK/m3 on %.3f m3 u.b. | max |sum(contrib) - total| = %.2e | max |sum(shares) - 1| = %.2e | non-negative: %s",
                tot$nok_per_m3, tot$commercial_volume_m3, sum_dev, share_dev, nonneg))
  } else {
    rec("Check 09: NOK per m3 outputs present and consistent", FAIL,
        "per_m3_summary / per_m3_by_assortment missing or malformed")
  }
} else {
  rec("Check 09: NOK per m3 outputs present and consistent", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 10: no per-ha or plot-level outputs; trees-only label
# ==============================================================================
cat("\n--- 10. No per-ha / plot-level outputs ---\n",
    file = audit_txt, append = TRUE)
cat("\n--- 10. No per-ha / plot-level outputs ---\n")

if (!is.null(smoke_result)) {
  all_names <- c(names(smoke_result), unlist(lapply(smoke_result, function(x)
    if (is.data.frame(x)) names(x) else NULL)), names(smoke_result$diagnostics))
  bad <- grep("per_ha|plot_area|plot_summary|ha_expan", all_names,
              value = TRUE, ignore.case = TRUE)
  lbl <- smoke_result$label %||% ""
  ok_lbl <- grepl("selected trees only", lbl, fixed = TRUE)
  rec("Check 10: No per-ha or plot-level outputs; trees-only label",
      if (length(bad) == 0L && ok_lbl) PASS else FAIL,
      sprintf("per-ha/plot names: %s | label: '%s'",
              if (length(bad) == 0L) "none" else paste(unique(bad), collapse = ", "),
              lbl))
} else {
  rec("Check 10: No per-ha or plot-level outputs; trees-only label", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 11: price validation has no FAIL rows
# ==============================================================================
cat("\n--- 11. Price validation no FAIL rows ---\n",
    file = audit_txt, append = TRUE)
cat("\n--- 11. Price validation no FAIL rows ---\n")

if (!is.null(smoke_result)) {
  pvc <- smoke_result$price_validation_checks
  if (is.data.frame(pvc) && "result" %in% names(pvc)) {
    n_fail   <- sum(pvc$result == "FAIL",   na.rm = TRUE)
    n_review <- sum(pvc$result == "REVIEW", na.rm = TRUE)
    rec("Check 11: Price validation contains no FAIL rows",
        if (n_fail == 0L) PASS else FAIL,
        sprintf("%d FAIL, %d REVIEW in price validation checks", n_fail, n_review))
  } else {
    rec("Check 11: Price validation contains no FAIL rows", FAIL,
        "price_validation_checks is not a data.frame or has no 'result' column")
  }
} else {
  rec("Check 11: Price validation contains no FAIL rows", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Check 12: profile_failures
# ==============================================================================
cat("\n--- 12. Profile failures ---\n", file = audit_txt, append = TRUE)
cat("\n--- 12. Profile failures ---\n")

if (!is.null(smoke_result)) {
  pf <- smoke_result$profile_failures
  n_fail_prof <- if (is.data.frame(pf)) nrow(pf) else NA_integer_
  rec("Check 12: No taper profile failures",
      if (is.na(n_fail_prof)) REVIEW
      else if (n_fail_prof == 0L) PASS
      else REVIEW,
      sprintf("%s profile failure(s) in smoke run",
              if (is.na(n_fail_prof)) "unknown" else n_fail_prof))
} else {
  rec("Check 12: No taper profile failures", FAIL, "smoke call did not complete")
}


# ==============================================================================
# Check 13: runtime under 120 seconds
# ==============================================================================
cat("\n--- 13. Runtime ---\n", file = audit_txt, append = TRUE)
cat("\n--- 13. Runtime ---\n")

if (!is.null(smoke_result) && !is.na(smoke_time)) {
  rec("Check 13: Runtime under 120 s for n_plots=5",
      if (smoke_time < 120) PASS else REVIEW,
      sprintf("runtime: %.1f s (threshold: 120 s)", smoke_time))
} else if (!is.null(smoke_result)) {
  rec("Check 13: Runtime under 120 s for n_plots=5", REVIEW,
      "runtime not measured")
} else {
  rec("Check 13: Runtime under 120 s for n_plots=5", FAIL,
      "smoke call did not complete")
}


# ==============================================================================
# Checks 14-15: fast optimizer == reference optimizer
# ==============================================================================
# The app runs .optimize_dp_fast() (backend), not optimize_one_tree_dp()
# (R/helpers_dp_optimizer.R). Shim the fast optimizer in the global env so
# every tree's real priced dp_in is also solved by the reference, then
# restore it. Runs after check 13 so the smoke runtime is unaffected.
cat("\n--- 14-15. Fast vs reference optimizer ---\n",
    file = audit_txt, append = TRUE)
cat("\n--- 14-15. Fast vs reference optimizer ---\n")

eq_check_14 <- "Check 14: .optimize_dp_fast() value and n_selected match reference"
eq_check_15 <- "Check 15: .optimize_dp_fast() cutting pattern matches reference"

if (source_ok && exists(".optimize_dp_fast", mode = "function")) {
  E_eq <- new.env(parent = emptyenv())
  E_eq$rows <- list()

  .cut_pattern <- function(res) {
    sl <- res$selected_logs
    if (!is.data.frame(sl) || nrow(sl) == 0L) return(character(0))
    sort(paste(sl$StartPos, sl$StopPos, sl$ProductKey, sep = "|"))
  }

  .fast_impl <- .optimize_dp_fast
  .optimize_dp_fast <- function(valued_candidates) {
    f <- .fast_impl(valued_candidates)
    r <- tryCatch(optimize_one_tree_dp(valued_candidates),
                  error = function(e) NULL)
    E_eq$rows[[length(E_eq$rows) + 1L]] <- data.frame(
      ref_ok        = !is.null(r),
      value_diff    = if (is.null(r)) NA_real_
                      else abs(f$dp_best_value - r$dp_best_value),
      n_sel_match   = !is.null(r) && f$n_selected == r$n_selected,
      pattern_match = !is.null(r) && identical(.cut_pattern(f), .cut_pattern(r)),
      stringsAsFactors = FALSE)
    f
  }

  eq_settings <- list(
    list(species_group = "spruce", hogstkl = c(41L, 42L),
         abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
    list(species_group = "pine",   hogstkl = c(41L, 42L),
         abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
    list(species_group = "both",   hogstkl = c(41L, 42L),
         abonitet = c(14L, 17L), n_plots = 10L, seed = 7L)
  )
  eq_errors <- character(0)
  t_eq <- proc.time()
  for (s in eq_settings) {
    err <- tryCatch({
      run_sampled_plot_bucking(params = s, price_matrix_path = "default_2021",
                               root = root_dir)
      ""
    }, error = function(e) conditionMessage(e))
    if (nzchar(err))
      eq_errors <- c(eq_errors, sprintf("%s: %s", s$species_group, err))
  }
  t_eq <- as.numeric((proc.time() - t_eq)["elapsed"])

  # Restore the backend's fast optimizer.
  .optimize_dp_fast <- .fast_impl
  rm(.fast_impl)

  eq <- if (length(E_eq$rows) > 0L) do.call(rbind, E_eq$rows) else
    data.frame(ref_ok = logical(0), value_diff = numeric(0),
               n_sel_match = logical(0), pattern_match = logical(0))
  n_trees    <- nrow(eq)
  value_ok   <- eq$ref_ok & !is.na(eq$value_diff) & eq$value_diff <= 1e-8
  n_val_bad  <- sum(!(value_ok & eq$n_sel_match))
  n_pat_tie  <- sum(value_ok & eq$n_sel_match & !eq$pattern_match)

  rec(eq_check_14,
      if (length(eq_errors) == 0L && n_trees > 0L && n_val_bad == 0L) PASS
      else FAIL,
      sprintf("%d trees compared over %d runs (%.1f s); %d mismatch(es); max |value diff| = %.2e%s",
              n_trees, length(eq_settings), t_eq, n_val_bad,
              if (n_trees > 0L) max(c(0, eq$value_diff), na.rm = TRUE) else NA_real_,
              if (length(eq_errors) > 0L)
                paste(" | run errors:", paste(eq_errors, collapse = "; "))
              else ""))
  n_pat_diff <- sum(!eq$pattern_match)
  rec(eq_check_15,
      if (n_trees == 0L || n_val_bad > 0L || n_pat_diff > 0L) FAIL else PASS,
      if (n_val_bad > 0L)
        sprintf("value/n_selected mismatches in %d tree(s); see check 14", n_val_bad)
      else
        sprintf("%d of %d trees differ in cutting pattern (%d of them at equal value)",
                n_pat_diff, n_trees, n_pat_tie))
} else {
  rec(eq_check_14, FAIL, "backend not sourced; .optimize_dp_fast() unavailable")
  rec(eq_check_15, FAIL, "backend not sourced; .optimize_dp_fast() unavailable")
}


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 16 complete.\n")
cat("Results written to:", out_dir, "\n")
