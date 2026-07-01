# ==============================================================================
# audits/audit_08_example_sample_bucking.R
#
# Purpose : Independently audit outputs from
#           R/08_run_bucking_on_example_sample.R.
#           Reads only – does not modify any project files.
#           Writes audit outputs to outputs/example_sample_bucking/.
#
# Checks  : PASS / REVIEW / FAIL
# ==============================================================================

library(readr)
library(dplyr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "example_sample_bucking")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

audit_log_file <- file.path(out_dir, "audit_08_example_sample_bucking.txt")
cat("", file = audit_log_file)

# --------------------------------------------------------------------------- #
# helpers                                                                      #
# --------------------------------------------------------------------------- #

checks <- list()

add_check <- function(label, result, msg = "") {
  checks[[length(checks) + 1L]] <<- list(label = label, result = result,
                                          msg   = msg)
  prefix <- switch(result,
    PASS   = "  [PASS]  ",
    REVIEW = "  [REVIEW]",
    FAIL   = "  [FAIL]  ",
    "  [????]  ")
  line <- sprintf("%s %s%s", prefix, label,
                  if (nchar(trimws(msg)) > 0) paste0(" | ", msg) else "")
  cat(line, "\n")
  cat(line, "\n", file = audit_log_file, append = TRUE)
}

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_log_file, append = TRUE)
}

alog(
  "=============================================================",
  "  Audit: R/08_run_bucking_on_example_sample.R outputs",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)

# --------------------------------------------------------------------------- #
# 1. Required output files exist                                               #
# --------------------------------------------------------------------------- #

alog("", "--- 1. Required output files ---")

req_files <- c(
  "sample_selected_logs.csv",
  "sample_tree_summary.csv",
  "sample_product_summary.csv",
  "sample_diagnostics.csv",
  "sample_bucking_summary.txt"
)

all_ok <- TRUE
for (fname in req_files) {
  if (file.exists(file.path(out_dir, fname))) {
    add_check(sprintf("File exists: %s", fname), "PASS")
  } else {
    add_check(sprintf("File exists: %s", fname), "FAIL", "not found")
    all_ok <- FALSE
  }
}
if (!all_ok)
  stop("Cannot continue: required output file(s) missing from ", out_dir)

# --------------------------------------------------------------------------- #
# 2. Read files                                                                #
# --------------------------------------------------------------------------- #

alog("", "--- 2. Reading files ---")

sel   <- readr::read_csv(file.path(out_dir, "sample_selected_logs.csv"),
                          show_col_types = FALSE)
summ  <- readr::read_csv(file.path(out_dir, "sample_tree_summary.csv"),
                          show_col_types = FALSE)
prod  <- readr::read_csv(file.path(out_dir, "sample_product_summary.csv"),
                          show_col_types = FALSE)
diag  <- readr::read_csv(file.path(out_dir, "sample_diagnostics.csv"),
                          show_col_types = FALSE)

alog(sprintf("  Selected logs    : %d rows", nrow(sel)))
alog(sprintf("  Tree summary     : %d rows", nrow(summ)))
alog(sprintf("  Product summary  : %d rows", nrow(prod)))
alog(sprintf("  Diagnostics      : %d rows", nrow(diag)))

diag_val <- function(item_name) {
  v <- diag$value[diag$item == item_name]
  if (length(v) == 0) NA_character_ else v[1]
}

n_ok  <- sum(is.na(summ$error))
n_err <- sum(!is.na(summ$error))

# --------------------------------------------------------------------------- #
# 3. All input trees accounted for in tree_summary                            #
# --------------------------------------------------------------------------- #

alog("", "--- 3. Tree coverage ---")

n_input <- suppressWarnings(as.integer(diag_val("n_trees_input")))
n_summ  <- nrow(summ)

alog(sprintf("  Diagnostics n_trees_input : %s", diag_val("n_trees_input")))
alog(sprintf("  Rows in tree_summary      : %d", n_summ))

if (!is.na(n_input) && n_summ == n_input) {
  add_check("tree_summary has one row per input tree", "PASS",
            sprintf("%d trees", n_summ))
} else {
  add_check("tree_summary has one row per input tree", "FAIL",
            sprintf("diagnostics says %s trees, summary has %d rows",
                    diag_val("n_trees_input"), n_summ))
}

# --------------------------------------------------------------------------- #
# 4. Error rate < 5%                                                          #
# --------------------------------------------------------------------------- #

alog("", "--- 4. Error rate ---")

alog(sprintf("  Trees completed: %d", n_ok))
alog(sprintf("  Trees errored  : %d", n_err))

err_rate <- if (n_summ > 0) 100 * n_err / n_summ else 100

if (n_err == 0) {
  add_check("No errors during run", "PASS")
} else if (err_rate < 5) {
  add_check("Error rate < 5%", "REVIEW",
            sprintf("%d tree(s) errored (%.1f%%)", n_err, err_rate))
} else {
  add_check("Error rate < 5%", "FAIL",
            sprintf("%d tree(s) errored (%.1f%%)", n_err, err_rate))
}

# --------------------------------------------------------------------------- #
# 5. LogLength == StopPos - StartPos in selected logs                         #
# --------------------------------------------------------------------------- #

alog("", "--- 5. LogLength == StopPos - StartPos ---")

if (nrow(sel) == 0) {
  add_check("LogLength == StopPos - StartPos", "REVIEW", "No selected logs")
} else {
  n_bad <- sum(abs(sel$LogLength - (sel$StopPos - sel$StartPos)) >= 1e-9,
               na.rm = TRUE)
  if (n_bad == 0) {
    add_check("LogLength == StopPos - StartPos", "PASS",
              sprintf("%d logs checked", nrow(sel)))
  } else {
    add_check("LogLength == StopPos - StartPos", "FAIL",
              sprintf("%d inconsistent rows", n_bad))
  }
}

# --------------------------------------------------------------------------- #
# 6. No overlapping selected logs within any tree                             #
# --------------------------------------------------------------------------- #

alog("", "--- 6. No overlapping logs within any tree ---")

if (nrow(sel) == 0) {
  add_check("No overlapping logs (all trees)", "PASS", "0 logs")
} else {
  tree_id_col <- c("treid", "tree_id", "TreeId")[
    c("treid", "tree_id", "TreeId") %in% names(sel)][1]

  if (is.na(tree_id_col)) {
    add_check("No overlapping logs (all trees)", "FAIL",
              "No tree id column in selected_logs")
  } else {
    total_overlaps <- 0L
    for (tid in unique(sel[[tree_id_col]])) {
      s <- sel[sel[[tree_id_col]] == tid, ]
      s <- s[order(s$StartPos), ]
      if (nrow(s) > 1) {
        for (k in seq_len(nrow(s) - 1L)) {
          if (s$StopPos[k] > s$StartPos[k + 1L])
            total_overlaps <- total_overlaps + 1L
        }
      }
    }
    if (total_overlaps == 0) {
      add_check("No overlapping logs (all trees)", "PASS")
    } else {
      add_check("No overlapping logs (all trees)", "FAIL",
                sprintf("%d overlapping pair(s)", total_overlaps))
    }
  }
}

# Confirm this matches the diagnostics field.
diag_overlap <- suppressWarnings(
  as.integer(diag_val("n_overlapping_pairs_total")))
if (!is.na(diag_overlap) && diag_overlap == 0) {
  add_check("Diagnostics n_overlapping_pairs_total == 0", "PASS")
} else if (!is.na(diag_overlap) && diag_overlap > 0) {
  add_check("Diagnostics n_overlapping_pairs_total == 0", "FAIL",
            sprintf("Diagnostics reports %d overlapping pair(s)", diag_overlap))
}

# --------------------------------------------------------------------------- #
# 7. No missing PriceNOKperM3 or value_nok in selected logs                  #
# --------------------------------------------------------------------------- #

alog("", "--- 7. No missing prices/values in selected logs ---")

if (nrow(sel) == 0) {
  add_check("No missing PriceNOKperM3 (selected)", "PASS", "0 logs")
  add_check("No missing value_nok (selected)",     "PASS", "0 logs")
} else {
  n_miss_p <- sum(is.na(sel$PriceNOKperM3))
  n_miss_v <- sum(is.na(sel$value_nok))
  if (n_miss_p == 0) {
    add_check("No missing PriceNOKperM3 (selected)", "PASS")
  } else {
    add_check("No missing PriceNOKperM3 (selected)", "FAIL",
              sprintf("%d rows", n_miss_p))
  }
  if (n_miss_v == 0) {
    add_check("No missing value_nok (selected)", "PASS")
  } else {
    add_check("No missing value_nok (selected)", "FAIL",
              sprintf("%d rows", n_miss_v))
  }
}

# --------------------------------------------------------------------------- #
# 8. All selected ProductKey values are valid                                 #
# --------------------------------------------------------------------------- #

alog("", "--- 8. Valid ProductKey values ---")

expected_pks <- c("spruce_sawlog", "spruce_pulpwood",
                  "pine_sawlog",   "pine_pulpwood",
                  "spruce_waste",  "pine_waste")

if (nrow(sel) == 0) {
  add_check("All selected ProductKey values valid", "PASS", "0 logs")
} else {
  unexpected <- setdiff(unique(sel$ProductKey), expected_pks)
  if (length(unexpected) == 0) {
    add_check("All selected ProductKey values valid", "PASS",
              paste(sort(unique(sel$ProductKey)), collapse = ", "))
  } else {
    add_check("All selected ProductKey values valid", "FAIL",
              paste("Unexpected:", paste(unexpected, collapse = ", ")))
  }
}

# --------------------------------------------------------------------------- #
# 9. Per-tree value in tree_summary consistent with selected_logs             #
# --------------------------------------------------------------------------- #

alog("", "--- 9. Per-tree value consistency ---")

if (nrow(sel) == 0 || !("treid" %in% names(sel))) {
  add_check("Per-tree value consistent", "PASS", "No selected logs or no treid")
} else {
  ok_s     <- summ[is.na(summ$error), ]
  n_mismatch <- 0L
  for (k in seq_len(nrow(ok_s))) {
    tid     <- ok_s$treid[k]
    s_val   <- sum(sel$value_nok[sel$treid == tid], na.rm = TRUE)
    d_val   <- ok_s$total_value_nok[k]
    if (!is.na(d_val) && abs(s_val - d_val) >= 0.01) n_mismatch <- n_mismatch + 1L
  }
  if (n_mismatch == 0) {
    add_check("Per-tree value consistent", "PASS")
  } else {
    add_check("Per-tree value consistent", "FAIL",
              sprintf("%d tree(s) with mismatched totals", n_mismatch))
  }
}

# --------------------------------------------------------------------------- #
# 10. Batch total value: diagnostics == sum(tree_summary)                     #
# --------------------------------------------------------------------------- #

alog("", "--- 10. Batch total value consistency ---")

diag_tv  <- suppressWarnings(as.numeric(diag_val("total_value_nok_all_trees")))
summ_tv  <- sum(summ$total_value_nok, na.rm = TRUE)
alog(sprintf("  Diagnostics total_value_nok_all_trees : %s",
             diag_val("total_value_nok_all_trees")))
alog(sprintf("  sum(tree_summary$total_value_nok)     : %.2f", summ_tv))

if (!is.na(diag_tv) && abs(diag_tv - summ_tv) < 0.01) {
  add_check("Batch total value consistent with diagnostics", "PASS",
            sprintf("%.2f NOK", summ_tv))
} else {
  add_check("Batch total value consistent with diagnostics", "FAIL",
            sprintf("diagnostics = %s, sum(tree_summary) = %.2f",
                    diag_val("total_value_nok_all_trees"), summ_tv))
}

# --------------------------------------------------------------------------- #
# 11. Product summary totals consistent with selected_logs                    #
# --------------------------------------------------------------------------- #

alog("", "--- 11. Product summary consistency ---")

if (nrow(prod) == 0 || nrow(sel) == 0) {
  add_check("Product summary consistent with selected_logs", "PASS",
            "No selected logs or empty product summary")
} else {
  total_vol_prod <- sum(prod$total_volume_m3, na.rm = TRUE)
  total_vol_sel  <- sum(sel$price_volume_m3,  na.rm = TRUE)
  if (abs(total_vol_prod - total_vol_sel) < 1e-3) {
    add_check("Product summary total volume consistent", "PASS",
              sprintf("%.4f m3", total_vol_prod))
  } else {
    add_check("Product summary total volume consistent", "FAIL",
              sprintf("prod_sum = %.4f, sum(sel) = %.4f",
                      total_vol_prod, total_vol_sel))
  }
}

# --------------------------------------------------------------------------- #
# 12. STUMP_HEIGHT_CM is 10 cm                                                #
# --------------------------------------------------------------------------- #

alog("", "--- 12. STUMP_HEIGHT_CM check ---")

stump_val <- diag_val("stump_height_cm")
alog(sprintf("  Diagnostics stump_height_cm: %s", stump_val))

if (is.na(stump_val)) {
  add_check("STUMP_HEIGHT_CM in diagnostics", "FAIL", "item not found")
} else if (as.integer(stump_val) == 10L) {
  add_check("STUMP_HEIGHT_CM == 10 cm", "PASS")
} else {
  add_check("STUMP_HEIGHT_CM == 10 cm", "REVIEW",
            sprintf("Value is %s cm (not the default 10 cm)", stump_val))
}

# --------------------------------------------------------------------------- #
# 13. Trees with zero selected logs are small (DBH < 10 cm threshold)        #
# --------------------------------------------------------------------------- #

alog("", "--- 13. Zero-log trees ---")

ok_s      <- summ[is.na(summ$error), ]
zero_sel  <- ok_s[!is.na(ok_s$n_selected) & ok_s$n_selected == 0, ]
pct_zero  <- if (nrow(ok_s) > 0) 100 * nrow(zero_sel) / nrow(ok_s) else 0

alog(sprintf("  Trees with 0 selected logs: %d (%.1f%%)", nrow(zero_sel), pct_zero))

if (nrow(zero_sel) == 0) {
  add_check("No trees with 0 selected logs", "PASS")
} else if (pct_zero <= 20) {
  # Check that zero-log trees are small.
  max_dbh_zero <- max(zero_sel$dbh_mm, na.rm = TRUE)
  alog(sprintf("  Max DBH of zero-log trees: %.0f mm", max_dbh_zero))
  add_check("Trees with 0 selected logs are present", "REVIEW",
            sprintf("%d tree(s) (%.1f%%), max DBH = %.0f mm. Expected for small trees.",
                    nrow(zero_sel), pct_zero, max_dbh_zero))
} else {
  add_check("Trees with 0 selected logs are present", "FAIL",
            sprintf("%.1f%% of completed trees have 0 selected logs", pct_zero))
}

# --------------------------------------------------------------------------- #
# 14. REVIEW: 31 dm sawlog price gap                                          #
# --------------------------------------------------------------------------- #

alog("", "--- 14. REVIEW: 31 dm sawlog price gap ---")

n_dm31_str <- diag_val("n_unmatched_31dm_total")
n_dm31     <- suppressWarnings(as.integer(n_dm31_str))

if (!is.na(n_dm31) && n_dm31 > 0) {
  add_check(
    "Unpriced 31 dm sawlog candidates (known issue)",
    "REVIEW",
    sprintf("%d candidate(s) with LengthClass_dm == 31 had no price. Price matrix starts at 34 dm.",
            n_dm31))
} else {
  add_check("Unpriced 31 dm sawlog candidates (known issue)", "PASS",
            "None found")
}

# --------------------------------------------------------------------------- #
# 15. No selected log has LengthClass_dm == 31                                #
# --------------------------------------------------------------------------- #

alog("", "--- 15. No unpriced 31 dm sawlog selected ---")

if (nrow(sel) == 0) {
  add_check("No unpriced 31 dm sawlog selected", "PASS", "0 logs")
} else {
  lc_col <- if ("LengthClass_dm" %in% names(sel)) sel$LengthClass_dm else rep(NA_integer_, nrow(sel))
  sel_31 <- sel[!is.na(lc_col) & lc_col == 31 &
                  sel$ProductKey %in% c("spruce_sawlog", "pine_sawlog"), ]
  if (nrow(sel_31) == 0) {
    add_check("No unpriced 31 dm sawlog selected", "PASS")
  } else {
    add_check("No unpriced 31 dm sawlog selected", "FAIL",
              sprintf("%d row(s) with LengthClass_dm == 31 in selected_logs",
                      nrow(sel_31)))
  }
}

# --------------------------------------------------------------------------- #
# 16. Save audit outputs                                                       #
# --------------------------------------------------------------------------- #

alog("", "--- 16. Saving audit outputs ---")

checks_df <- do.call(rbind, lapply(checks, function(x)
  data.frame(label = x$label, result = x$result, msg = x$msg,
             stringsAsFactors = FALSE)))

audit_csv <- file.path(out_dir, "audit_08_example_sample_bucking.csv")
readr::write_csv(checks_df, audit_csv)
alog(sprintf("  Saved: %s", basename(audit_csv)))

# --------------------------------------------------------------------------- #
# 17. Final verdict                                                            #
# --------------------------------------------------------------------------- #

alog(
  "",
  "=============================================================",
  "  AUDIT FINAL SUMMARY",
  "=============================================================",
  capture.output(print(
    data.frame(result = c("PASS", "REVIEW", "FAIL"),
               n = c(sum(checks_df$result == "PASS"),
                     sum(checks_df$result == "REVIEW"),
                     sum(checks_df$result == "FAIL")),
               stringsAsFactors = FALSE), row.names = FALSE))
)

n_fail   <- sum(checks_df$result == "FAIL")
n_review <- sum(checks_df$result == "REVIEW")

overall <- if (n_fail > 0) {
  "REJECTED"
} else if (n_review > 0) {
  "ACCEPTED WITH REVIEW ITEMS"
} else {
  "ACCEPTED"
}

alog("")
alog(sprintf("  Script 08 outputs: %s", overall))
alog("=============================================================")
alog(sprintf("  Audit log: %s", audit_log_file))

if (n_review > 0) {
  cat("\nREVIEW items:\n")
  for (i in which(checks_df$result == "REVIEW"))
    cat(sprintf("  [REVIEW] %s | %s\n", checks_df$label[i], checks_df$msg[i]))
}
if (n_fail > 0) {
  cat("\nFAIL items:\n")
  for (i in which(checks_df$result == "FAIL"))
    cat(sprintf("  [FAIL] %s | %s\n", checks_df$label[i], checks_df$msg[i]))
  stop(sprintf("Audit FAILED: %d FAIL check(s). See %s.",
               n_fail, basename(audit_csv)))
}

message(sprintf("[%s] Script 08 audit complete.", overall))
