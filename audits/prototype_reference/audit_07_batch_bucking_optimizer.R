# ==============================================================================
# audits/audit_07_batch_bucking_optimizer.R
#
# Purpose : Independently audit outputs from
#           R/07_test_batch_bucking_optimizer.R.
#           Reads only – does not modify any project files.
#           Writes audit outputs to outputs/batch_optimizer_tests/.
#
# Checks  : PASS / REVIEW / FAIL
# ==============================================================================

library(readr)
library(dplyr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "batch_optimizer_tests")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

audit_log_file <- file.path(out_dir, "audit_07_batch_bucking_optimizer.txt")
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
  "  Audit: R/07_test_batch_bucking_optimizer.R outputs",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)

# --------------------------------------------------------------------------- #
# 1. Required output files exist                                               #
# --------------------------------------------------------------------------- #

alog("", "--- 1. Required output files ---")

req_files <- c(
  "batch_selected_logs.csv",
  "batch_tree_summary.csv",
  "batch_optimizer_summary.txt",
  "batch_diagnostics.csv"
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
  stop("Cannot continue audit: required output file(s) missing from ", out_dir)

# --------------------------------------------------------------------------- #
# 2. Read files                                                                #
# --------------------------------------------------------------------------- #

alog("", "--- 2. Reading files ---")

sel  <- readr::read_csv(file.path(out_dir, "batch_selected_logs.csv"),
                         show_col_types = FALSE)
summ <- readr::read_csv(file.path(out_dir, "batch_tree_summary.csv"),
                         show_col_types = FALSE)
diag <- readr::read_csv(file.path(out_dir, "batch_diagnostics.csv"),
                         show_col_types = FALSE)

alog(sprintf("  Selected logs    : %d rows", nrow(sel)))
alog(sprintf("  Tree summary     : %d rows", nrow(summ)))
alog(sprintf("  Diagnostics      : %d rows", nrow(diag)))

diag_val <- function(item_name) {
  v <- diag$value[diag$item == item_name]
  if (length(v) == 0) NA_character_ else v[1]
}

# --------------------------------------------------------------------------- #
# 3. At least 2 trees completed without error                                 #
# --------------------------------------------------------------------------- #

alog("", "--- 3. Trees completed ---")

n_completed <- sum(is.na(summ$error))
n_error     <- sum(!is.na(summ$error))

alog(sprintf("  Trees completed: %d", n_completed))
alog(sprintf("  Trees errored  : %d", n_error))

if (n_completed >= 2) {
  add_check("At least 2 trees completed", "PASS",
            sprintf("%d completed, %d errored", n_completed, n_error))
} else {
  add_check("At least 2 trees completed", "FAIL",
            sprintf("Only %d tree(s) completed", n_completed))
}

# --------------------------------------------------------------------------- #
# 4. Multiple species present in batch                                        #
# --------------------------------------------------------------------------- #

alog("", "--- 4. Multiple species groups present ---")

n_species <- dplyr::n_distinct(summ$species_group[is.na(summ$error)])
if (n_species >= 2) {
  add_check("Multiple species groups in batch", "PASS",
            paste(sort(unique(summ$species_group[is.na(summ$error)])),
                  collapse = ", "))
} else if (n_species == 1) {
  add_check("Multiple species groups in batch", "REVIEW",
            sprintf("Only 1 species group ('%s'). Input may not contain both spruce and pine.",
                    unique(summ$species_group[is.na(summ$error)])))
} else {
  add_check("Multiple species groups in batch", "FAIL", "0 species groups")
}

# --------------------------------------------------------------------------- #
# 5. LogLength == StopPos - StartPos in selected logs                         #
# --------------------------------------------------------------------------- #

alog("", "--- 5. LogLength == StopPos - StartPos (selected) ---")

if (nrow(sel) == 0) {
  add_check("LogLength == StopPos - StartPos", "REVIEW",
            "No selected logs to check")
} else {
  n_bad <- sum(abs(sel$LogLength - (sel$StopPos - sel$StartPos)) >= 1e-9,
               na.rm = TRUE)
  if (n_bad == 0) {
    add_check("LogLength == StopPos - StartPos", "PASS")
  } else {
    add_check("LogLength == StopPos - StartPos", "FAIL",
              sprintf("%d rows inconsistent", n_bad))
  }
}

# --------------------------------------------------------------------------- #
# 6. No overlapping selected logs within each tree                            #
# --------------------------------------------------------------------------- #

alog("", "--- 6. No overlapping selected logs within any tree ---")

if (nrow(sel) == 0) {
  add_check("No overlapping logs (any tree)", "PASS", "0 logs selected")
} else {
  tree_id_col <- c("treid", "tree_id", "TreeId")[
    c("treid", "tree_id", "TreeId") %in% names(sel)][1]

  if (is.na(tree_id_col)) {
    add_check("No overlapping logs (any tree)", "FAIL",
              "No tree id column found in batch_selected_logs.csv")
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
      add_check("No overlapping logs (any tree)", "PASS")
    } else {
      add_check("No overlapping logs (any tree)", "FAIL",
                sprintf("%d overlapping pair(s) across all trees", total_overlaps))
    }
  }
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
# 9. Per-tree value in tree_summary == sum(value_nok) in selected logs        #
# --------------------------------------------------------------------------- #

alog("", "--- 9. Per-tree value consistency ---")

if (nrow(sel) == 0 || !("treid" %in% names(sel))) {
  add_check("Per-tree value consistent (summary vs selected logs)", "PASS",
            "No selected logs or no treid column")
} else {
  ok_summ <- summ[is.na(summ$error), ]
  val_mismatch <- 0L
  for (k in seq_len(nrow(ok_summ))) {
    tid      <- ok_summ$treid[k]
    s_total  <- sum(sel$value_nok[sel$treid == tid], na.rm = TRUE)
    d_total  <- ok_summ$total_value_nok[k]
    if (!is.na(d_total) && abs(s_total - d_total) >= 0.01) {
      val_mismatch <- val_mismatch + 1L
      alog(sprintf("  MISMATCH treid %s: summary = %.2f, selected_logs = %.2f",
                   tid, d_total, s_total))
    }
  }
  if (val_mismatch == 0) {
    add_check("Per-tree value consistent (summary vs selected logs)", "PASS")
  } else {
    add_check("Per-tree value consistent (summary vs selected logs)", "FAIL",
              sprintf("%d tree(s) with inconsistent totals", val_mismatch))
  }
}

# --------------------------------------------------------------------------- #
# 10. Total value in diagnostics == sum of tree_summary values                #
# --------------------------------------------------------------------------- #

alog("", "--- 10. Batch total value consistency ---")

diag_total_val <- suppressWarnings(
  as.numeric(diag_val("total_value_nok_all_trees")))
summ_total_val <- sum(summ$total_value_nok, na.rm = TRUE)
alog(sprintf("  Diagnostics total_value_nok_all_trees : %s",
             diag_val("total_value_nok_all_trees")))
alog(sprintf("  sum(tree_summary$total_value_nok)     : %.2f", summ_total_val))

if (!is.na(diag_total_val) && abs(diag_total_val - summ_total_val) < 0.01) {
  add_check("Batch total value consistent with diagnostics", "PASS",
            sprintf("%.2f NOK", summ_total_val))
} else {
  add_check("Batch total value consistent with diagnostics", "FAIL",
            sprintf("diagnostics = %s, sum(tree_summary) = %.2f",
                    diag_val("total_value_nok_all_trees"), summ_total_val))
}

# --------------------------------------------------------------------------- #
# 11. STUMP_HEIGHT_CM is 10 cm                                                #
# --------------------------------------------------------------------------- #

alog("", "--- 11. STUMP_HEIGHT_CM check ---")

stump_val <- diag_val("stump_height_cm")
alog(sprintf("  Diagnostics stump_height_cm: %s", stump_val))

if (is.na(stump_val)) {
  add_check("STUMP_HEIGHT_CM in diagnostics", "FAIL",
            "Item 'stump_height_cm' not found in diagnostics CSV")
} else if (as.integer(stump_val) == 10L) {
  add_check("STUMP_HEIGHT_CM == 10 cm", "PASS")
} else {
  add_check("STUMP_HEIGHT_CM == 10 cm", "REVIEW",
            sprintf("Value is %s cm (not the default 10 cm)", stump_val))
}

# --------------------------------------------------------------------------- #
# 12. REVIEW: 31 dm sawlog unpriced candidates                                #
# --------------------------------------------------------------------------- #

alog("", "--- 12. REVIEW: 31 dm sawlog price gap ---")

n_dm31_str <- diag_val("n_unmatched_31dm_total")
n_dm31     <- suppressWarnings(as.integer(n_dm31_str))

if (!is.na(n_dm31) && n_dm31 > 0) {
  add_check(
    "Unpriced 31 dm sawlog candidates (known issue)",
    "REVIEW",
    sprintf("%d total candidate(s) with LengthClass_dm == 31 have no price. Price matrix starts at 34 dm.",
            n_dm31))
} else {
  add_check("Unpriced 31 dm sawlog candidates (known issue)", "PASS",
            "No unpriced 31 dm sawlog candidates")
}

# --------------------------------------------------------------------------- #
# 13. No selected log has LengthClass_dm == 31                                #
# --------------------------------------------------------------------------- #

alog("", "--- 13. No unpriced 31 dm sawlog selected ---")

if (nrow(sel) == 0) {
  add_check("No unpriced 31 dm sawlog selected", "PASS", "0 logs selected")
} else {
  lc_col  <- if ("LengthClass_dm" %in% names(sel)) sel$LengthClass_dm else rep(NA_integer_, nrow(sel))
  sel_31  <- sel[!is.na(lc_col) & lc_col == 31 &
                   sel$ProductKey %in% c("spruce_sawlog", "pine_sawlog"), ]
  if (nrow(sel_31) == 0) {
    add_check("No unpriced 31 dm sawlog selected", "PASS")
  } else {
    add_check("No unpriced 31 dm sawlog selected", "FAIL",
              sprintf("%d such row(s) in batch_selected_logs.csv", nrow(sel_31)))
  }
}

# --------------------------------------------------------------------------- #
# 14. All completed trees have n_generated > 0                                #
# --------------------------------------------------------------------------- #

alog("", "--- 14. All completed trees have generated candidates ---")

ok_summ  <- summ[is.na(summ$error), ]
zero_gen <- sum(!is.na(ok_summ$n_generated) & ok_summ$n_generated == 0)
if (zero_gen == 0) {
  add_check("All completed trees have n_generated > 0", "PASS")
} else {
  add_check("All completed trees have n_generated > 0", "FAIL",
            sprintf("%d tree(s) with n_generated == 0", zero_gen))
}

# --------------------------------------------------------------------------- #
# 15. All completed trees have at least one selected log                      #
# --------------------------------------------------------------------------- #

alog("", "--- 15. At least one log selected per completed tree ---")

if (nrow(ok_summ) == 0) {
  add_check("At least one log selected per completed tree", "REVIEW",
            "No completed trees")
} else {
  n_zero_sel <- sum(!is.na(ok_summ$n_selected) & ok_summ$n_selected == 0)
  if (n_zero_sel == 0) {
    add_check("At least one log selected per completed tree", "PASS",
              sprintf("%d tree(s) all have >= 1 selected log", nrow(ok_summ)))
  } else {
    add_check("At least one log selected per completed tree", "REVIEW",
              sprintf("%d tree(s) selected 0 logs (may be very small trees with no priced candidates)",
                      n_zero_sel))
  }
}

# --------------------------------------------------------------------------- #
# 16. Save audit outputs                                                       #
# --------------------------------------------------------------------------- #

alog("", "--- 16. Saving audit outputs ---")

checks_df <- do.call(rbind, lapply(checks, function(x)
  data.frame(label = x$label, result = x$result, msg = x$msg,
             stringsAsFactors = FALSE)))

audit_csv <- file.path(out_dir, "audit_07_batch_bucking_optimizer.csv")
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
alog(sprintf("  Script 07 outputs: %s", overall))
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

message(sprintf("[%s] Script 07 audit complete.", overall))
