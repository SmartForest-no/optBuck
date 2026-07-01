# ==============================================================================
# audits/audit_06_one_tree_bucking_optimizer.R
#
# Purpose : Independently audit outputs from
#           R/06_test_one_tree_bucking_optimizer.R.
#           Reads only – does not modify any project files.
#           Writes audit outputs to outputs/optimizer_tests/.
#
# Checks  : PASS / REVIEW / FAIL
# ==============================================================================

library(readr)
library(dplyr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
opt_dir  <- file.path(root_dir, "outputs", "optimizer_tests")

dir.create(opt_dir, showWarnings = FALSE, recursive = TRUE)

audit_log_file <- file.path(opt_dir, "audit_06_one_tree_bucking_optimizer.txt")
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
  "  Audit: R/06_test_one_tree_bucking_optimizer.R outputs",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)

# --------------------------------------------------------------------------- #
# 1. Required output files exist                                               #
# --------------------------------------------------------------------------- #

alog("", "--- 1. Required output files ---")

req_files <- c(
  "one_tree_optimizer_generated_candidates.csv",
  "one_tree_optimizer_priced_candidates.csv",
  "one_tree_optimizer_selected_logs.csv",
  "one_tree_optimizer_all_states.csv",
  "one_tree_optimizer_summary.txt",
  "one_tree_optimizer_diagnostics.csv"
)

all_files_ok <- TRUE
for (fname in req_files) {
  fpath <- file.path(opt_dir, fname)
  if (file.exists(fpath)) {
    add_check(sprintf("File exists: %s", fname), "PASS")
  } else {
    add_check(sprintf("File exists: %s", fname), "FAIL", "not found")
    all_files_ok <- FALSE
  }
}

if (!all_files_ok)
  stop("Cannot continue audit: required output file(s) missing from ", opt_dir)

# --------------------------------------------------------------------------- #
# 2. Read outputs                                                              #
# --------------------------------------------------------------------------- #

alog("", "--- 2. Reading files ---")

gen    <- readr::read_csv(file.path(opt_dir, "one_tree_optimizer_generated_candidates.csv"),
                           show_col_types = FALSE)
priced <- readr::read_csv(file.path(opt_dir, "one_tree_optimizer_priced_candidates.csv"),
                           show_col_types = FALSE)
sel    <- readr::read_csv(file.path(opt_dir, "one_tree_optimizer_selected_logs.csv"),
                           show_col_types = FALSE)
diag   <- readr::read_csv(file.path(opt_dir, "one_tree_optimizer_diagnostics.csv"),
                           show_col_types = FALSE)

alog(sprintf("  Generated candidates : %d rows", nrow(gen)))
alog(sprintf("  Priced candidates    : %d rows", nrow(priced)))
alog(sprintf("  Selected logs        : %d rows", nrow(sel)))
alog(sprintf("  Diagnostics          : %d rows", nrow(diag)))

# Reconstruct DP input from priced_candidates (same filter as script 06).
dp_input <- priced[
  !is.na(priced$PriceNOKperM3) &
    !is.na(priced$value_nok) &
    !is.na(priced$price_volume_m3), ]
dp_input$StartPos <- as.integer(round(dp_input$StartPos))
dp_input$StopPos  <- as.integer(round(dp_input$StopPos))

alog(sprintf("  DP input (reconstructed) : %d rows", nrow(dp_input)))

diag_val <- function(item_name) {
  v <- diag$value[diag$item == item_name]
  if (length(v) == 0) NA_character_ else v[1]
}

# --------------------------------------------------------------------------- #
# 3. Generated candidates have more than one unique StartPos                  #
# --------------------------------------------------------------------------- #

alog("", "--- 3. Multiple unique StartPos in generated candidates ---")

n_unique_start_gen <- dplyr::n_distinct(gen$StartPos)
alog(sprintf("  Unique StartPos in generated candidates: %d", n_unique_start_gen))

if (n_unique_start_gen > 1) {
  add_check("Generated candidates have > 1 unique StartPos", "PASS",
            sprintf("%d unique StartPos values", n_unique_start_gen))
} else {
  add_check("Generated candidates have > 1 unique StartPos", "FAIL",
            sprintf("Only %d unique StartPos found", n_unique_start_gen))
}

# --------------------------------------------------------------------------- #
# 4. Priced DP input has more than one unique StartPos                        #
# --------------------------------------------------------------------------- #

alog("", "--- 4. Multiple unique StartPos in DP input ---")

n_unique_start_dp <- dplyr::n_distinct(dp_input$StartPos)
alog(sprintf("  Unique StartPos in DP input: %d", n_unique_start_dp))

if (n_unique_start_dp > 1) {
  add_check("DP input has > 1 unique StartPos", "PASS",
            sprintf("%d unique StartPos values", n_unique_start_dp))
} else {
  add_check("DP input has > 1 unique StartPos", "REVIEW",
            sprintf("Only %d unique StartPos (tree may be too short or price matrix too restrictive)",
                    n_unique_start_dp))
}

# --------------------------------------------------------------------------- #
# 5. LogLength == StopPos - StartPos (generated candidates)                   #
# --------------------------------------------------------------------------- #

alog("", "--- 5. LogLength == StopPos - StartPos (generated) ---")

ll_ok <- all(abs(gen$LogLength - (gen$StopPos - gen$StartPos)) < 1e-9,
             na.rm = TRUE)
n_bad <- sum(abs(gen$LogLength - (gen$StopPos - gen$StartPos)) >= 1e-9,
             na.rm = TRUE)
if (ll_ok) {
  add_check("LogLength == StopPos - StartPos (generated)", "PASS")
} else {
  add_check("LogLength == StopPos - StartPos (generated)", "FAIL",
            sprintf("%d rows inconsistent", n_bad))
}

# Also check selected logs.
if (nrow(sel) > 0) {
  ll_ok_sel <- all(abs(sel$LogLength - (sel$StopPos - sel$StartPos)) < 1e-9,
                   na.rm = TRUE)
  if (ll_ok_sel) {
    add_check("LogLength == StopPos - StartPos (selected)", "PASS")
  } else {
    n_bad_sel <- sum(abs(sel$LogLength - (sel$StopPos - sel$StartPos)) >= 1e-9,
                     na.rm = TRUE)
    add_check("LogLength == StopPos - StartPos (selected)", "FAIL",
              sprintf("%d rows inconsistent", n_bad_sel))
  }
}

# --------------------------------------------------------------------------- #
# 6. Selected logs do not overlap                                              #
# --------------------------------------------------------------------------- #

alog("", "--- 6. No overlap between selected logs ---")

if (nrow(sel) <= 1) {
  add_check("No overlapping selected logs", "PASS",
            sprintf("%d log(s) selected", nrow(sel)))
} else {
  sel_ord <- sel[order(sel$StartPos), ]
  n_overlap <- 0L
  for (k in seq_len(nrow(sel_ord) - 1L)) {
    if (sel_ord$StopPos[k] > sel_ord$StartPos[k + 1L]) n_overlap <- n_overlap + 1L
  }
  if (n_overlap == 0) {
    add_check("No overlapping selected logs", "PASS")
  } else {
    add_check("No overlapping selected logs", "FAIL",
              sprintf("%d overlapping consecutive pair(s)", n_overlap))
  }
}

# --------------------------------------------------------------------------- #
# 7. No missing PriceNOKperM3 or value_nok in selected logs                  #
# --------------------------------------------------------------------------- #

alog("", "--- 7. No missing prices/values in selected logs ---")

if (nrow(sel) == 0) {
  add_check("No missing PriceNOKperM3 (selected)", "PASS", "0 logs selected")
  add_check("No missing value_nok (selected)",     "PASS", "0 logs selected")
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
# 8. Selected ProductKey values are among the 6 valid keys                   #
# --------------------------------------------------------------------------- #

alog("", "--- 8. Valid ProductKey values ---")

expected_pks <- c("spruce_sawlog", "spruce_pulpwood",
                  "pine_sawlog",   "pine_pulpwood",
                  "spruce_waste",  "pine_waste")

if (nrow(sel) == 0) {
  add_check("All selected ProductKey values valid", "PASS", "0 logs selected")
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
# 9. Total value in diagnostics == sum(value_nok) in selected logs            #
# --------------------------------------------------------------------------- #

alog("", "--- 9. Total value consistency ---")

diag_total_val <- suppressWarnings(as.numeric(diag_val("total_value_nok")))
computed_val   <- sum(sel$value_nok, na.rm = TRUE)
alog(sprintf("  Diagnostics total_value_nok : %s", diag_val("total_value_nok")))
alog(sprintf("  sum(sel$value_nok)          : %.4f", computed_val))

if (!is.na(diag_total_val) && abs(diag_total_val - computed_val) < 0.01) {
  add_check("Total value_nok consistent with diagnostics", "PASS",
            sprintf("%.2f NOK", computed_val))
} else {
  add_check("Total value_nok consistent with diagnostics", "FAIL",
            sprintf("diagnostics = %s, sum(sel) = %.4f",
                    diag_val("total_value_nok"), computed_val))
}

# --------------------------------------------------------------------------- #
# 10. Total volume in diagnostics == sum(price_volume_m3) in selected logs    #
# --------------------------------------------------------------------------- #

alog("", "--- 10. Total volume consistency ---")

diag_total_vol <- suppressWarnings(as.numeric(diag_val("total_volume_m3")))
computed_vol   <- sum(sel$price_volume_m3, na.rm = TRUE)
alog(sprintf("  Diagnostics total_volume_m3  : %s", diag_val("total_volume_m3")))
alog(sprintf("  sum(sel$price_volume_m3)     : %.6f", computed_vol))

if (!is.na(diag_total_vol) && abs(diag_total_vol - computed_vol) < 1e-4) {
  add_check("Total price_volume_m3 consistent with diagnostics", "PASS",
            sprintf("%.5f m3", computed_vol))
} else {
  add_check("Total price_volume_m3 consistent with diagnostics", "FAIL",
            sprintf("diagnostics = %s, sum(sel) = %.6f",
                    diag_val("total_volume_m3"), computed_vol))
}

# --------------------------------------------------------------------------- #
# 11. Independent DP reproduction                                              #
# --------------------------------------------------------------------------- #

alog("", "--- 11. Independent DP reproduction ---")

if (nrow(dp_input) == 0) {
  add_check("Independent DP: candidates available", "FAIL",
            "No DP input candidates in priced_candidates.csv")
} else {
  add_check("Independent DP: candidates available", "PASS",
            sprintf("%d rows", nrow(dp_input)))

  # PROFILE_STEP_CM needed by VolumeCalc_nfi – not called in audit DP,
  # but define for clarity.
  run_dp_audit <- function(cands) {
    all_pos  <- sort(unique(c(cands$StartPos, cands$StopPos)))
    sentinel <- max(all_pos) + 1L
    all_pos  <- c(all_pos, sentinel)
    n_pos    <- length(all_pos)
    dp_val   <- numeric(n_pos)
    dp_log   <- integer(n_pos)

    for (i in seq(n_pos - 1L, 1L)) {
      pos      <- all_pos[i]
      best_val <- dp_val[i + 1L]
      best_log <- 0L
      here_idx <- which(cands$StartPos == pos)
      for (j in here_idx) {
        stop_i     <- match(cands$StopPos[j], all_pos)
        future_val <- if (!is.na(stop_i)) dp_val[stop_i] else 0
        total_val  <- cands$value_nok[j] + future_val
        if (total_val > best_val) { best_val <- total_val; best_log <- j }
      }
      dp_val[i] <- best_val
      dp_log[i] <- best_log
    }

    sel_rows <- integer(0)
    i <- 1L
    while (i < n_pos) {
      if (dp_log[i] > 0L) {
        j        <- dp_log[i]
        sel_rows <- c(sel_rows, j)
        stop_i   <- match(cands$StopPos[j], all_pos)
        i        <- if (!is.na(stop_i)) stop_i else n_pos
      } else {
        i <- i + 1L
      }
    }
    list(best_val = dp_val[1], selected_rows = sel_rows)
  }

  audit_dp  <- run_dp_audit(dp_input)
  audit_sel <- dp_input[audit_dp$selected_rows, ]

  script_best <- suppressWarnings(as.numeric(diag_val("dp_best_value")))

  alog(sprintf("  Audit DP best value  : %.2f NOK", audit_dp$best_val))
  alog(sprintf("  Script DP best value : %s NOK",   diag_val("dp_best_value")))

  if (!is.na(script_best) && abs(audit_dp$best_val - script_best) < 0.01) {
    add_check("Independent DP reproduces same optimum value", "PASS",
              sprintf("%.2f NOK", audit_dp$best_val))
  } else {
    add_check("Independent DP reproduces same optimum value", "FAIL",
              sprintf("audit = %.2f, script = %s",
                      audit_dp$best_val, diag_val("dp_best_value")))
  }

  # Compare selected intervals.
  if (nrow(audit_sel) == nrow(sel)) {
    if (nrow(sel) == 0) {
      add_check("Independent DP: selected intervals match", "PASS",
                "Both selected 0 logs")
    } else {
      audit_ord  <- audit_sel[order(audit_sel$StartPos), ]
      script_ord <- sel[order(sel$StartPos), ]
      intervals_match <- all(
        audit_ord$StartPos == script_ord$StartPos &
          audit_ord$StopPos  == script_ord$StopPos,
        na.rm = TRUE)
      if (intervals_match) {
        add_check("Independent DP: selected intervals match", "PASS")
      } else {
        add_check("Independent DP: selected intervals match", "FAIL",
                  "StartPos / StopPos differ between audit and script")
      }
    }
  } else {
    add_check("Independent DP: selected intervals match", "FAIL",
              sprintf("audit selected %d, script selected %d",
                      nrow(audit_sel), nrow(sel)))
  }
}

# --------------------------------------------------------------------------- #
# 12. No unpriced 31 dm sawlog selected                                       #
# --------------------------------------------------------------------------- #

alog("", "--- 12. No unpriced 31 dm sawlog selected ---")

if (nrow(sel) == 0) {
  add_check("No unpriced 31 dm sawlog selected", "PASS", "0 logs selected")
} else {
  lc_col <- if ("LengthClass_dm" %in% names(sel)) sel$LengthClass_dm else rep(NA_integer_, nrow(sel))
  sel_31 <- sel[
    !is.na(lc_col) &
      lc_col == 31 &
      sel$ProductKey %in% c("spruce_sawlog", "pine_sawlog"), ]
  if (nrow(sel_31) == 0) {
    add_check("No unpriced 31 dm sawlog selected", "PASS")
  } else {
    add_check("No unpriced 31 dm sawlog selected", "FAIL",
              sprintf("%d such row(s) in selected logs", nrow(sel_31)))
  }
}

# --------------------------------------------------------------------------- #
# 13. STUMP_HEIGHT_CM is recorded and equals 10                               #
# --------------------------------------------------------------------------- #

alog("", "--- 13. STUMP_HEIGHT_CM check ---")

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
# 14. REVIEW: 31 dm sawlog unpriced candidates reported (not selected)        #
# --------------------------------------------------------------------------- #

alog("", "--- 14. REVIEW: 31 dm sawlog price gap ---")

n_dm31_str <- diag_val("n_unmatched_31dm_sawlog")
n_dm31     <- suppressWarnings(as.integer(n_dm31_str))

if (!is.na(n_dm31) && n_dm31 > 0) {
  add_check(
    "Unpriced 31 dm sawlog candidates exist (known issue)",
    "REVIEW",
    sprintf("%d candidate(s) with LengthClass_dm == 31 have no price. Price matrix starts at 34 dm.",
            n_dm31))
} else {
  add_check("Unpriced 31 dm sawlog candidates (known issue)", "PASS",
            "No unpriced 31 dm sawlog candidates")
}

# --------------------------------------------------------------------------- #
# 15. Save audit outputs                                                       #
# --------------------------------------------------------------------------- #

alog("", "--- 15. Saving audit outputs ---")

checks_df <- do.call(rbind, lapply(checks, function(x)
  data.frame(label = x$label, result = x$result, msg = x$msg,
             stringsAsFactors = FALSE)))

audit_csv <- file.path(opt_dir, "audit_06_one_tree_bucking_optimizer.csv")
readr::write_csv(checks_df, audit_csv)
alog(sprintf("  Saved: %s", basename(audit_csv)))

# --------------------------------------------------------------------------- #
# 16. Final verdict                                                            #
# --------------------------------------------------------------------------- #

alog(
  "",
  "=============================================================",
  "  AUDIT FINAL SUMMARY",
  "============================================================="
)

n_pass   <- sum(checks_df$result == "PASS")
n_review <- sum(checks_df$result == "REVIEW")
n_fail   <- sum(checks_df$result == "FAIL")

alog(sprintf("  PASS   : %d", n_pass))
alog(sprintf("  REVIEW : %d", n_review))
alog(sprintf("  FAIL   : %d", n_fail))

overall <- if (n_fail > 0) {
  "REJECTED"
} else if (n_review > 0) {
  "ACCEPTED WITH REVIEW ITEMS"
} else {
  "ACCEPTED"
}

alog("")
alog(sprintf("  Script 06 outputs: %s", overall))
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

message(sprintf("[%s] Script 06 audit complete.", overall))