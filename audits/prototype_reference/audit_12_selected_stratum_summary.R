# ==============================================================================
# audits/audit_12_selected_stratum_summary.R
#
# Purpose : Independent audit of R/12_summarise_selected_stratum_bucking.R
#           outputs.
#
# Checks  :
#  1.  Required output files exist
#  2.  Product-level totals equal selected-log totals
#  3.  Tree-level totals equal selected-log totals by tree
#  4.  Plot-level totals equal tree-level totals by plot
#  5.  Product-level total volume equals overall selected-log volume
#  6.  Product-level total value equals overall selected-log value
#  7.  Zero-log tree file contains exactly the trees with n_selected_logs == 0
#  8.  Simple per-ha values only where plot_area_m2 > 0
#  9.  No negative volume or value
# 10.  No missing ProductKey, price_volume_m3, or value_nok in selected logs
# 11.  Price gap summary contains only the known 31 dm sawlog issue
# 12.  Diameter cap summary: PriceDiameterClass_cm_used <= 50
# 13.  Plots created or clearly reported as skipped
# 14.  Summary report contains key numbers
#
# Verdict :
#   PASS                   -- all checks pass, no unexpected issues
#   ACCEPTED WITH REVIEW   -- known 31 dm price gap, zero-log small trees,
#                             or documented diameter capping only
#   FAIL                   -- totals don't reconcile, required files missing,
#                             negative values, unexpected missing prices,
#                             or per-ha without plot area
#
# Outputs :
#   outputs/selected_stratum_summary/audit_12_selected_stratum_summary.csv
#   outputs/selected_stratum_summary/audit_12_selected_stratum_summary.txt
# ==============================================================================


# ---- setup ----

library(readr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
sum_dir  <- file.path(root_dir, "outputs", "selected_stratum_summary")
buc_dir  <- file.path(root_dir, "outputs", "selected_stratum_bucking")

MAX_PRICE_DIAMETER_CLASS_CM <- 50L

audit_txt <- file.path(sum_dir, "audit_12_selected_stratum_summary.txt")
audit_csv <- file.path(sum_dir, "audit_12_selected_stratum_summary.csv")

cat("", file = audit_txt)

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_txt, append = TRUE)
}

alog(
  "=============================================================",
  "  Audit: R/12_summarise_selected_stratum_bucking.R outputs",
  sprintf("  Run: %s", Sys.time()),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
  "============================================================="
)

results <- data.frame(check = character(0), result = character(0),
                      detail = character(0), stringsAsFactors = FALSE)

record <- function(check, result, detail = "") {
  tag <- sprintf("  [%-6s] %s", result, check)
  if (nchar(detail) > 0L) tag <- paste0(tag, " | ", detail)
  alog(tag)
  results <<- rbind(results,
    data.frame(check = check, result = result, detail = detail,
               stringsAsFactors = FALSE))
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"


# ---- 1. Required output files ----

alog("", "--- 1. Required output files ---")

required_files <- c(
  "selected_stratum_summary_by_product.csv",
  "selected_stratum_summary_by_tree.csv",
  "selected_stratum_summary_by_plot.csv",
  "selected_stratum_summary_by_plot_product.csv",
  "selected_stratum_zero_log_trees.csv",
  "selected_stratum_price_gap_summary.csv",
  "selected_stratum_capped_diameter_summary.csv",
  "selected_stratum_summary_report.txt"
)

for (fn in required_files) {
  fp <- file.path(sum_dir, fn)
  if (file.exists(fp)) {
    record(sprintf("File exists: %s", fn), PASS)
  } else {
    record(sprintf("File exists: %s", fn), FAIL, "File not found")
  }
}


# ---- 2. Read summary outputs ----

alog("", "--- 2. Reading summary outputs ---")

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(data.frame())
  tryCatch(
    as.data.frame(readr::read_csv(path, show_col_types = FALSE)),
    error = function(e) data.frame()
  )
}

prod_sum      <- read_csv_safe(file.path(sum_dir, "selected_stratum_summary_by_product.csv"))
tree_sum      <- read_csv_safe(file.path(sum_dir, "selected_stratum_summary_by_tree.csv"))
plot_sum      <- read_csv_safe(file.path(sum_dir, "selected_stratum_summary_by_plot.csv"))
plot_prod_sum <- read_csv_safe(file.path(sum_dir, "selected_stratum_summary_by_plot_product.csv"))
zero_log      <- read_csv_safe(file.path(sum_dir, "selected_stratum_zero_log_trees.csv"))
price_gap     <- read_csv_safe(file.path(sum_dir, "selected_stratum_price_gap_summary.csv"))
cap_sum       <- read_csv_safe(file.path(sum_dir, "selected_stratum_capped_diameter_summary.csv"))

# Also read the original selected_logs from script 11 for ground-truth comparisons
sel <- read_csv_safe(file.path(buc_dir, "selected_stratum_selected_logs.csv"))
ts  <- read_csv_safe(file.path(buc_dir, "selected_stratum_tree_summary.csv"))

alog(
  sprintf("  prod_sum      : %d rows", nrow(prod_sum)),
  sprintf("  tree_sum      : %d rows", nrow(tree_sum)),
  sprintf("  plot_sum      : %d rows", nrow(plot_sum)),
  sprintf("  plot_prod_sum : %d rows", nrow(plot_prod_sum)),
  sprintf("  zero_log      : %d rows", nrow(zero_log)),
  sprintf("  price_gap     : %d rows", nrow(price_gap)),
  sprintf("  cap_sum       : %d rows", nrow(cap_sum)),
  sprintf("  sel (s11)     : %d rows", nrow(sel)),
  sprintf("  ts  (s11)     : %d rows", nrow(ts))
)


# ---- 3. Product-level totals vs selected-log totals ----

alog("", "--- 3. Product-level totals vs selected-log ground truth ---")

if (nrow(prod_sum) > 0L && nrow(sel) > 0L &&
    all(c("ProductKey", "total_volume_m3", "total_value_nok",
          "n_logs") %in% names(prod_sum)) &&
    all(c("ProductKey", "price_volume_m3", "value_nok") %in% names(sel))) {

  sel_agg <- do.call(rbind, lapply(split(sel, sel$ProductKey), function(d) {
    data.frame(ProductKey      = d$ProductKey[1L],
               n_logs_gt       = nrow(d),
               vol_gt          = sum(d$price_volume_m3, na.rm = TRUE),
               val_gt          = sum(d$value_nok,       na.rm = TRUE))
  }))

  merged_pk <- merge(prod_sum, sel_agg, by = "ProductKey", all = TRUE)

  n_logs_bad <- sum(
    !is.na(merged_pk$n_logs) & !is.na(merged_pk$n_logs_gt) &
    merged_pk$n_logs != merged_pk$n_logs_gt, na.rm = TRUE)
  n_vol_bad  <- sum(
    !is.na(merged_pk$total_volume_m3) & !is.na(merged_pk$vol_gt) &
    abs(merged_pk$total_volume_m3 - merged_pk$vol_gt) > 1e-4, na.rm = TRUE)
  n_val_bad  <- sum(
    !is.na(merged_pk$total_value_nok) & !is.na(merged_pk$val_gt) &
    abs(merged_pk$total_value_nok - merged_pk$val_gt) > 0.01, na.rm = TRUE)

  record("Product log counts match selected_logs",
         if (n_logs_bad == 0L) PASS else FAIL,
         sprintf("%d product(s) with count mismatch", n_logs_bad))
  record("Product volumes match selected_logs (|diff|<=1e-4)",
         if (n_vol_bad  == 0L) PASS else FAIL,
         sprintf("%d product(s) with volume mismatch", n_vol_bad))
  record("Product values match selected_logs (|diff|<=0.01)",
         if (n_val_bad  == 0L) PASS else FAIL,
         sprintf("%d product(s) with value mismatch", n_val_bad))
} else {
  record("Product-level totals check", REVIEW,
         "Cannot check (missing files or columns)")
}


# ---- 4. Tree-level totals vs selected-log totals ----

alog("", "--- 4. Tree-level totals vs selected-log ground truth ---")

if (nrow(tree_sum) > 0L && nrow(sel) > 0L &&
    all(c("treid", "total_selected_volume_m3",
          "total_selected_value_nok") %in% names(tree_sum)) &&
    all(c("treid", "price_volume_m3", "value_nok") %in% names(sel))) {

  sel_by_tree <- do.call(rbind, lapply(split(sel, sel$treid), function(d) {
    data.frame(treid   = d$treid[1L],
               vol_gt  = sum(d$price_volume_m3, na.rm = TRUE),
               val_gt  = sum(d$value_nok,       na.rm = TRUE))
  }))

  merged_tree <- merge(
    tree_sum[, c("treid", "total_selected_volume_m3", "total_selected_value_nok")],
    sel_by_tree, by = "treid", all.x = TRUE)

  # Trees not in sel have 0 logs; ground truth vol/val is 0
  merged_tree$vol_gt[is.na(merged_tree$vol_gt)] <- 0
  merged_tree$val_gt[is.na(merged_tree$val_gt)] <- 0

  n_vol_bad <- sum(
    abs(merged_tree$total_selected_volume_m3 - merged_tree$vol_gt) > 1e-4,
    na.rm = TRUE)
  n_val_bad <- sum(
    abs(merged_tree$total_selected_value_nok - merged_tree$val_gt) > 0.01,
    na.rm = TRUE)

  record("Tree volumes match selected_logs by tree (|diff|<=1e-4)",
         if (n_vol_bad == 0L) PASS else FAIL,
         sprintf("%d tree(s) with volume mismatch", n_vol_bad))
  record("Tree values match selected_logs by tree (|diff|<=0.01 NOK)",
         if (n_val_bad == 0L) PASS else FAIL,
         sprintf("%d tree(s) with value mismatch", n_val_bad))
} else {
  record("Tree-level totals check", REVIEW,
         "Cannot check (missing files or columns)")
}


# ---- 5. Plot-level totals vs tree-level totals ----

alog("", "--- 5. Plot-level totals vs tree-level ground truth ---")

if (nrow(plot_sum) > 0L && nrow(tree_sum) > 0L &&
    all(c("plot_id", "total_selected_volume_m3",
          "total_selected_value_nok") %in% names(plot_sum)) &&
    all(c("plot_id", "total_selected_volume_m3",
          "total_selected_value_nok") %in% names(tree_sum))) {

  ts_by_plot <- do.call(rbind, lapply(
    split(tree_sum, tree_sum$plot_id), function(d) {
      data.frame(plot_id = d$plot_id[1L],
                 vol_gt  = sum(d$total_selected_volume_m3, na.rm = TRUE),
                 val_gt  = sum(d$total_selected_value_nok, na.rm = TRUE))
    }))

  merged_plot <- merge(
    plot_sum[, c("plot_id", "total_selected_volume_m3",
                 "total_selected_value_nok")],
    ts_by_plot, by = "plot_id", all = TRUE)

  n_vol_bad <- sum(
    abs(merged_plot$total_selected_volume_m3 - merged_plot$vol_gt) > 1e-4,
    na.rm = TRUE)
  n_val_bad <- sum(
    abs(merged_plot$total_selected_value_nok - merged_plot$val_gt) > 0.01,
    na.rm = TRUE)

  record("Plot volumes match tree-level totals by plot (|diff|<=1e-4)",
         if (n_vol_bad == 0L) PASS else FAIL,
         sprintf("%d plot(s) with volume mismatch", n_vol_bad))
  record("Plot values match tree-level totals by plot (|diff|<=0.01 NOK)",
         if (n_val_bad == 0L) PASS else FAIL,
         sprintf("%d plot(s) with value mismatch", n_val_bad))
} else {
  record("Plot-level totals check", REVIEW,
         "Cannot check (missing files or columns)")
}


# ---- 6. Product grand totals ----

alog("", "--- 6. Product grand totals vs selected-log grand totals ---")

if (nrow(prod_sum) > 0L && nrow(sel) > 0L) {
  tot_vol_ps  <- sum(prod_sum$total_volume_m3, na.rm = TRUE)
  tot_val_ps  <- sum(prod_sum$total_value_nok, na.rm = TRUE)
  tot_vol_sel <- sum(sel$price_volume_m3,      na.rm = TRUE)
  tot_val_sel <- sum(sel$value_nok,            na.rm = TRUE)

  record("Grand total volume: prod_sum == selected_logs",
         if (abs(tot_vol_ps - tot_vol_sel) <= 1e-4) PASS else FAIL,
         sprintf("prod_sum=%.4f  sel=%.4f  diff=%.6f",
                 tot_vol_ps, tot_vol_sel, tot_vol_ps - tot_vol_sel))
  record("Grand total value: prod_sum == selected_logs",
         if (abs(tot_val_ps - tot_val_sel) <= 0.01) PASS else FAIL,
         sprintf("prod_sum=%.2f  sel=%.2f  diff=%.4f",
                 tot_val_ps, tot_val_sel, tot_val_ps - tot_val_sel))
} else {
  record("Grand total volume/value check", REVIEW,
         "Cannot check (missing files)")
}


# ---- 7. Zero-log tree file completeness ----

alog("", "--- 7. Zero-log tree file completeness ---")

if (nrow(tree_sum) > 0L && "n_selected_logs" %in% names(tree_sum)) {
  expected_zero <- sum(tree_sum$n_selected_logs == 0L, na.rm = TRUE)

  if (nrow(zero_log) == expected_zero) {
    record("Zero-log file has exactly the expected trees",
           PASS, sprintf("%d trees", expected_zero))
  } else {
    record("Zero-log file has exactly the expected trees",
           FAIL, sprintf("file=%d  expected=%d", nrow(zero_log), expected_zero))
  }

  if (nrow(zero_log) > 0L && "n_selected_logs" %in% names(tree_sum)) {
    zero_treids <- tree_sum$treid[tree_sum$n_selected_logs == 0L]
    if ("treid" %in% names(zero_log)) {
      unexpected_pos <- sum(!is.na(zero_log$treid) &
                              !(zero_log$treid %in% zero_treids))
      record("No zero-log file entry has n_selected_logs > 0",
             if (unexpected_pos == 0L) PASS else FAIL,
             sprintf("%d unexpected entries", unexpected_pos))
    }
  }
} else {
  record("Zero-log file check", REVIEW,
         "Cannot check (tree_sum missing or no n_selected_logs column)")
}


# ---- 8. Per-ha values only where plot_area_m2 > 0 ----

alog("", "--- 8. Per-ha values only where plot_area_m2 > 0 ---")

if (nrow(plot_sum) > 0L &&
    all(c("plot_area_m2", "simple_volume_m3_ha",
          "simple_value_nok_ha") %in% names(plot_sum))) {

  no_area  <- is.na(plot_sum$plot_area_m2) | plot_sum$plot_area_m2 <= 0
  has_vol  <- !is.na(plot_sum$simple_volume_m3_ha)
  has_val  <- !is.na(plot_sum$simple_value_nok_ha)

  n_bad <- sum(no_area & (has_vol | has_val), na.rm = TRUE)

  record("Per-ha values absent where plot_area_m2 missing/zero",
         if (n_bad == 0L) PASS else FAIL,
         sprintf("%d row(s) with per-ha value but no valid plot_area_m2", n_bad))

  n_has_ha <- sum(!no_area, na.rm = TRUE)
  record(sprintf("Plots with per-ha values (%d/%d)", n_has_ha, nrow(plot_sum)),
         PASS,
         sprintf("%.1f%% of plots have valid plot_area_m2",
                 n_has_ha / nrow(plot_sum) * 100))
} else {
  record("Per-ha value check", REVIEW,
         "Required columns missing from plot_sum")
}


# ---- 9. No negative volume or value ----

alog("", "--- 9. No negative volume or value ---")

if (nrow(sel) > 0L) {
  n_neg_vol <- sum(!is.na(sel$price_volume_m3) & sel$price_volume_m3 < 0)
  n_neg_val <- sum(!is.na(sel$value_nok)        & sel$value_nok < 0)
  record("No negative price_volume_m3 in selected_logs",
         if (n_neg_vol == 0L) PASS else FAIL,
         sprintf("%d negative values", n_neg_vol))
  record("No negative value_nok in selected_logs",
         if (n_neg_val == 0L) PASS else FAIL,
         sprintf("%d negative values", n_neg_val))
}

if (nrow(tree_sum) > 0L) {
  n_neg_ts_vol <- sum(!is.na(tree_sum$total_selected_volume_m3) &
                        tree_sum$total_selected_volume_m3 < 0)
  n_neg_ts_val <- sum(!is.na(tree_sum$total_selected_value_nok) &
                        tree_sum$total_selected_value_nok < 0)
  record("No negative total_selected_volume_m3 in tree_sum",
         if (n_neg_ts_vol == 0L) PASS else FAIL,
         sprintf("%d negative values", n_neg_ts_vol))
  record("No negative total_selected_value_nok in tree_sum",
         if (n_neg_ts_val == 0L) PASS else FAIL,
         sprintf("%d negative values", n_neg_ts_val))
}


# ---- 10. No missing ProductKey / price_volume_m3 / value_nok ----

alog("", "--- 10. No missing key columns in selected logs ---")

if (nrow(sel) > 0L) {
  n_na_pk  <- sum(is.na(sel$ProductKey))
  n_na_vol <- if ("price_volume_m3" %in% names(sel)) sum(is.na(sel$price_volume_m3)) else NA_integer_
  n_na_val <- if ("value_nok" %in% names(sel)) sum(is.na(sel$value_nok)) else NA_integer_

  record("No missing ProductKey in selected_logs",
         if (n_na_pk == 0L) PASS else FAIL,
         sprintf("%d NA", n_na_pk))
  record("No missing price_volume_m3 in selected_logs",
         if (!is.na(n_na_vol) && n_na_vol == 0L) PASS else FAIL,
         sprintf("%d NA", n_na_vol %||% NA_integer_))
  record("No missing value_nok in selected_logs",
         if (!is.na(n_na_val) && n_na_val == 0L) PASS else FAIL,
         sprintf("%d NA", n_na_val %||% NA_integer_))
} else {
  record("Missing key columns check", REVIEW, "No selected logs to check")
}


# ---- 11. Price gap summary -- only known 31 dm sawlog issue ----

alog("", "--- 11. Price gap summary ---")

if (nrow(price_gap) > 0L) {
  if ("known_issue" %in% names(price_gap)) {
    n_unknown <- sum(!price_gap$known_issue, na.rm = TRUE)
    n_known   <- sum( price_gap$known_issue, na.rm = TRUE)

    record("Price gap summary: known 31 dm issue present",
           if (n_known > 0L) PASS else REVIEW,
           sprintf("%d known-issue row(s) in price_gap_summary", n_known))

    record("Price gap summary: no unexpected unpriced entries",
           if (n_unknown == 0L) PASS else FAIL,
           sprintf("%d unexpected unpriced row(s) in price_gap_summary",
                   n_unknown))
  } else {
    record("Price gap summary structure", REVIEW,
           "known_issue column missing from price_gap_summary")
  }

  # Each known row should be ProductKey containing "sawlog", LengthClass_dm == 31
  if ("LengthClass_dm" %in% names(price_gap) &&
      "ProductKey"     %in% names(price_gap)) {
    known_rows <- price_gap[!is.na(price_gap$known_issue) &
                              price_gap$known_issue, ]
    not_31  <- sum(!is.na(known_rows$LengthClass_dm) &
                     known_rows$LengthClass_dm != 31L)
    not_saw <- sum(!is.na(known_rows$ProductKey) &
                     !grepl("sawlog", known_rows$ProductKey))
    record("Known-issue rows: LengthClass_dm == 31",
           if (not_31  == 0L) PASS else REVIEW,
           sprintf("%d known-issue row(s) not at 31 dm", not_31))
    record("Known-issue rows: ProductKey contains 'sawlog'",
           if (not_saw == 0L) PASS else REVIEW,
           sprintf("%d known-issue row(s) not a sawlog product", not_saw))
  }
} else {
  record("Price gap summary has rows", REVIEW, "price_gap_summary is empty")
}


# ---- 12. Diameter cap: used <= MAX ----

alog("", "--- 12. Diameter cap summary ---")

if (nrow(sel) > 0L &&
    all(c("PriceDiameterClass_cm_used",
          "PriceDiameterClass_cm_uncapped") %in% names(sel))) {

  n_used_gt_max <- sum(
    !is.na(sel$PriceDiameterClass_cm_used) &
    sel$PriceDiameterClass_cm_used > MAX_PRICE_DIAMETER_CLASS_CM, na.rm = TRUE)

  record("No selected log has PriceDiameterClass_cm_used > MAX",
         if (n_used_gt_max == 0L) PASS else FAIL,
         sprintf("%d selected log(s) with used > %d cm",
                 n_used_gt_max, MAX_PRICE_DIAMETER_CLASS_CM))

  # Check cap summary confirms correctness
  if (nrow(cap_sum) > 0L && "cap_applied_correctly" %in% names(cap_sum)) {
    all_correct <- all(cap_sum$cap_applied_correctly, na.rm = TRUE)
    record("Diameter cap summary: cap_applied_correctly == TRUE for all rows",
           if (all_correct) PASS else FAIL,
           sprintf("%d row(s) with cap_applied_correctly == FALSE",
                   sum(!cap_sum$cap_applied_correctly, na.rm = TRUE)))
  }

  n_capped_sel <- sum(
    !is.na(sel$PriceDiameterClass_cm_uncapped) &
    sel$PriceDiameterClass_cm_uncapped > MAX_PRICE_DIAMETER_CLASS_CM)

  if (n_capped_sel > 0L) {
    record(sprintf("REVIEW: %d selected log(s) had price-diameter cap applied",
                   n_capped_sel),
           REVIEW,
           sprintf("PriceDiameterClass_cm_used capped at %d cm; uncapped stored in _uncapped",
                   MAX_PRICE_DIAMETER_CLASS_CM))
  } else {
    record("No selected logs required diameter cap",
           PASS, "0 selected logs with uncapped > MAX")
  }
} else {
  record("Diameter cap check", REVIEW,
         "Required columns missing from selected_logs")
}


# ---- 13. Plot files exist ----

alog("", "--- 13. Plot files ---")

plot_files <- c(
  "plot_value_by_dbh.png",
  "plot_volume_by_dbh.png",
  "plot_product_volume_bar.png",
  "plot_product_value_bar.png",
  "plot_zero_log_tree_dbh_distribution.png",
  "plot_plot_level_value_per_ha_distribution.png",
  "plot_plot_level_volume_per_ha_distribution.png"
)

n_plots_found   <- 0L
n_plots_missing <- 0L

for (fn in plot_files) {
  fp <- file.path(sum_dir, fn)
  if (file.exists(fp)) {
    n_plots_found <- n_plots_found + 1L
  } else {
    n_plots_missing <- n_plots_missing + 1L
    alog(sprintf("  [REVIEW] Plot file missing (skipped): %s", fn))
  }
}

record(sprintf("Plot files created (%d / %d)", n_plots_found, length(plot_files)),
       if (n_plots_missing == 0L) PASS else REVIEW,
       sprintf("%d found, %d missing/skipped", n_plots_found, n_plots_missing))


# ---- 14. Summary report key numbers ----

alog("", "--- 14. Summary report content ---")

if (file.exists(file.path(sum_dir, "selected_stratum_summary_report.txt"))) {
  report_text <- paste(
    readLines(file.path(sum_dir, "selected_stratum_summary_report.txt")),
    collapse = "\n")

  key_patterns <- list(
    "trees processed"     = "Trees processed|n_trees_valid",
    "plots"               = "Plots",
    "selected logs"       = "Selected logs",
    "total volume"        = "Total volume",
    "total value"         = "Total value",
    "zero-log trees"      = "Zero-log trees",
    "31 dm unpriced"      = "31 dm",
    "capped diameter"     = "[Cc]apped"
  )

  for (label in names(key_patterns)) {
    found <- grepl(key_patterns[[label]], report_text, perl = TRUE)
    record(sprintf("Report contains: %s", label),
           if (found) PASS else REVIEW,
           if (found) "found" else "not found in report")
  }
} else {
  record("Summary report exists", FAIL, "File not found")
}


# ---- verdict ----

alog(
  "",
  "=============================================================",
  "  AUDIT FINAL SUMMARY",
  "============================================================="
)

tbl <- as.data.frame(table(results$result))
names(tbl) <- c("result", "n")
print(tbl)
alog(capture.output(print(tbl)))

n_fail   <- sum(results$result == FAIL)
n_review <- sum(results$result == REVIEW)

verdict <- if (n_fail > 0L) {
  "FAIL"
} else if (n_review > 0L) {
  "ACCEPTED WITH REVIEW ITEMS"
} else {
  "PASS"
}

alog(
  "",
  sprintf("  Script 12 outputs: %s", verdict),
  "============================================================="
)

if (n_review > 0L || n_fail > 0L) {
  alog("", if (n_fail > 0L) "FAIL items:" else "REVIEW items:")
  for (i in seq_len(nrow(results))) {
    if (results$result[i] %in% c(FAIL, REVIEW)) {
      alog(sprintf("  [%s] %s | %s",
                   results$result[i], results$check[i], results$detail[i]))
    }
  }
}

alog(sprintf("\nAudit log: %s", audit_txt))
cat(sprintf("[%s] Script 12 audit complete.\n", verdict))


# ---- save audit csv ----

readr::write_csv(results, audit_csv)
