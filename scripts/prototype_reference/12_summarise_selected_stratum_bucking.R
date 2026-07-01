# ==============================================================================
# R/12_summarise_selected_stratum_bucking.R
#
# Purpose : Summarise and report the selected-stratum bucking outputs from
#           R/11_run_bucking_on_selected_stratum.R.
#
#   This script is a reporting and quality-control step.
#   It does NOT rerun taper, candidate generation, valuation, or optimisation.
#   No NFI expansion factors are applied.
#   Simple per-ha values (simple_volume_m3_ha, simple_value_nok_ha) are
#   computed only where plot_area_m2 is available and > 0, and are clearly
#   labelled as unexpanded plot-scaling (NOT ha-expanded NFI estimates).
#
# Inputs (outputs/selected_stratum_bucking/):
#   selected_stratum_selected_logs.csv
#   selected_stratum_tree_summary.csv
#   selected_stratum_candidate_diagnostics.csv
#   selected_stratum_unpriced_summary.csv
#   selected_stratum_run_summary.txt
#   selected_stratum_plot_summary.csv    (produced by script 11)
#
# Outputs (outputs/selected_stratum_summary/):
#   selected_stratum_summary_by_product.csv
#   selected_stratum_summary_by_tree.csv
#   selected_stratum_summary_by_plot.csv
#   selected_stratum_summary_by_plot_product.csv
#   selected_stratum_zero_log_trees.csv
#   selected_stratum_price_gap_summary.csv
#   selected_stratum_capped_diameter_summary.csv
#   selected_stratum_summary_report.txt
#   plot_value_by_dbh.png
#   plot_volume_by_dbh.png
#   plot_product_volume_bar.png
#   plot_product_value_bar.png
#   plot_zero_log_tree_dbh_distribution.png
#   plot_plot_level_value_per_ha_distribution.png
#   plot_plot_level_volume_per_ha_distribution.png
# ==============================================================================


# ---- setup ----

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tibble)
})

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
in_dir   <- file.path(root_dir, "outputs", "selected_stratum_bucking")
out_dir  <- file.path(root_dir, "outputs", "selected_stratum_summary")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

MAX_PRICE_DIAMETER_CLASS_CM <- 50L   # must match script 11

report_txt <- file.path(out_dir, "selected_stratum_summary_report.txt")
cat("", file = report_txt)

rlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = report_txt, append = TRUE)
}

rlog(
  "=============================================================",
  "  Selected stratum bucking -- summary report",
  sprintf("  Generated    : %s", Sys.time()),
  sprintf("  Input dir    : %s", in_dir),
  sprintf("  Output dir   : %s", out_dir),
  "  NOTE: simple_*_ha values are unexpanded plot-scaling only.",
  "        No NFI expansion factors have been applied.",
  "============================================================="
)


# ---- read inputs ----

rlog("", "--- Reading inputs ---")

sel  <- as.data.frame(readr::read_csv(
  file.path(in_dir, "selected_stratum_selected_logs.csv"),
  show_col_types = FALSE))
ts   <- as.data.frame(readr::read_csv(
  file.path(in_dir, "selected_stratum_tree_summary.csv"),
  show_col_types = FALSE))
diag <- as.data.frame(readr::read_csv(
  file.path(in_dir, "selected_stratum_candidate_diagnostics.csv"),
  show_col_types = FALSE))
unpr <- as.data.frame(readr::read_csv(
  file.path(in_dir, "selected_stratum_unpriced_summary.csv"),
  show_col_types = FALSE))
ps11 <- as.data.frame(readr::read_csv(
  file.path(in_dir, "selected_stratum_plot_summary.csv"),
  show_col_types = FALSE))

rlog(
  sprintf("  selected_logs       : %d rows, %d cols", nrow(sel), ncol(sel)),
  sprintf("  tree_summary        : %d rows, %d cols", nrow(ts),  ncol(ts)),
  sprintf("  diagnostics         : %d rows", nrow(diag)),
  sprintf("  unpriced_summary    : %d rows", nrow(unpr)),
  sprintf("  plot_summary_s11    : %d rows", nrow(ps11))
)

# Helper: look up scalar from diagnostics key-value table
diag_val <- function(key) {
  v <- diag$value[diag$item == key]
  if (length(v) == 0L) NA_character_ else v[1L]
}


# ---- 1. Product-level summary ----

rlog("", "--- 1. Product-level summary ---")

prod_sum <- sel %>%
  group_by(ProductKey) %>%
  summarise(
    n_logs                      = n(),
    total_volume_m3             = sum(price_volume_m3,    na.rm = TRUE),
    total_value_nok             = sum(value_nok,          na.rm = TRUE),
    mean_log_volume_m3          = mean(price_volume_m3,   na.rm = TRUE),
    mean_log_value_nok          = mean(value_nok,         na.rm = TRUE),
    mean_price_nok_m3           = if (sum(price_volume_m3, na.rm = TRUE) > 0)
                                    sum(value_nok, na.rm = TRUE) /
                                    sum(price_volume_m3, na.rm = TRUE)
                                  else NA_real_,
    min_log_length_cm           = min(LogLength, na.rm = TRUE),
    max_log_length_cm           = max(LogLength, na.rm = TRUE),
    min_price_diameter_class_cm = min(PriceDiameterClass_cm_used, na.rm = TRUE),
    max_price_diameter_class_cm = max(PriceDiameterClass_cm_used, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(prod_sum,
  file.path(out_dir, "selected_stratum_summary_by_product.csv"))
rlog(sprintf("  Saved: selected_stratum_summary_by_product.csv (%d products)",
             nrow(prod_sum)))

for (i in seq_len(nrow(prod_sum))) {
  rlog(sprintf("    %-22s  %6d logs  %9.4f m3  %12.2f NOK",
               prod_sum$ProductKey[i],
               prod_sum$n_logs[i],
               prod_sum$total_volume_m3[i],
               prod_sum$total_value_nok[i]))
}
rlog(sprintf("    %-22s  %6d logs  %9.4f m3  %12.2f NOK",
             "TOTAL",
             sum(prod_sum$n_logs),
             sum(prod_sum$total_volume_m3),
             sum(prod_sum$total_value_nok)))


# ---- 2. Tree-level summary ----

rlog("", "--- 2. Tree-level summary ---")

tree_sum <- ts %>%
  mutate(
    zero_log_tree   = n_selected_logs == 0L,
    sawlog_vol_share = ifelse(
      !is.na(total_selected_volume_m3) & total_selected_volume_m3 > 0,
      selected_sawlog_volume_m3 / total_selected_volume_m3, NA_real_),
    sawlog_val_share = ifelse(
      !is.na(total_selected_value_nok) & total_selected_value_nok > 0,
      selected_sawlog_value_nok / total_selected_value_nok, NA_real_),
    value_per_m3     = ifelse(
      !is.na(total_selected_volume_m3) & total_selected_volume_m3 > 0,
      total_selected_value_nok / total_selected_volume_m3, NA_real_)
  )

readr::write_csv(tree_sum,
  file.path(out_dir, "selected_stratum_summary_by_tree.csv"))
rlog(sprintf("  Saved: selected_stratum_summary_by_tree.csv (%d trees)",
             nrow(tree_sum)))


# ---- 3. Plot-level summary ----

rlog("", "--- 3. Plot-level summary ---")

# Aggregate tree-level data by plot
plot_tree_agg <- ts %>%
  group_by(plot_id) %>%
  summarise(
    n_trees                     = n(),
    n_zero_log_trees            = sum(n_selected_logs == 0L, na.rm = TRUE),
    total_selected_volume_m3    = sum(total_selected_volume_m3, na.rm = TRUE),
    total_selected_value_nok    = sum(total_selected_value_nok, na.rm = TRUE),
    total_sawlog_volume_m3      = sum(selected_sawlog_volume_m3, na.rm = TRUE),
    total_pulpwood_volume_m3    = sum(selected_pulpwood_volume_m3, na.rm = TRUE),
    total_waste_volume_m3       = sum(selected_waste_volume_m3, na.rm = TRUE),
    total_sawlog_value_nok      = sum(selected_sawlog_value_nok, na.rm = TRUE),
    total_pulpwood_value_nok    = sum(selected_pulpwood_value_nok, na.rm = TRUE),
    total_waste_value_nok       = sum(selected_waste_value_nok, na.rm = TRUE),
    .groups = "drop"
  )

# Join plot_area_m2 from script 11 plot summary
plot_area_lkp <- ps11[, c("plot_id", "plot_area_m2")]
plot_sum <- merge(plot_tree_agg, plot_area_lkp, by = "plot_id", all.x = TRUE)

# Simple per-ha scaling (unexpanded; no NFI factors applied)
# Only computed where plot_area_m2 is present and > 0.
has_area <- !is.na(plot_sum$plot_area_m2) & plot_sum$plot_area_m2 > 0
plot_sum$simple_volume_m3_ha  <- NA_real_
plot_sum$simple_value_nok_ha  <- NA_real_
plot_sum$simple_volume_m3_ha[has_area] <-
  plot_sum$total_selected_volume_m3[has_area] /
  plot_sum$plot_area_m2[has_area] * 10000
plot_sum$simple_value_nok_ha[has_area] <-
  plot_sum$total_selected_value_nok[has_area] /
  plot_sum$plot_area_m2[has_area] * 10000
plot_sum$note_per_ha <- ifelse(
  has_area,
  "simple unexpanded plot-scaling; no NFI expansion",
  "plot_area_m2 missing; per-ha values not computed")

readr::write_csv(plot_sum,
  file.path(out_dir, "selected_stratum_summary_by_plot.csv"))
rlog(sprintf("  Saved: selected_stratum_summary_by_plot.csv (%d plots)",
             nrow(plot_sum)))

n_with_ha  <- sum(has_area)
rlog(sprintf("  Plots with per-ha values : %d / %d", n_with_ha, nrow(plot_sum)))

if (n_with_ha > 0L) {
  vha <- plot_sum$simple_volume_m3_ha[has_area]
  vha_nok <- plot_sum$simple_value_nok_ha[has_area]
  rlog(
    sprintf("  Volume m3/ha  : mean=%.2f  median=%.2f  min=%.2f  max=%.2f",
            mean(vha), stats::median(vha), min(vha), max(vha)),
    sprintf("  Value NOK/ha  : mean=%.0f  median=%.0f  min=%.0f  max=%.0f",
            mean(vha_nok), stats::median(vha_nok), min(vha_nok), max(vha_nok))
  )
}


# ---- 4. Plot x product summary ----

rlog("", "--- 4. Plot x product summary ---")

plot_prod_sum <- sel %>%
  group_by(plot_id, ProductKey) %>%
  summarise(
    n_logs          = n(),
    total_volume_m3 = sum(price_volume_m3, na.rm = TRUE),
    total_value_nok = sum(value_nok,       na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(plot_prod_sum,
  file.path(out_dir, "selected_stratum_summary_by_plot_product.csv"))
rlog(sprintf("  Saved: selected_stratum_summary_by_plot_product.csv (%d rows)",
             nrow(plot_prod_sum)))


# ---- 5. Zero-log tree detail ----

rlog("", "--- 5. Zero-log tree summary ---")

zero_log <- tree_sum %>%
  filter(zero_log_tree) %>%
  select(treid, plot_id, dbh_cm, height_m, species_group,
         n_generated_candidates, n_priced_candidates,
         optimizer_status, optimizer_message) %>%
  arrange(dbh_cm)

readr::write_csv(zero_log,
  file.path(out_dir, "selected_stratum_zero_log_trees.csv"))
rlog(sprintf("  Saved: selected_stratum_zero_log_trees.csv (%d trees)",
             nrow(zero_log)))

if (nrow(zero_log) > 0L) {
  rlog(
    sprintf("  Zero-log trees : %d (%.1f%% of %d)",
            nrow(zero_log),
            nrow(zero_log) / nrow(ts) * 100,
            nrow(ts)),
    sprintf("  Max DBH        : %.1f cm",  max(zero_log$dbh_cm, na.rm = TRUE)),
    sprintf("  Max height     : %.1f m",   max(zero_log$height_m, na.rm = TRUE))
  )
  # Count by 2-cm DBH class
  dbh_breaks <- seq(0, ceiling(max(zero_log$dbh_cm, na.rm = TRUE) / 2) * 2, by = 2)
  dbh_cls <- cut(zero_log$dbh_cm, breaks = dbh_breaks, right = FALSE)
  dbh_tbl <- as.data.frame(table(DBH_class = dbh_cls), stringsAsFactors = FALSE)
  dbh_tbl <- dbh_tbl[dbh_tbl$Freq > 0L, ]
  rlog("  By 2-cm DBH class:")
  for (i in seq_len(nrow(dbh_tbl))) {
    rlog(sprintf("    %s : %d", dbh_tbl$DBH_class[i], dbh_tbl$Freq[i]))
  }
}


# ---- 6. Price gap summary ----

rlog("", "--- 6. Price gap summary ---")

KNOWN_GAP_PRODUCT <- "spruce_sawlog"
KNOWN_GAP_DM      <- 31L

price_gap_sum <- unpr %>%
  mutate(
    known_issue = (ProductKey == KNOWN_GAP_PRODUCT &
                   LengthClass_dm == KNOWN_GAP_DM),
    issue_label = ifelse(
      known_issue,
      "KNOWN: sawlog price matrix starts at 34 dm",
      "UNEXPECTED: review required")
  ) %>%
  arrange(known_issue, ProductKey, LengthClass_dm)

readr::write_csv(price_gap_sum,
  file.path(out_dir, "selected_stratum_price_gap_summary.csv"))
rlog(sprintf("  Saved: selected_stratum_price_gap_summary.csv (%d rows)",
             nrow(price_gap_sum)))

if (nrow(price_gap_sum) > 0L) {
  for (i in seq_len(nrow(price_gap_sum))) {
    rlog(sprintf("    %-20s  %3d dm  %8d candidates  [%s]",
                 price_gap_sum$ProductKey[i],
                 price_gap_sum$LengthClass_dm[i],
                 price_gap_sum$n[i],
                 price_gap_sum$issue_label[i]))
  }
}


# ---- 7. Diameter cap summary ----

rlog("", "--- 7. Diameter cap summary ---")

# From selected logs: rows where uncapped > MAX
capped_sel <- sel %>%
  filter(!is.na(PriceDiameterClass_cm_uncapped) &
           PriceDiameterClass_cm_uncapped > MAX_PRICE_DIAMETER_CLASS_CM)

cap_by_pk <- capped_sel %>%
  group_by(ProductKey) %>%
  summarise(
    n_selected_logs_capped      = n(),
    max_uncapped_diameter_cm    = max(PriceDiameterClass_cm_uncapped, na.rm = TRUE),
    max_used_diameter_cm        = max(PriceDiameterClass_cm_used,     na.rm = TRUE),
    all_used_le_max             = all(PriceDiameterClass_cm_used <= MAX_PRICE_DIAMETER_CLASS_CM,
                                      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(cap_applied_correctly = all_used_le_max)

# Also note total capped candidates from diagnostics
n_cap_cands <- as.integer(diag_val("n_price_diameter_capped_total"))

cap_summary <- cap_by_pk %>%
  mutate(
    n_capped_candidates_total   = n_cap_cands,  # from diagnostics (all cands, not just selected)
    note = sprintf(
      "PriceDiameterClass_cm_used capped at %d cm; original in _uncapped column",
      MAX_PRICE_DIAMETER_CLASS_CM)
  )

readr::write_csv(cap_summary,
  file.path(out_dir, "selected_stratum_capped_diameter_summary.csv"))
rlog(sprintf("  Saved: selected_stratum_capped_diameter_summary.csv (%d rows)",
             nrow(cap_summary)))

rlog(
  sprintf("  Capped candidates (all products, from diagnostics) : %s",
          ifelse(is.na(n_cap_cands), "NA", format(n_cap_cands, big.mark = ","))),
  sprintf("  Capped selected logs                               : %d",
          nrow(capped_sel))
)
if (nrow(cap_by_pk) > 0L) {
  for (i in seq_len(nrow(cap_by_pk))) {
    rlog(sprintf("    %-22s  %d selected capped  max_uncapped=%d cm  used_le_max=%s",
                 cap_by_pk$ProductKey[i],
                 cap_by_pk$n_selected_logs_capped[i],
                 cap_by_pk$max_uncapped_diameter_cm[i],
                 cap_by_pk$cap_applied_correctly[i]))
  }
}


# ---- 8. Summary report ----

rlog(
  "",
  "=============================================================",
  "  SUMMARY",
  "=============================================================",
  sprintf("  Stratum file          : %s", diag_val("stratum_file")),
  sprintf("  Trees processed       : %s", diag_val("n_trees_valid")),
  sprintf("  Plots                 : %s", diag_val("n_plots_represented")),
  sprintf("  Profile failures      : %s", diag_val("n_profile_failures")),
  sprintf("  Optimizer errors      : %s", diag_val("n_trees_optimizer_fail")),
  sprintf("  Zero-log trees        : %s (%.1f%%)",
          diag_val("n_trees_zero_selected_logs"),
          as.numeric(diag_val("n_trees_zero_selected_logs")) /
            as.numeric(diag_val("n_trees_valid")) * 100),
  sprintf("  Selected logs         : %s", diag_val("n_selected_logs_total")),
  sprintf("  Total volume (m3)     : %s", diag_val("total_volume_m3_all_trees")),
  sprintf("  Total value (NOK)     : %s", diag_val("total_value_nok_all_trees")),
  sprintf("  31 dm unpriced cands  : %s", diag_val("n_unpriced_31dm_sawlog_total")),
  sprintf("  Capped diameter cands : %s", diag_val("n_price_diameter_capped_total")),
  sprintf("  Elapsed               : %s s", diag_val("elapsed_seconds"))
)


# ---- 9. Diagnostic plots ----

rlog("", "--- 9. Diagnostic plots ---")

plot_ok <- function(name, expr) {
  tryCatch({
    expr
    rlog(sprintf("  Saved: %s", name))
  }, error = function(e) {
    rlog(sprintf("  SKIPPED: %s (%s)", name, conditionMessage(e)))
  })
}

# Trees with at least 1 selected log (for value/volume vs DBH)
ts_pos <- tree_sum[!is.na(tree_sum$total_selected_volume_m3) &
                     tree_sum$total_selected_volume_m3 > 0, ]

# 9a. Value by DBH
plot_ok("plot_value_by_dbh.png", {
  p <- ggplot(ts_pos, aes(x = dbh_cm, y = total_selected_value_nok)) +
    geom_point(alpha = 0.2, size = 0.7, colour = "steelblue") +
    geom_smooth(method = "loess", se = FALSE, colour = "firebrick", linewidth = 0.8) +
    labs(title = "Total selected value by DBH (trees with >= 1 log)",
         x = "DBH (cm)", y = "Total value (NOK)") +
    theme_bw()
  ggsave(file.path(out_dir, "plot_value_by_dbh.png"), p,
         width = 8, height = 5, dpi = 150)
})

# 9b. Volume by DBH
plot_ok("plot_volume_by_dbh.png", {
  p <- ggplot(ts_pos, aes(x = dbh_cm, y = total_selected_volume_m3)) +
    geom_point(alpha = 0.2, size = 0.7, colour = "steelblue") +
    geom_smooth(method = "loess", se = FALSE, colour = "firebrick", linewidth = 0.8) +
    labs(title = "Total selected volume by DBH (trees with >= 1 log)",
         x = "DBH (cm)", y = "Volume (m3)") +
    theme_bw()
  ggsave(file.path(out_dir, "plot_volume_by_dbh.png"), p,
         width = 8, height = 5, dpi = 150)
})

# 9c. Product volume bar
plot_ok("plot_product_volume_bar.png", {
  p <- ggplot(prod_sum, aes(x = reorder(ProductKey, -total_volume_m3),
                             y = total_volume_m3, fill = ProductKey)) +
    geom_col() +
    labs(title = "Total selected volume by product",
         x = "Product", y = "Volume (m3)") +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(out_dir, "plot_product_volume_bar.png"), p,
         width = 7, height = 5, dpi = 150)
})

# 9d. Product value bar
plot_ok("plot_product_value_bar.png", {
  p <- ggplot(prod_sum, aes(x = reorder(ProductKey, -total_value_nok),
                             y = total_value_nok, fill = ProductKey)) +
    geom_col() +
    labs(title = "Total selected value by product",
         x = "Product", y = "Value (NOK)") +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(angle = 25, hjust = 1))
  ggsave(file.path(out_dir, "plot_product_value_bar.png"), p,
         width = 7, height = 5, dpi = 150)
})

# 9e. Zero-log tree DBH distribution
plot_ok("plot_zero_log_tree_dbh_distribution.png", {
  if (nrow(zero_log) > 0L) {
    p <- ggplot(zero_log, aes(x = dbh_cm)) +
      geom_histogram(binwidth = 1, fill = "tomato", colour = "white") +
      labs(title = sprintf("DBH distribution of zero-log trees (n=%d)", nrow(zero_log)),
           x = "DBH (cm)", y = "Count") +
      theme_bw()
    ggsave(file.path(out_dir, "plot_zero_log_tree_dbh_distribution.png"), p,
           width = 7, height = 4, dpi = 150)
  } else {
    stop("no zero-log trees")
  }
})

# 9f. Per-ha volume distribution
plot_ok("plot_plot_level_volume_per_ha_distribution.png", {
  ph_data <- plot_sum[!is.na(plot_sum$simple_volume_m3_ha), ]
  if (nrow(ph_data) == 0L) stop("no per-ha values")
  p <- ggplot(ph_data, aes(x = simple_volume_m3_ha)) +
    geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
    labs(
      title = "Plot-level simple volume per ha (unexpanded)",
      subtitle = "simple_volume_m3_ha = plot_volume / plot_area * 10000; no NFI expansion",
      x = "Volume (m3/ha, unexpanded)", y = "Number of plots") +
    theme_bw()
  ggsave(file.path(out_dir, "plot_plot_level_volume_per_ha_distribution.png"), p,
         width = 8, height = 5, dpi = 150)
})

# 9g. Per-ha value distribution
plot_ok("plot_plot_level_value_per_ha_distribution.png", {
  ph_data <- plot_sum[!is.na(plot_sum$simple_value_nok_ha), ]
  if (nrow(ph_data) == 0L) stop("no per-ha values")
  p <- ggplot(ph_data, aes(x = simple_value_nok_ha)) +
    geom_histogram(bins = 40, fill = "darkorange", colour = "white") +
    labs(
      title = "Plot-level simple value per ha (unexpanded)",
      subtitle = "simple_value_nok_ha = plot_value / plot_area * 10000; no NFI expansion",
      x = "Value (NOK/ha, unexpanded)", y = "Number of plots") +
    theme_bw()
  ggsave(file.path(out_dir, "plot_plot_level_value_per_ha_distribution.png"), p,
         width = 8, height = 5, dpi = 150)
})


# ---- done ----

rlog(
  "",
  "=============================================================",
  "  Script 12 complete.",
  sprintf("  Outputs written to: %s", out_dir),
  "============================================================="
)

cat("Script 12 complete.\n")
