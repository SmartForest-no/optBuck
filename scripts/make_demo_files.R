# ==============================================================================
# scripts/make_demo_files.R
#
# Purpose : The demo files in demo/ (Phase 4d), for showing the app:
#   scenario_A.xlsx       Scenario A: the default assortments and prices
#                         (upload under Scenario A, "Upload scenario").
#   scenario_B.xlsx       Scenario B: spruce sawlog min top diameter 14 cm
#                         instead of 12 (a rule change), and spruce sawlog
#                         prices +8 % for diameter classes >= 25 cm.
#   prices_A.xlsx,        The price matrices only (wide layout; "Upload price
#   prices_B.xlsx         matrix" in each column).
#   tree_list_example.csv Two plots, spruce and pine, some heights missing:
#                         15 of 26 spruce heights measured (fieldPlot), 4 of
#                         14 pine heights measured (FinnHoyde x ratio).
#
# Usage   : Rscript scripts/make_demo_files.R
#           Audit 24 uploads and runs every file.
# ==============================================================================

root_dir <- local({
  r <- Sys.getenv("APTER_ROOT", "")
  if (nzchar(r)) return(normalizePath(r, winslash = "/"))
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "R", "helpers_paths.R"))) return(d)
    p <- dirname(d); if (identical(p, d)) stop("Set APTER_ROOT to the project root.")
    d <- p
  }
})
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"))

out <- file.path(root_dir, "demo")
dir.create(out, showWarnings = FALSE)

# ---- Scenario A: the defaults ---------------------------------------------------
defs_a <- defs_from_products(load_products(root_dir))
pm_a   <- pm_minimal(load_price_matrix_long(root_dir))

# ---- Scenario B: one rule and some prices changed --------------------------------
defs_b <- defs_a
i <- assortment_table_keys(defs_b) == "spruce_sawlog"
defs_b$MinTopDiameter_cm[i] <- 14
pm_b <- pm_a
j <- pm_b$ProductKey == "spruce_sawlog" & !is.na(pm_b$PriceDiameterClass_cm) &
     pm_b$PriceDiameterClass_cm >= 25
pm_b$PriceNOKperM3[j] <- round(pm_b$PriceNOKperM3[j] * 1.08)

stopifnot(check_price_set(defs_a, pm_a)$ok, check_price_set(defs_b, pm_b)$ok)
scenario_export(defs_a, pm_a, file.path(out, "scenario_A.xlsx"))
scenario_export(defs_b, pm_b, file.path(out, "scenario_B.xlsx"))
pm_template_wide(pm_a, file.path(out, "prices_A.xlsx"))
pm_template_wide(pm_b, file.path(out, "prices_B.xlsx"))

# ---- Tree list --------------------------------------------------------------------
# Fixed values (no RNG). Heights near the FinnHoyde curves, a little taller on
# plot 1; NA = not measured.
dbh_sp <- c(14.2, 17.8, 21.5, 24.9, 27.3, 30.1, 33.6, 36.4, 19.7, 23.1, 28.8, 31.9, 38.2,
            15.6, 18.9, 22.4, 25.7, 29.4, 32.8, 35.1, 20.3, 26.2, 30.7, 34.4, 16.8, 40.5)
dbh_pi <- c(18.4, 22.9, 26.1, 29.8, 33.5, 21.2, 24.6, 27.9, 31.3, 35.8, 19.9, 28.4, 23.7, 37.1)
plot_sp <- rep(c("1", "2"), each = 13)
plot_pi <- rep(c("1", "2"), each = 7)
h <- function(sp, d, plot) round(finnhoyde_height(d, sp) * ifelse(plot == "1", 1.05, 0.95) +
                                 c(-0.4, 0.3, 0.1, -0.2, 0.5)[seq_along(d) %% 5 + 1], 1)
h_sp <- h("spruce", dbh_sp, plot_sp)
h_pi <- h("pine", dbh_pi, plot_pi)
h_sp[c(2, 5, 8, 11, 14, 16, 18, 20, 22, 24, 26)] <- NA    # 15 of 26 measured
h_pi[-c(1, 4, 9, 12)] <- NA                                 # 4 of 14 measured
tl <- data.frame(
  plot_id = c(plot_sp, plot_pi),
  tree_id = c(ave(seq_along(plot_sp), plot_sp, FUN = seq_along),
              13L + ave(seq_along(plot_pi), plot_pi, FUN = seq_along)),
  species = c(rep("spruce", length(dbh_sp)), rep("pine", length(dbh_pi))),
  dbh     = c(dbh_sp, dbh_pi),
  height  = c(h_sp, h_pi),
  stringsAsFactors = FALSE)
tl <- tl[order(tl$plot_id, tl$tree_id), ]
utils::write.csv(tl, file.path(out, "tree_list_example.csv"), row.names = FALSE, na = "")

cat("Demo files written to", out, "\n")
print(table(tl$plot_id, tl$species))
