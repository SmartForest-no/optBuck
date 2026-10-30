# ==============================================================================
# scripts/roadmap7_tune.R
#
# Purpose : Roadmap 7 (session 2): test runs R1-R10 of tune_prices()
#           (R/helpers_price_tuning.R) on the owner's .hpr files. Per run: tune
#           on a sample, then buck ALL stems of the file with the original and
#           with the best prices and compare.
#
# Privacy : LOCAL ONLY. The files are named file1..file5 (alphabetical in
#           C:/apter_private/hpr/); only product names, counts and numbers are
#           printed or written, to outputs/roadmap7/ (gitignored).
#
# Output  : outputs/roadmap7/tune_summary.csv, tune_trace_R<k>.csv,
#           fg_trace.png (FG on the sample against iteration, one line per run)
#
# Usage   : Rscript scripts/roadmap7_tune.R [folder] [runs, e.g. R2,R10,R8b]
#           (without runs: R1-R10 and the early_stop = FALSE runs R2b, R4b, R5b, R8b, R8c;
#           with runs, the summary and plot get the suffix _extra)
# ==============================================================================

args <- commandArgs(TRUE)
folder <- if (length(args) >= 1L) args[1L] else "C:/apter_private/hpr"
only   <- if (length(args) >= 2L) strsplit(args[2L], ",", fixed = TRUE)[[1L]] else NULL
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root)
suppressPackageStartupMessages(
  source(file.path(root, "scripts", "app_backend", "01_run_sampled_plot_bucking.R")))
source(file.path(root, "scripts", "roadmap7_helpers.R"))     # also sources R/helpers_price_tuning.R
out_dir <- file.path(root, "outputs", "roadmap7")
show <- make_show(out_dir)
options(width = 220)

files <- sort(list.files(folder, pattern = "\\.hpr$", full.names = TRUE, ignore.case = TRUE))
files <- files[order(basename(files), method = "radix")]
stopifnot(length(files) == 5L)

# run, file, species, product name pattern (regex on ProductName), target, n
runs <- data.frame(
  run = paste0("R", 1:10), file = c(5, 3, 4, 5, 5, 1, 1, 1, 3, 5),
  species = c(rep("spruce", 7), "pine", "pine", "spruce"),
  pattern = c("140.*sca", "140.*sca", "140.*sca", "140.*sca", "140.*sca", "sag - REDACTED37",
              "sag - REDACTED37", "sag - REDACTED48", "REDACTED23", "140.*sca"),
  target = c("real", "real", "real", "neutral", "long", "neutral", "long", "neutral", "neutral", "real"),
  n = c(rep(1000, 9), Inf), early_stop = TRUE, stringsAsFactors = FALSE)
# early_stop = FALSE: always 30 iterations (R2b, R4b, R5b, R8b = R2, R4, R5, R8 without the stop rule;
# R8c = R8b on all stems)
runs <- rbind(runs, within(runs[match(c("R2", "R4", "R5", "R8", "R8"), runs$run), ], {
  run <- c("R2b", "R4b", "R5b", "R8b", "R8c"); early_stop <- FALSE; n <- c(1000, 1000, 1000, 1000, Inf) }))
if (!is.null(only)) runs <- runs[runs$run %in% only, ]
suffix <- if (is.null(only)) "" else "_extra"
fg_at <- function(tr, k) if (k <= nrow(tr)) round(tr$fg[k], 2) else NA_real_

total_value <- function(res) sum(res$tree_summary$total_value_nok)
sawlog_volume <- function(res) { sl <- res$selected_logs_all
  sum(sl$price_volume_m3[as.character(sl$ProductType) == "sawlog"]) }
# Value of a run valued at the ORIGINAL prices: total - sum(V*P_run) + sum(V*P1) over the grid.
value_at_original <- function(res, tal, P_run, P1) { a <- !is.na(P1)
  total_value(res) - sum(tal$v[a] * P_run[a]) + sum(tal$v[a] * P1[a]) }

summary_rows <- list(); traces <- list()
for (fi in unique(runs$file)) {
  f <- files[fi]; fl <- sprintf("file%d", fi)
  cat(sprintf("\n######## %s ########\n", fl))
  hp <- read_hpr(f); stopifnot(isTRUE(hp$ok))
  smap_h <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap_h); stopifnot(isTRUE(sc$ok))
  pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
  products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
  dst <- read_hpr_distribution(f)
  keys <- assortment_table_keys(sc$defs)
  st <- read_hpr_stems(f); stopifnot(isTRUE(st$ok))
  smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
  rm(st); invisible(gc())
  cat(sprintf("%s: %d stems bucked\n", fl, length(job$pvs)))
  res0 <- finish_job(job, buck_job(job))                    # original prices, all stems
  cat(sprintf("%s: original run, total value %.0f NOK\n", fl, total_value(res0)))

  for (ri in which(runs$file == fi)) {
    R <- runs[ri, ]
    pd <- products$pd
    key <- pd$ProductKey[pd$species_group == R$species & pd$ProductType == "sawlog" &
                           grepl(R$pattern, pd$ProductName, ignore.case = TRUE)]
    if (length(key) != 1L) { cat(sprintf("%s: product pattern '%s' matched %d products\n", R$run, R$pattern, length(key))); next }
    hk <- sc$defs$HprProductKey[match(key, keys)]
    hpr <- hpr_tuning_info(dst, hk)
    cat(sprintf("\n-- %s: %s %s (%s), target %s, n = %s --\n", R$run, fl, pd$ProductName[match(key, pd$ProductKey)],
                key, R$target, R$n))
    t_run <- system.time(tu <- tune_prices(job, products, key, R$target, n = R$n, seed = 1, max_iter = 30, hpr = hpr, early_stop = R$early_stop))[["elapsed"]]
    print(round(tu$trace, 2), row.names = FALSE)
    g <- tu$grid; W <- tu$W; P1 <- tu$P_first; Pb <- tu$best_price; basis <- tu$settings$basis

    tal0 <- tally_grid(res0, key, g)
    job_t <- swap_job_prices(job, apply_grid_prices(products, g, Pb))
    res_t <- finish_job(job_t, buck_job(job_t))
    tal_t <- tally_grid(res_t, key, g)
    fg0 <- fg_grid(tal0, W, basis)$overall; fgt <- fg_grid(tal_t, W, basis)$overall
    v0 <- total_value(res0); vt <- value_at_original(res_t, tal_t, Pb, P1)
    a <- !is.na(P1)
    tr <- tu$trace
    summary_rows[[R$run]] <- data.frame(
      run = R$run, file = fl, product = pd$ProductName[match(key, pd$ProductKey)], target = R$target,
      basis = basis, n = if (is.finite(R$n)) R$n else tu$settings$stems,
      fg_sample_first = round(tr$fg[1L], 2), fg_sample_best = round(tr$fg[tu$best_iter], 2),
      fg_all_original = round(fg0, 2), fg_all_tuned = round(fgt, 2),
      best_iter = tu$best_iter, iterations = nrow(tr), early_stop = R$early_stop,
      fg_it5 = fg_at(tr, 5L), fg_it10 = fg_at(tr, 10L), fg_it20 = fg_at(tr, 20L), fg_it30 = fg_at(tr, 30L),
      product_logs_sample = tu$logs_first, product_logs_all = sum(tal0$n),
      eligible_classes = sum(tu$eligible), target_classes = sum(colSums(!is.na(tu$W)) > 0),
      value_orig_all = round(v0), value_tuned_at_orig_prices = round(vt), value_change_pct = round(100 * (vt - v0) / v0, 2),
      product_volume_orig = round(sum(tal0$v), 1), product_volume_tuned = round(sum(tal_t$v), 1),
      sawlog_volume_orig = round(sawlog_volume(res0), 1), sawlog_volume_tuned = round(sawlog_volume(res_t), 1),
      max_increase_nok = round(max(0, Pb[a] - P1[a]), 1), max_decrease_nok = round(max(0, P1[a] - Pb[a]), 1),
      floor_nok = tu$floor$value, floor_set_by = tu$floor$product, floor_set_by_price = tu$floor$price,
      steered_cells = tu$steered, steered_cells_at_floor_best = tu$cells_at_floor_best,
      runtime_s = round(t_run, 1))
    traces[[R$run]] <- tr
    utils::write.csv(cbind(run = R$run, tr), file.path(out_dir, sprintf("tune_trace_%s.csv", R$run)), row.names = FALSE)
    cat(sprintf("%s: FG sample %.2f -> %.2f, all stems %.2f -> %.2f, value at original prices %+.2f %%, %.0f s\n",
                R$run, tr$fg[1L], tr$fg[tu$best_iter], fg0, fgt, 100 * (vt - v0) / v0, t_run))
    rm(job_t, res_t); invisible(gc())
  }
  rm(job, res0); invisible(gc())
}

sm <- do.call(rbind, summary_rows); rownames(sm) <- NULL
show(sm, paste0("tune_summary", suffix), "Tuning runs")

# ---- plot ------------------------------------------------------------------------
grDevices::png(file.path(out_dir, paste0("fg_trace", suffix, ".png")), width = 1000, height = 650, res = 120)
yr <- range(unlist(lapply(traces, function(x) x$fg)), na.rm = TRUE)
xr <- c(1, max(vapply(traces, nrow, 0L)))
cols <- grDevices::hcl.colors(max(3L, length(traces)), "Dark 3")
plot(NA, xlim = xr, ylim = yr, xlab = "Iteration", ylab = "FG (sample)", las = 1)
for (i in seq_along(traces)) graphics::lines(traces[[i]]$r, traces[[i]]$fg, col = cols[i], lwd = 2)
graphics::legend("bottomright", legend = names(traces), col = cols[seq_along(traces)], lwd = 2, bty = "n", ncol = 2)
grDevices::dev.off()

# ---- expectations (reported, nothing is adjusted) -----------------------------------
flag <- sm[sm$fg_sample_best <= sm$fg_sample_first | sm$value_change_pct < -3, c("run", "fg_sample_first", "fg_sample_best", "value_change_pct")]
if (nrow(flag)) { cat("\nEXPECTATIONS NOT MET (FG did not rise, or value dropped more than 3 %):\n"); print(flag, row.names = FALSE)
} else cat("\nExpectations held: FG rose and the value change is within 3 % in every run.\n")
cat("\nDone.\n")
