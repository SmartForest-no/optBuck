# ==============================================================================
# scripts/roadmap7_timing.R
#
# Purpose : Roadmap 7 preparation (price-matrix tuning). Investigation and
#           timing only, with the app's own functions (no reimplemented
#           bucking): .hpr stems bucked with the products and prices imported
#           from the same .hpr file, stem-grade options off.
#             1. runtimes: warm-up, preprocessing, bucking all / 100 / 1000
#                stems (parallel and 1 worker), the price-swap reuse path
#             2. product classes of the import, sawlog logs per diameter class
#             3. initial fordelningsgrad (FG) for two synthetic targets
#
# Privacy : LOCAL ONLY. The .hpr file holds coordinates and names; only counts,
#           times and product definitions are printed or written. Output goes
#           to outputs/roadmap7/ (gitignored).
#
# Usage   : Rscript scripts/roadmap7_timing.R [file.hpr] [reps]
# ==============================================================================

args <- commandArgs(TRUE)
hpr_file <- if (length(args) >= 1L) args[1L] else "C:/apter_private/hpr/REDACTED28"
reps     <- if (length(args) >= 2L) as.integer(args[2L]) else 3L
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root)
suppressPackageStartupMessages(
  source(file.path(root, "scripts", "app_backend", "01_run_sampled_plot_bucking.R")))

out_dir <- file.path(root, "outputs", "roadmap7")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
show <- function(df, name, title = name) {
  cat("\n==== ", title, " ====\n", sep = "")
  print(df, row.names = FALSE, right = FALSE)
  utils::write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)
  invisible(df)
}
secs <- function(expr) { t0 <- proc.time()[["elapsed"]]; force(expr); proc.time()[["elapsed"]] - t0 }

# ---- script-only helpers -----------------------------------------------------------------

# FG of one diameter class: S, W in percent over the length classes.
fg_class <- function(S, W) 100 - 0.5 * sum(abs(S - W))

# Hand check (stop if wrong): 100 - 0.5 * (10+7+1+1+1) = 90.
stopifnot(isTRUE(all.equal(fg_class(c(30, 13, 19, 19, 19), rep(20, 5)), 90)))
cat("FG hand check passed: FG = 90\n")

# A job restricted to some stems (names of job$pvs); order and everything else kept.
sub_job <- function(job, ids) {
  keep <- names(job$pvs) %in% ids
  job$pvs <- job$pvs[keep]
  job$tree_data <- job$tree_data[as.character(job$tree_data$treid) %in% ids, , drop = FALSE]
  job$meta$trees <- job$tree_data
  job
}

# Rebuild only the runs (.load_runs) and the prepared runs (.prepare_runs) for a
# price matrix, as a price-tuning iteration would. Returns the job and the times.
swap_prices <- function(job, pm, pd, lc) {
  path <- tempfile(fileext = ".csv"); pm_export(pm, path)
  t_load <- secs(pps <- .load_runs(path, root, pd, lc, NULL))
  pps[[1L]]$grade_opts <- normalize_grade_opts(NULL)
  t_prep <- secs(prep <- .prepare_runs(lapply(pps, function(pp)
    list(pd = pp$pd, lc = pp$lc, lookup = pp$lookup, opts = pp$grade_opts))))
  job$pps <- pps; job$prep <- prep
  list(job = job, t_load = t_load, t_prep = t_prep)
}

# ---- 0. products and prices from the file ------------------------------------------------

t_import <- secs({
  hp <- read_hpr(hpr_file)
  stopifnot(isTRUE(hp$ok))
  smap_h <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap_h)
  stopifnot(isTRUE(sc$ok))
})
pm <- pm_minimal(sc$pm)
pr <- products_from_defs(sc$defs, pm)
price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
cat(sprintf("Product import (read_hpr + hpr_to_scenario): %.1f s, %d assortments\n",
            t_import, nrow(sc$defs)))

# ---- 1a. reading + cleaning + preprocessing, once --------------------------------------

workers <- apter_workers()
t_read <- secs(st <- read_hpr_stems(hpr_file))
stopifnot(isTRUE(st$ok))
smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
t_prep_all <- secs(job <- prepare_hpr_stems_job(
  st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc))
n_read <- job$meta$prepared$n_read; n_used <- length(job$pvs)
cat(sprintf("Stems read %s, bucked after cleaning %s\n", n_read, n_used))

# ---- samples (drawn once) -------------------------------------------------------------------

ids <- names(job$pvs)
set.seed(1)
s100  <- sample(ids, 100L)
s1000 <- sample(ids, 1000L)
job100 <- sub_job(job, s100); job1000 <- sub_job(job, s1000)

# ---- 1b. warm-up (worker start), then timings -------------------------------------------

t_warm <- secs(buck_job(job100, workers))
cat(sprintf("Warm-up bucking call (100 stems, %d workers, worker start): %.1f s\n", workers, t_warm))

bench <- function(label, j, w) {
  t <- vapply(seq_len(reps), function(i) secs(b <- buck_job(j, w)), 0)
  data.frame(step = label, stems = length(j$pvs), workers = w, reps = reps,
             median_s = round(stats::median(t), 2), min_s = round(min(t), 2), max_s = round(max(t), 2))
}
rt <- rbind(
  data.frame(step = c("warm-up bucking call (100 stems)", "import products (read_hpr + hpr_to_scenario)",
                      "(a1) read stems", "(a2) profiles + checks + job (incl. prices)"),
             stems = c(100L, NA, n_read, n_used), workers = c(workers, NA, NA, NA), reps = 1L,
             median_s = round(c(t_warm, t_import, t_read, t_prep_all), 2),
             min_s = NA_real_, max_s = NA_real_),
  bench("(b) buck all stems", job, workers),
  bench("(c) buck sample 100", job100, workers),
  bench("(c) buck sample 100", job100, 1L),
  bench("(d) buck sample 1000", job1000, workers),
  bench("(d) buck sample 1000", job1000, 1L))
show(rt, "runtimes", "Runtimes (elapsed seconds; median and range over reps)")

# Results with the original prices (used below).
b_all  <- buck_job(job, workers);     t_fin_all <- secs(res_all <- finish_job(job, b_all))
b_1000 <- buck_job(job1000, workers); t_fin_1k  <- secs(res_1000 <- finish_job(job1000, b_1000))
cat(sprintf("finish_job (result tables): all stems %.1f s, 1000 stems %.1f s\n", t_fin_all, t_fin_1k))

# ---- 1c. reuse path: change one sawlog product's prices, swap, buck ----------------------

keys <- assortment_table_keys(sc$defs)
saw_keys <- keys[sc$defs$Type == "sawlog"]
tal_all <- log_tally(res_all$selected_logs_all, pm)
nlog_by_key <- tapply(tal_all$logs, tal_all$ProductKey, sum)
main_key <- names(which.max(nlog_by_key[names(nlog_by_key) %in% saw_keys[grepl("^spruce_", saw_keys)]]))
pm_plus <- pm
rows <- pm_plus$ProductKey == main_key & !is.na(pm_plus$PriceNOKperM3)
pm_plus$PriceNOKperM3[rows] <- pm_plus$PriceNOKperM3[rows] + 10

sw1 <- swap_prices(job1000, pm_plus, pr$pd, pr$lc)
t_b_plus <- secs(b_plus <- buck_job(sw1$job, workers))
res_plus <- finish_job(sw1$job, b_plus)
sw2 <- swap_prices(job1000, pm, pr$pd, pr$lc)
t_b_back <- secs(b_back <- buck_job(sw2$job, workers))
res_back <- finish_job(sw2$job, b_back)

val <- function(r) sum(r$tree_summary$total_value_nok)
differs  <- !identical(res_plus$selected_logs_all, res_1000$selected_logs_all) ||
            !isTRUE(all.equal(val(res_plus), val(res_1000)))
restored <- identical(res_back$selected_logs_all, res_1000$selected_logs_all) &&
            identical(res_back$tree_summary, res_1000$tree_summary)
reuse <- data.frame(
  item = c("product changed (+10 NOK on every priced cell)", "rebuild pps (.load_runs), s", "rebuild prep (.prepare_runs), s",
           "buck_job 1000 stems, s", "total value original prices, NOK", "total value +10 NOK, NOK",
           "(a) result differs from original", "(b) restoring prices reproduces original exactly (identical)"),
  value = c(main_key, round(sw1$t_load, 2), round(sw1$t_prep, 2), round(t_b_plus, 2),
            round(val(res_1000), 1), round(val(res_plus), 1), differs, restored))
show(reuse, "reuse_path", "Reuse path: swap prices into the existing job")

# ---- 2. product classes -----------------------------------------------------------------

ptab <- hpr_product_table(hp, smap_h)
grp <- vapply(hp$products[ptab$key], function(p) or_default(p$group, NA_character_), "")
sgrp <- vapply(hp$products[ptab$key], function(p)
  paste(hp$species$name[match(p$species_keys, hp$species$key)], collapse = ", "), "")
cells_file <- vapply(hp$products[ptab$key], function(p) nrow(p$cells), 0L)
avail_file <- vapply(hp$products[ptab$key], function(p)
  sum(!is.na(p$cells$price) & !p$cells$criteria %in% HPR_UNAVAILABLE), 0L)
prod_file <- data.frame(species_group = sgrp, product = ptab$name, ProductGroupName = grp,
                        classified = ptab$type, imported_by_default = ptab$default,
                        length_classes_cm = ptab$lengths, diameter_classes_mm = ptab$diameters,
                        cells = cells_file, available_cells = avail_file)
show(prod_file, "products_in_file", "Products in the file (as classified by the import rule)")

lens <- assortment_lengths(sc$defs, pm)
grid_n <- function(k) { r <- pm[pm$ProductKey == k, ]; c(nrow(r), sum(!is.na(r$PriceNOKperM3))) }
gn <- t(vapply(keys, grid_n, c(0, 0)))
hk <- sc$defs$HprProductKey
prod_imp <- data.frame(
  key = keys, species = sc$defs$Species, name = sc$defs$Name,
  ProductGroupName = ifelse(is.na(hk), NA_character_, grp[match(hk, ptab$key)]),
  type = sc$defs$Type, price_type = sc$defs$PriceType,
  length_classes_cm = vapply(lens, function(l) paste(l, collapse = ","), ""),
  diameter_rows_cm = vapply(keys, function(k) {
    d <- pm$PriceDiameterClass_cm[pm$ProductKey == k]; d <- d[!is.na(d)]
    if (length(d)) sprintf("%d-%d (%d)", min(d), max(d), length(unique(d))) else "-" }, ""),
  cells = gn[, 1], available_cells = gn[, 2], row.names = NULL)
show(prod_imp, "products_imported", "Imported assortments (grid after import; diameter rows in cm)")

# Sawlog logs and volume per diameter class (all stems, original prices).
saw_rows <- list()
for (k in saw_keys) {
  g <- pm[pm$ProductKey == k & !is.na(pm$LengthClass_dm), ]
  classes <- sort(unique(g$PriceDiameterClass_cm[!is.na(g$PriceNOKperM3)]))
  if (!length(classes)) next
  tk <- tal_all[tal_all$ProductKey == k, ]
  n <- tapply(tk$logs, tk$diameter_class_cm, sum)[as.character(classes)]
  v <- tapply(tk$volume_m3, tk$diameter_class_cm, sum)[as.character(classes)]
  n[is.na(n)] <- 0; v[is.na(v)] <- 0
  saw_rows[[k]] <- data.frame(key = k, diameter_class_cm = classes, logs = as.integer(n),
                              commercial_volume_m3 = round(unname(v), 2))
}
saw_cls <- do.call(rbind, saw_rows); rownames(saw_cls) <- NULL
show(saw_cls, "sawlog_by_diameter_class", "Sawlog logs and commercial volume per diameter class")
saw_sum <- do.call(rbind, lapply(split(saw_cls, saw_cls$key), function(x)
  data.frame(key = x$key[1L], classes = nrow(x), logs = sum(x$logs),
             classes_lt20_logs = sum(x$logs < 20L), classes_zero_logs = sum(x$logs == 0L))))
show(saw_sum, "sawlog_summary", "Sawlog products: diameter classes with fewer than 20 logs")

pulp <- pm[pm$ProductKey %in% keys[sc$defs$Type == "pulpwood"] & !is.na(pm$PriceNOKperM3), ]
pk <- data.frame(key = keys, sg = sgrp[match(hk, ptab$key)], species = sc$defs$Species)
pulp$species_group <- pk$sg[match(pulp$ProductKey, pk$key)]
pulp_max <- stats::aggregate(PriceNOKperM3 ~ species_group, data = pulp, FUN = max)
names(pulp_max)[2L] <- "highest_pulpwood_price_NOK_m3"
show(pulp_max, "pulpwood_max_price", "Highest pulpwood price per species group")

# ---- 3. initial FG ---------------------------------------------------------------------------

# Counts [length x diameter] of a product's logs, availability and the two targets.
fg_for <- function(k, tal) {
  g <- pm[pm$ProductKey == k & !is.na(pm$LengthClass_dm), ]
  L <- sort(unique(g$LengthClass_dm)); D <- sort(unique(g$PriceDiameterClass_cm))
  avail <- matrix(FALSE, length(L), length(D), dimnames = list(L, D))
  ok <- g[!is.na(g$PriceNOKperM3), ]
  avail[cbind(match(ok$LengthClass_dm, L), match(ok$PriceDiameterClass_cm, D))] <- TRUE
  tk <- tal[tal$ProductKey == k, ]
  cnt <- matrix(0, length(L), length(D), dimnames = list(L, D))
  cnt[cbind(match(tk$length_class_dm, L), match(tk$diameter_class_cm, D))] <- tk$logs
  nj <- colSums(cnt)
  norm <- function(w) sweep(w, 2L, pmax(colSums(w), 1e-12), "/") * 100
  Wn <- norm(avail * 1)                           # neutral: equal share
  Wl <- norm(apply(avail, 2L, cumsum) * avail)    # long: 1, 2, ..., k by length
  S <- sweep(cnt, 2L, pmax(nj, 1e-12), "/") * 100
  fg <- function(W) vapply(seq_along(D), function(j) fg_class(S[, j], W[, j]), 0)
  list(L = L, D = D, N = nj, fg_neutral = fg(Wn), fg_long = fg(Wl))
}
overall <- function(f, which) { w <- f$N; ok <- w > 0
  if (!any(ok)) NA_real_ else sum(w[ok] * f[[which]][ok]) / sum(w[ok]) }

multi <- saw_keys[vapply(saw_keys, function(k)
  length(unique(pm$LengthClass_dm[pm$ProductKey == k & !is.na(pm$LengthClass_dm) &
                                  !is.na(pm$PriceNOKperM3)])) > 1L, NA)]
tal_1000 <- log_tally(res_1000$selected_logs_all, pm)
fg_rows <- lapply(multi, function(k) {
  fa <- fg_for(k, tal_all); fs <- fg_for(k, tal_1000)
  data.frame(key = k, logs_all = sum(fa$N), FG_neutral_all = round(overall(fa, "fg_neutral"), 1),
             FG_long_all = round(overall(fa, "fg_long"), 1), logs_1000 = sum(fs$N),
             FG_neutral_1000 = round(overall(fs, "fg_neutral"), 1),
             FG_long_1000 = round(overall(fs, "fg_long"), 1))
})
fg_tab <- do.call(rbind, fg_rows)
show(fg_tab, "fg_overall", "Initial FG per sawlog product (length classes > 1)")

main <- if (main_key %in% multi) main_key else multi[1L]
fm <- fg_for(main, tal_all)
fg_cls <- data.frame(diameter_class_cm = fm$D, logs = fm$N,
                     FG_neutral = round(fm$fg_neutral, 1), FG_long = round(fm$fg_long, 1))
fg_cls[fg_cls$logs == 0, c("FG_neutral", "FG_long")] <- NA
show(fg_cls, "fg_main_by_class", paste("FG per diameter class,", main, "(all stems)"))

cat("\nDone. CSV files in", out_dir, "\n")
