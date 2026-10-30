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
source(file.path(root, "scripts", "roadmap7_helpers.R"))   # shared helpers (hand check runs here)
show <- make_show(out_dir)

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
job100 <- subset_job_stems(job, match(s100, ids)); job1000 <- subset_job_stems(job, match(s1000, ids))

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

t_swap <- secs(job_plus <- swap_job_prices(job1000, list(pd = pr$pd, lc = pr$lc, pm = pm_plus)))
t_b_plus <- secs(b_plus <- buck_job(job_plus, workers))
res_plus <- finish_job(job_plus, b_plus)
job_back <- swap_job_prices(job1000, list(pd = pr$pd, lc = pr$lc, pm = pm))
t_b_back <- secs(b_back <- buck_job(job_back, workers))
res_back <- finish_job(job_back, b_back)

val <- function(r) sum(r$tree_summary$total_value_nok)
differs  <- !identical(res_plus$selected_logs_all, res_1000$selected_logs_all) ||
            !isTRUE(all.equal(val(res_plus), val(res_1000)))
restored <- identical(res_back$selected_logs_all, res_1000$selected_logs_all) &&
            identical(res_back$tree_summary, res_1000$tree_summary)
reuse <- data.frame(
  item = c("product changed (+10 NOK on every priced cell)", "swap_job_prices (pps + prep), s",
           "buck_job 1000 stems, s", "total value original prices, NOK", "total value +10 NOK, NOK",
           "(a) result differs from original", "(b) restoring prices reproduces original exactly (identical)"),
  value = c(main_key, round(t_swap, 2), round(t_b_plus, 2),
            round(val(res_1000), 1), round(val(res_plus), 1), differs, restored))
show(reuse, "reuse_path", "Reuse path: swap prices into the existing job")

# ---- 2. product classes -----------------------------------------------------------------

pt <- product_file_table(hp, smap_h)
ptab <- pt$ptab; grp <- pt$grp; sgrp <- pt$sgrp
show(pt$prod_file, "products_in_file", "Products in the file (as classified by the import rule)")

hk <- sc$defs$HprProductKey
show(product_imported_table(sc, pm, grp, ptab), "products_imported",
     "Imported assortments (grid after import; diameter rows in cm)")

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

multi <- saw_keys[vapply(saw_keys, function(k)
  length(unique(pm$LengthClass_dm[pm$ProductKey == k & !is.na(pm$LengthClass_dm) &
                                  !is.na(pm$PriceNOKperM3)])) > 1L, NA)]
tal_1000 <- log_tally(res_1000$selected_logs_all, pm)
fg_rows <- lapply(multi, function(k) {
  fa <- fg_for(k, tal_all, pm); fs <- fg_for(k, tal_1000, pm)
  data.frame(key = k, logs_all = sum(fa$N), FG_neutral_all = round(overall(fa, "fg_neutral"), 1),
             FG_long_all = round(overall(fa, "fg_long"), 1), logs_1000 = sum(fs$N),
             FG_neutral_1000 = round(overall(fs, "fg_neutral"), 1),
             FG_long_1000 = round(overall(fs, "fg_long"), 1))
})
fg_tab <- do.call(rbind, fg_rows)
show(fg_tab, "fg_overall", "Initial FG per sawlog product (length classes > 1)")

main <- if (main_key %in% multi) main_key else multi[1L]
fm <- fg_for(main, tal_all, pm)
fg_cls <- data.frame(diameter_class_cm = fm$D, logs = fm$N,
                     FG_neutral = round(fm$fg_neutral, 1), FG_long = round(fm$fg_long, 1))
fg_cls[fg_cls$logs == 0, c("FG_neutral", "FG_long")] <- NA
show(fg_cls, "fg_main_by_class", paste("FG per diameter class,", main, "(all stems)"))

cat("\nDone. CSV files in", out_dir, "\n")
