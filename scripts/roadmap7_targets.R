# ==============================================================================
# scripts/roadmap7_targets.R
#
# Purpose : Roadmap 7 preparation (investigation only): all .hpr files in a
#           folder. Per file: stems and runtimes (products and prices imported
#           from the file itself, stem-grade options off), sawlog products with
#           their logs and diameter classes; the meaning of the Distribution
#           values (read from the XML: the import does not read them); the
#           file's own target against the neutral and long targets (FG).
#
# Privacy : LOCAL ONLY. Files are named file1..file5 (alphabetical) plus the
#           file name; only product names, counts and numbers are printed.
#
# Usage   : Rscript scripts/roadmap7_targets.R [folder] [workers]
# ==============================================================================

args <- commandArgs(TRUE)
folder  <- if (length(args) >= 1L) args[1L] else "C:/apter_private/hpr"
workers <- if (length(args) >= 2L) as.integer(args[2L]) else NULL
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root)
suppressPackageStartupMessages(
  source(file.path(root, "scripts", "app_backend", "01_run_sampled_plot_bucking.R")))
source(file.path(root, "scripts", "roadmap7_helpers.R"))
show <- make_show(file.path(root, "outputs", "roadmap7"))
if (is.null(workers)) workers <- apter_workers()
options(width = 250)

files <- sort(list.files(folder, pattern = "\\.hpr$", full.names = TRUE, ignore.case = TRUE))
files <- files[order(basename(files), method = "radix")]
cat(sprintf("%d files, %d workers\n", length(files), workers))
for (i in seq_along(files)) cat(sprintf("  file%d = %s\n", i, basename(files[i])))

F_tab <- list(); S_tab <- list(); D_tab <- list(); M_long <- list(); G_tab <- list()

for (i in seq_along(files)) {
  f <- files[i]; fl <- sprintf("file%d", i)
  cat(sprintf("\n######## %s (%s) ########\n", fl, basename(f)))

  # ---- products, prices and the Distribution values --------------------------------------
  hp <- read_hpr(f)
  if (!isTRUE(hp$ok)) { cat("  read_hpr failed:", paste(hp$errors, collapse = " "), "\n"); next }
  smap_h <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap_h)
  dst <- read_hpr_distribution(f)
  pt <- product_file_table(hp, smap_h)
  cls <- setNames(pt$ptab$type, pt$ptab$key)        # the import's sawlog / pulpwood rule

  # ---- stems and bucking --------------------------------------------------------------------
  have_stems <- FALSE; res <- NULL; tal <- NULL; pm <- NULL; row <- list(
    file = fl, name = basename(f), stems_read = NA, stems_used = NA, read_s = NA, prepare_s = NA, buck_s = NA,
    note = "")
  if (!isTRUE(sc$ok)) {
    row$note <- paste("scenario import failed:", paste(sc$errors, collapse = " "))
  } else {
    pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
    price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
    row$read_s <- round(secs(st <- read_hpr_stems(f)), 1)
    if (!isTRUE(st$ok)) {
      row$note <- paste("no stem profiles:", paste(st$errors, collapse = " "))
    } else {
      smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
      row$prepare_s <- round(secs(job <- prepare_hpr_stems_job(
        st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)), 1)
      row$stems_read <- job$meta$prepared$n_read; row$stems_used <- length(job$pvs)
      rm(st); invisible(gc())
      set.seed(1)
      buck_job(sub_job(job, sample(names(job$pvs), min(100L, length(job$pvs)))), workers)   # warm-up
      row$buck_s <- round(secs(b <- buck_job(job, workers)), 1)
      res <- finish_job(job, b); tal <- log_tally(res$selected_logs_all, pm)
      have_stems <- TRUE
      rm(job, b); invisible(gc())
    }
  }
  F_tab[[fl]] <- as.data.frame(row, stringsAsFactors = FALSE)

  # ---- sawlog products: logs and classes -----------------------------------------------------
  keys <- if (isTRUE(sc$ok)) assortment_table_keys(sc$defs) else character(0)
  hk <- if (isTRUE(sc$ok)) sc$defs$HprProductKey else character(0)
  saw_keys <- keys[sc$defs$Type[seq_along(keys)] == "sawlog"]
  n_len <- function(k) length(unique(pm$LengthClass_dm[pm$ProductKey == k & !is.na(pm$LengthClass_dm) &
                                                          !is.na(pm$PriceNOKperM3)]))
  n_dfile <- function(k) { x <- dst$cells[[hk[match(k, keys)]]]; length(unique(x$d_mm[x$avail])) }
  if (have_stems) for (k in saw_keys) {
    g <- pm[pm$ProductKey == k & !is.na(pm$LengthClass_dm) & !is.na(pm$PriceNOKperM3), ]
    tk <- tal[tal$ProductKey == k, ]
    rows_cm <- sort(unique(g$PriceDiameterClass_cm))
    n_cm <- tapply(tk$logs, tk$diameter_class_cm, sum)[as.character(rows_cm)]; n_cm[is.na(n_cm)] <- 0
    fr <- fg_real(k, tal, within(dst$cells[[hk[match(k, keys)]]], dist[is.na(dist)] <- 0))
    S_tab[[paste(fl, k)]] <- data.frame(
      file = fl, product = k, logs = sum(tk$logs), length_classes = n_len(k),
      diameter_classes_cm = length(rows_cm), cm_classes_ge20_logs = sum(n_cm >= 20),
      diameter_classes_file = length(fr$D), file_classes_ge20_logs = sum(fr$N >= 20))
  }

  # ---- Distribution semantics per product ---------------------------------------------------
  for (k in dst$products$key) {
    p <- dst$products[dst$products$key == k, ]; cl <- dst$cells[[k]]
    s <- distribution_summary(cl)
    D_tab[[paste(fl, k)]] <- data.frame(
      file = fl, key = k, product = p$product, class = unname(cls[k]),
      DistributionAllowed = p$DistributionAllowed, Category = p$DistributionCategory,
      WithinProduct = p$DistributionWithinProduct, MAXDeviation = p$MAXDeviation,
      total = round(s$total, 3), diameter_classes = length(s$by_d),
      per_class_sums = paste(round(s$by_d, 2), collapse = " "), classes_mm = paste(names(s$by_d), collapse = " "),
      zero_classes = s$n_zero_classes, avail_cells = sum(cl$avail), avail_dist0 = s$avail_dist0,
      unavail_dist_pos = s$unavail_dist_pos, conclusion = s$conclusion)
  }

  # ---- the main sawlog product: Distribution, prices, achieved share ---------------------------
  main_hk <- NA_character_
  if (isTRUE(sc$ok) && length(saw_keys)) {
    if (have_stems) {
      nl <- vapply(saw_keys, function(k) sum(tal$logs[tal$ProductKey == k]), 0)
      main_k <- saw_keys[which.max(nl)]
    } else {
      main_k <- saw_keys[which.max(vapply(saw_keys, function(k) sum(pm$ProductKey == k & !is.na(pm$PriceNOKperM3)), 0))]
    }
    main_hk <- hk[match(main_k, keys)]
    cl <- dst$cells[[main_hk]]; cl$dist[is.na(cl$dist)] <- 0
    Dm <- sort(unique(cl$d_mm)); Lc <- sort(unique(cl$l_cm))
    mk <- function(v) { m <- matrix(NA_real_, length(Lc), length(Dm), dimnames = list(`length_cm` = Lc, `diam_mm` = Dm))
                        m[cbind(match(cl$l_cm, Lc), match(cl$d_mm, Dm))] <- v; m }
    mD <- mk(cl$dist); mP <- mk(ifelse(cl$avail, cl$price, NA_real_))
    cat(sprintf("\n-- %s: main sawlog product %s (%s)%s --\n", fl, main_k,
                dst$products$product[dst$products$key == main_hk],
                if (have_stems) sprintf(", %s logs", format(sum(tal$logs[tal$ProductKey == main_k]), big.mark = ",")) else ", no stems"))
    cat("Distribution (rows length cm, columns diameter class mm):\n"); print(mD)
    cat("Price (NOK/m3, NA = unavailable):\n"); print(mP)
    if (have_stems) {
      fr <- fg_real(main_k, tal, within(cl, avail <- avail))
      cat("Achieved share (% of the logs in the diameter class, original prices; 0 = no logs):\n")
      print(round(fr$S, 1)); cat("logs per diameter class:", paste(fr$N, collapse = " "), "\n")
      mS <- fr$S
    } else mS <- NULL
    M_long[[fl]] <- data.frame(file = fl, product = main_k, d_mm = cl$d_mm, l_cm = cl$l_cm,
      price = ifelse(cl$avail, cl$price, NA), distribution = cl$dist,
      share = if (!is.null(mS)) mS[cbind(match(cl$l_cm, Lc), match(cl$d_mm, Dm))] else NA_real_)
  }

  # ---- FG, three targets ---------------------------------------------------------------------
  if (have_stems) for (k in saw_keys) {
    h <- hk[match(k, keys)]; cl <- dst$cells[[h]]; p <- dst$products[dst$products$key == h, ]
    tk <- tal[tal$ProductKey == k, ]
    if (sum(tk$logs) < 100 || n_len(k) < 2L || n_dfile(k) < 2L) next
    fr <- fg_real(k, tal, cl)
    s <- distribution_summary(cl)
    real <- if (identical(tolower(p$DistributionAllowed), "false") || s$total == 0) "no real target"
            else sprintf("%.1f", overall_real(fr, "fg"))
    G_tab[[paste(fl, k)]] <- data.frame(
      file = fl, product = k, logs = sum(tk$logs), MAXDeviation = p$MAXDeviation,
      DistributionAllowed = p$DistributionAllowed,
      FG_real = real, FG_neutral = round(overall_real(fr, "fg_neutral"), 1),
      FG_long = round(overall_real(fr, "fg_long"), 1))
  }
  rm(res, tal); invisible(gc())
}

show(do.call(rbind, F_tab), "targets_files", "Files: stems and runtimes (bucking: all stems, one run after a warm-up call)")
show(do.call(rbind, S_tab), "targets_sawlog_products", "Sawlog products: logs and diameter classes (original prices)")
show(do.call(rbind, D_tab), "targets_distribution", "Distribution semantics per product (read from the XML)")
show(do.call(rbind, M_long), "targets_main_matrices", "Main sawlog product per file: price, Distribution, achieved share (long)")
show(do.call(rbind, G_tab), "targets_fg", "Initial FG (original prices), sawlog products with >= 100 logs, > 1 length and diameter class")
cat("\nDone.\n")
