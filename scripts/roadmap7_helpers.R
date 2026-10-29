# ==============================================================================
# scripts/roadmap7_helpers.R
#
# Purpose : Helpers shared by scripts/roadmap7_timing.R and
#           scripts/roadmap7_targets.R (roadmap 7 preparation, investigation
#           only): output, timing, job subsets and price swaps, product
#           tables, fordelningsgrad (FG) and the distribution targets.
#           Source it after the backend (scripts/app_backend/...) is loaded.
#           The functions take their data as arguments (no script globals).
# ==============================================================================

# ---- output and timing -------------------------------------------------------------------

# A printer that also writes a CSV to out_dir.
make_show <- function(out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  function(df, name, title = name) {
    cat("\n==== ", title, " ====\n", sep = "")
    print(df, row.names = FALSE, right = FALSE)
    utils::write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)
    invisible(df)
  }
}
secs <- function(expr) { t0 <- proc.time()[["elapsed"]]; force(expr); proc.time()[["elapsed"]] - t0 }

# ---- FG ----------------------------------------------------------------------------------------

# FG of one diameter class: S, W in percent over the length classes.
fg_class <- function(S, W) 100 - 0.5 * sum(abs(S - W))

# Hand check (stop if wrong): 100 - 0.5 * (10+7+1+1+1) = 90.
stopifnot(isTRUE(all.equal(fg_class(c(30, 13, 19, 19, 19), rep(20, 5)), 90)))
cat("FG hand check passed: FG = 90\n")

# Overall FG: classes weighted by their logs N_j (classes without logs: weight 0).
overall <- function(f, which) { w <- f$N; ok <- w > 0
  if (!any(ok)) NA_real_ else sum(w[ok] * f[[which]][ok]) / sum(w[ok]) }

# Counts [length x diameter] of a product's logs on the Apter price grid, the
# availability of its cells, and FG per diameter class for the neutral and the
# long target.
fg_for <- function(k, tal, pm) {
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

# Real target: the Distribution values of the available cells, normalised to
# 100 % within each diameter class (columns). A class whose values sum to 0
# has no target: its column is NA.
#   dist, avail  matrices [length x diameter] of the same shape.
real_target <- function(dist, avail) {
  w <- dist * avail; w[is.na(w)] <- 0
  s <- colSums(w)
  out <- sweep(w, 2L, ifelse(s > 0, s, NA_real_), "/") * 100
  out
}

# The Apter diameter row (whole cm) -> the file's diameter class (lower limit
# mm): the largest class <= c * 10 mm, as the import maps prices (the smallest
# class below it).
file_class_of_cm <- function(cm, d_file) {
  d_file <- sort(unique(d_file))
  vapply(cm, function(c) { u <- d_file[d_file <= c * 10]; if (length(u)) max(u) else min(d_file) }, 0)
}

# FG against the file's own Distribution target. `cells` = the product's cells
# (d_mm, l_cm, avail, dist); the logs of the Apter grid (tally rows of key k)
# are assigned to the file's diameter classes (file_class_of_cm) and length
# classes (ceiling(l_cm / 10) dm). Returns list(D, L, N, fg, S, W, dist, avail)
# over the file's classes; classes without a target are left out of the FG.
fg_real <- function(k, tal, cells) {
  Dm <- sort(unique(cells$d_mm)); Lc <- sort(unique(cells$l_cm))
  Ldm <- ceiling(Lc / 10)
  mk <- function(v, fill) { m <- matrix(fill, length(Lc), length(Dm), dimnames = list(Lc, Dm))
                            m[cbind(match(cells$l_cm, Lc), match(cells$d_mm, Dm))] <- v; m }
  dist <- mk(cells$dist, 0); avail <- mk(cells$avail, FALSE) == 1
  W <- real_target(dist, avail)
  tk <- tal[tal$ProductKey == k, ]
  cls <- file_class_of_cm(tk$diameter_class_cm, Dm)
  cnt <- matrix(0, length(Lc), length(Dm), dimnames = list(Lc, Dm))
  for (i in seq_len(nrow(tk))) {
    r <- match(tk$length_class_dm[i], Ldm); cc <- match(cls[i], Dm)
    if (!is.na(r) && !is.na(cc)) cnt[r, cc] <- cnt[r, cc] + tk$logs[i]
  }
  nj <- colSums(cnt)
  S <- sweep(cnt, 2L, pmax(nj, 1e-12), "/") * 100
  has <- !is.na(colSums(W))
  fg <- ifelse(has, vapply(seq_along(Dm), function(j)
    if (has[j]) fg_class(S[, j], W[, j]) else NA_real_, 0), NA_real_)
  nj_w <- ifelse(has, nj, 0)
  # The neutral and long targets on the same file grid (comparable with the real one).
  norm <- function(w) sweep(w, 2L, pmax(colSums(w), 1e-12), "/") * 100
  Wn <- norm(avail * 1); Wl <- norm(apply(avail, 2L, cumsum) * avail)
  fgw <- function(W) vapply(seq_along(Dm), function(j) fg_class(S[, j], W[, j]), 0)
  list(D = Dm, L = Lc, N = nj, N_target = nj_w, fg = fg, fg_neutral = fgw(Wn), fg_long = fgw(Wl),
       S = S, W = W, dist = dist, avail = avail)
}
# Overall FG of a fg_real() result: "fg" (real target; classes without target
# left out), "fg_neutral" or "fg_long" (classes with logs).
overall_real <- function(f, which = "fg") {
  w <- if (which == "fg") f$N_target else f$N
  ok <- w > 0 & !is.na(f[[which]])
  if (!any(ok)) NA_real_ else sum(w[ok] * f[[which]][ok]) / sum(w[ok]) }

# ---- jobs --------------------------------------------------------------------------------------

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
swap_prices <- function(job, pm, pd, lc, root) {
  path <- tempfile(fileext = ".csv"); pm_export(pm, path)
  t_load <- secs(pps <- .load_runs(path, root, pd, lc, NULL))
  pps[[1L]]$grade_opts <- normalize_grade_opts(NULL)
  t_prep <- secs(prep <- .prepare_runs(lapply(pps, function(pp)
    list(pd = pp$pd, lc = pp$lc, lookup = pp$lookup, opts = pp$grade_opts))))
  job$pps <- pps; job$prep <- prep
  list(job = job, t_load = t_load, t_prep = t_prep)
}

# ---- product tables ----------------------------------------------------------------------

# The file's products as classified by the import rule (ProductGroupName next
# to the classification). Returns list(ptab, grp, sgrp, prod_file).
product_file_table <- function(hp, smap_h) {
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
  list(ptab = ptab, grp = grp, sgrp = sgrp, prod_file = prod_file)
}

# The imported assortments with their grid after import.
product_imported_table <- function(sc, pm, grp, ptab) {
  keys <- assortment_table_keys(sc$defs)
  lens <- assortment_lengths(sc$defs, pm)
  grid_n <- function(k) { r <- pm[pm$ProductKey == k, ]; c(nrow(r), sum(!is.na(r$PriceNOKperM3))) }
  gn <- t(vapply(keys, grid_n, c(0, 0)))
  hk <- sc$defs$HprProductKey
  data.frame(
    key = keys, species = sc$defs$Species, name = sc$defs$Name,
    ProductGroupName = ifelse(is.na(hk), NA_character_, grp[match(hk, ptab$key)]),
    type = sc$defs$Type, price_type = sc$defs$PriceType,
    length_classes_cm = vapply(lens, function(l) paste(l, collapse = ","), ""),
    diameter_rows_cm = vapply(keys, function(k) {
      d <- pm$PriceDiameterClass_cm[pm$ProductKey == k]; d <- d[!is.na(d)]
      if (length(d)) sprintf("%d-%d (%d)", min(d), max(d), length(unique(d))) else "-" }, ""),
    cells = gn[, 1], available_cells = gn[, 2], row.names = NULL)
}

# ---- Distribution (read from the XML, not read by the import) ----------------------------

# Per product of a .hpr: DistributionAllowed / Category / WithinProduct /
# MAXDeviation and the cells of its ProductMatrixes (d_mm, l_cm, price,
# criteria, dist). Only the definitions (the bytes before the first stem) are
# parsed. Returns list(products (data.frame), cells (list by ProductKey)).
read_hpr_distribution <- function(path) {
  pre <- .hpr_prefix(path)
  doc <- xml2::read_xml(.hpr_close_tags(pre$bytes))
  xml2::xml_ns_strip(doc)
  prods <- xml2::xml_find_all(doc, "//ProductDefinition[ClassifiedProductDefinition]")
  txt <- function(n, xp) { x <- xml2::xml_find_first(n, xp); if (inherits(x, "xml_missing")) NA_character_ else trimws(xml2::xml_text(x)) }
  info <- list(); cells <- list()
  for (p in prods) {
    cp <- xml2::xml_find_first(p, "./ClassifiedProductDefinition")
    key <- txt(p, "./ProductKey")
    ld <- "./LengthDistributionDefinition"
    items <- xml2::xml_find_all(cp, "./ProductMatrixes/ProductMatrixItem")
    num <- function(x) suppressWarnings(as.numeric(x))
    cells[[key]] <- data.frame(
      d_mm = num(xml2::xml_attr(items, "diameterClassLowerLimit")),
      l_cm = num(xml2::xml_attr(items, "lengthClassLowerLimit")),
      price = num(xml2::xml_text(xml2::xml_find_first(items, "./Price"))),
      criteria = trimws(xml2::xml_text(xml2::xml_find_first(items, "./BuckingCriteria"))),
      dist = num(xml2::xml_text(xml2::xml_find_first(items, "./Distribution"))),
      stringsAsFactors = FALSE)
    cells[[key]]$avail <- !is.na(cells[[key]]$price) & !cells[[key]]$criteria %in% HPR_UNAVAILABLE
    info[[key]] <- data.frame(
      key = key, product = txt(cp, "./ProductName"), group = txt(cp, "./ProductGroupName"),
      DistributionAllowed = txt(cp, paste0(ld, "/DistributionAllowed")),
      DistributionCategory = txt(cp, paste0(ld, "/DistributionCategory")),
      DistributionWithinProduct = txt(cp, paste0(ld, "/DistributionWithinProduct")),
      MAXDeviation = num(txt(cp, paste0(ld, "/MAXDeviation"))), stringsAsFactors = FALSE)
  }
  list(products = do.call(rbind, info), cells = cells)
}

# What a product's Distribution values add up to.
distribution_summary <- function(cells, tol = 1e-6) {
  dist <- cells$dist; dist[is.na(dist)] <- 0
  total <- sum(dist)
  by_d <- tapply(dist, cells$d_mm, sum)                    # diameter class order
  nz <- by_d[by_d > 0]
  concl <- if (total == 0) "all zero"
    else if (all(abs(nz - 100) < tol)) "sums to 100 per diameter class"
    else if (abs(total - 100) < tol || abs(total - 1000) < tol) "sums to 100 (or 1000) over the whole matrix"
    else "other"
  list(total = total, by_d = by_d, n_zero_classes = sum(by_d == 0),
       avail_dist0 = sum(cells$avail & dist == 0),
       unavail_dist_pos = sum(!cells$avail & dist > 0),
       conclusion = concl)
}
