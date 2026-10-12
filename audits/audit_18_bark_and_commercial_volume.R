# ==============================================================================
# audits/audit_18_bark_and_commercial_volume.R
#
# Purpose : Audit of the bark model (R/helpers_bark.R) and the NOK/m3
#           denominator, commercial volume under bark
#           (R/helpers_commercial_volume.R), added in Phase 3 (2026-10-12).
#
# Checks  :
#  1.  Bark model matches taperNOR: for spruce, under_bark_diameter() equals
#      taperNOR(with_bark = FALSE) along the stem (<= 1e-9 mm) where B > 0
#  2.  Floor: for a 45 cm / 25 m pine, double bark >= 0 and d_ub <= d_ob at
#      every height; the number of floored points is reported (the B >= 0
#      floor is PROVISIONAL, pending the taperNOR authors)
#  3.  Swappable: a registered constant-10 mm test model selected with
#      options(apter.bark_model) gives d_ub = d_ob - 10 mm; option restored
#  4.  dbh_from_profile() on a taperNOR profile equals taperNOR(1.3)
#  5.  Synthetic stems, checked by hand:
#        a. cylinder d_ub 300 mm, one 4 m sawlog -> pi * 0.15^2 * 4 m3
#        b. linear cone, pulpwood log -> explicit sum of top-diameter cylinders
#        c. sawlog + pulpwood + waste + unbucked top -> only the two
#           non-waste logs count
#        d. log starting at 10 cm -> the stump section [0, 10) is excluded
#  6.  Waste and top excluded in real backend runs (spruce and pine): waste
#      logs carry 0; per tree, the commercial volume equals a recomputation
#      from a fresh taper profile over the non-waste logs only, and never
#      exceeds the stem volume up to the last non-waste log
#  7.  Denominator unaffected by prices: a run with all prices x1.5 gives, per
#      tree, the same cutting pattern (strict since Phase 4a's deterministic
#      tie-breaking), the same commercial volume and value x1.5, and NOK/m3
#      x1.5; on a fixed cutting
#      pattern the helper result is identical under two price matrices
#  8.  Price on under bark: for every sawlog, the price-diameter class is
#      floor(TopDiameter_ub / 10) and TopDiameter_ub <= TopDiameter_ob
#      (<=, not <: with the B >= 0 floor a pine top can have zero bark)
#  9.  Total NOK/m3 equals total value / total commercial volume
# 10.  For every pulpwood log, price volume equals commercial volume
#      (tolerance 1e-9 m3)
#
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/commercial_volume/audit_18_bark_and_commercial_volume.csv
#   outputs/commercial_volume/audit_18_bark_and_commercial_volume.txt
#
# Usage   : Rscript audits/audit_18_bark_and_commercial_volume.R
# ==============================================================================

suppressPackageStartupMessages(library(readr))

root_dir <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
# The backend also sources helpers_bark.R and helpers_commercial_volume.R.
source(file.path(root_dir, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)

out_dir <- path_output("commercial_volume", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_18_bark_and_commercial_volume.txt")
audit_csv <- file.path(out_dir, "audit_18_bark_and_commercial_volume.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; REVIEW <- "REVIEW"; FAIL <- "FAIL"

section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}

cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: bark model and commercial volume under bark\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

grid_profile <- function(dbh_cm, h_m, sp) {
  z <- seq(0.1, h_m, by = 0.1)
  list(pos = as.integer(round(z * 100)),
       ob  = taperNOR::taperNOR(z, dbh_cm, h_m, sp, with_bark = TRUE) * 10,
       ub  = taperNOR::taperNOR(z, dbh_cm, h_m, sp, with_bark = FALSE) * 10)
}


# ==============================================================================
# Check 01: bark model matches taperNOR (spruce)
# ==============================================================================
section("01. Bark model matches taperNOR(with_bark = FALSE), spruce")

dev1 <- 0; n1 <- 0L
for (tr in list(c(15, 14), c(30, 25), c(45, 30))) {
  g  <- grid_profile(tr[1], tr[2], "spruce")
  b  <- bark_model_barkNOR(g$ob / 10, tr[1], "spruce")
  ok <- b > 0
  mine <- under_bark_diameter(g$ob, tr[1] * 10, "spruce")
  dev1 <- max(dev1, abs(mine[ok] - g$ub[ok]))
  n1   <- n1 + sum(ok)
}
rec("Check 01: under_bark_diameter() equals taperNOR(with_bark = FALSE) for spruce",
    if (dev1 <= 1e-9) PASS else FAIL,
    sprintf("%d heights over 3 trees; max |diff| = %.2e mm", n1, dev1))


# ==============================================================================
# Check 02: B >= 0 floor (pine top)
# ==============================================================================
section("02. B >= 0 floor, large pine")

g2     <- grid_profile(45, 25, "pine")
b_raw  <- bark_model_barkNOR(g2$ob / 10, 45, "pine")
b_used <- double_bark_mm(g2$ob, 450, "pine")
ub2    <- under_bark_diameter(g2$ob, 450, "pine")
n_floored <- sum(b_raw < 0)
ok2 <- all(b_used >= 0) && all(ub2 <= g2$ob + 1e-12)
rec("Check 02: Double bark >= 0 and d_ub <= d_ob everywhere (pine 45 cm / 25 m)",
    if (ok2) PASS else FAIL,
    sprintf("%d of %d heights floored (raw barkNOR < 0, min %.1f mm, from %.1f m up); floor is provisional",
            n_floored, length(b_raw), min(b_raw),
            if (n_floored > 0) min(g2$pos[b_raw < 0]) / 100 else NA_real_))


# ==============================================================================
# Check 03: swappable bark model
# ==============================================================================
section("03. Swappable bark model")

old_opt <- getOption("apter.bark_model")
BARK_MODELS$audit18_const10 <- function(d_cm, dbh_cm, species_group) rep(10, length(d_cm))
options(apter.bark_model = "audit18_const10")
ub3 <- under_bark_diameter(g2$ob, 450, "pine")
active3 <- identical(get_bark_model(), BARK_MODELS$audit18_const10)
options(apter.bark_model = old_opt)
BARK_MODELS$audit18_const10 <- NULL
big <- g2$ob > 10
dev3 <- max(abs(ub3[big] - (g2$ob[big] - 10)))
rec("Check 03: Registered model via options(apter.bark_model) is used",
    if (active3 && dev3 <= 1e-12 &&
        identical(get_bark_model(), BARK_MODELS$barkNOR)) PASS else FAIL,
    sprintf("test model active: %s | max |d_ub - (d_ob - 10)| = %.2e mm | restored to barkNOR: %s",
            active3, dev3, identical(get_bark_model(), BARK_MODELS$barkNOR)))


# ==============================================================================
# Check 04: dbh read from the profile at 1.3 m
# ==============================================================================
section("04. dbh_from_profile()")

g4  <- grid_profile(30, 25, "pine")
d13 <- dbh_from_profile(g4$pos, g4$ob)
ref <- taperNOR::taperNOR(1.3, 30, 25, "pine") * 10
rec("Check 04: dbh_from_profile() equals taperNOR(1.3)",
    if (abs(d13 - ref) <= 1e-9) PASS else FAIL,
    sprintf("profile %.4f mm | taperNOR(1.3) %.4f mm", d13, ref))


# ==============================================================================
# Check 05: synthetic stems
# ==============================================================================
section("05. Synthetic stems")

hand_cyl_sum <- function(d_fun, start, stop) {      # explicit, independent loop
  v <- 0
  for (top in seq(start + 10, stop, by = 10)) v <- v + pi * (d_fun(top) / 2000)^2 * 0.1
  v
}
pos <- seq(0L, 2000L, by = 10L)

# a. cylinder
cyl <- rep(300, length(pos))
va  <- commercial_volume_ub(pos, cyl, data.frame(StartPos = 10, StopPos = 410,
                                                 ProductType = "sawlog"))
rec("Check 05a: Cylinder d 300 mm, 4 m sawlog = pi * 0.15^2 * 4",
    if (abs(va - pi * 0.15^2 * 4) <= 1e-12) PASS else FAIL,
    sprintf("got %.10f m3, expected %.10f m3", va, pi * 0.15^2 * 4))

# b. cone
cone_f <- function(z) 400 - 0.2 * z
vb  <- commercial_volume_ub(pos, cone_f(pos), data.frame(StartPos = 100, StopPos = 600,
                                                         ProductType = "pulpwood"))
eb  <- hand_cyl_sum(cone_f, 100, 600)
rec("Check 05b: Linear cone = sum of top-diameter cylinders",
    if (abs(vb - eb) <= 1e-12) PASS else FAIL,
    sprintf("got %.10f m3, hand %.10f m3", vb, eb))

# c. sawlog + pulpwood + waste + unbucked top
logs_c <- data.frame(StartPos = c(10, 410, 710), StopPos = c(410, 710, 760),
                     ProductType = c("sawlog", "pulpwood", "waste"))
vc <- commercial_volume_ub(pos, cone_f(pos), logs_c)
ec <- hand_cyl_sum(cone_f, 10, 410) + hand_cyl_sum(cone_f, 410, 710)
rec("Check 05c: Waste log and unbucked top contribute nothing",
    if (abs(sum(vc) - ec) <= 1e-12 && vc[3] == 0) PASS else FAIL,
    sprintf("per log %s m3 | total %.10f, hand (non-waste only) %.10f",
            paste(sprintf("%.6f", vc), collapse = " / "), sum(vc), ec))

# d. stump
vd <- commercial_volume_ub(pos, cyl, data.frame(StartPos = 10, StopPos = 110,
                                                ProductType = "pulpwood"))
rec("Check 05d: Log from 10 cm excludes the stump section",
    if (abs(vd - pi * 0.15^2 * 1) <= 1e-12) PASS else FAIL,
    sprintf("got %.10f m3 for 1 m, expected %.10f m3", vd, pi * 0.15^2))


# ==============================================================================
# Backend runs used by checks 06-09
# ==============================================================================
runs <- list(
  spruce = list(species_group = "spruce", hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 10L, seed = 42L),
  pine   = list(species_group = "pine", hogstkl = c(41L, 42L, 51L, 52L),
                abonitet = c(11L, 14L), n_plots = 10L, seed = 42L))
res <- lapply(runs, function(p)
  tryCatch(run_sampled_plot_bucking(p, price_matrix_path = "default_2021",
                                    root = root_dir),
           error = function(e) conditionMessage(e)))
runs_ok <- vapply(res, is.list, logical(1))
if (!all(runs_ok))
  for (nm in names(res)[!runs_ok])
    rec(sprintf("Backend run (%s)", nm), FAIL, res[[nm]])


# ==============================================================================
# Check 06: waste and top excluded in real runs
# ==============================================================================
section("06. Waste and top excluded (backend runs)")

if (all(runs_ok)) {
  n_trees6 <- 0L; dev6 <- 0; waste_nonzero <- 0L; over_stem <- 0L
  for (nm in names(res)) {
    r  <- res[[nm]]; sl <- r$selected_logs_all; ts <- r$tree_summary
    waste_nonzero <- waste_nonzero + sum(sl$ProductType == "waste" &
                                         sl$commercial_volume_m3 != 0)
    for (k in which(ts$n_logs_selected > 0L)) {
      tr <- ts[k, ]
      lg <- sl[sl$plot_id_github == tr$plot_id_github & sl$treid == tr$treid, ]
      pr <- generate_taper_profile_one_tree(tr$treid, tr$plot_id_github,
                                            tr$species_group, tr$dbh_cm, tr$height_m)
      pv <- add_under_bark_profile(profiles_to_profile_vectors(pr)[[1]])
      nw <- lg$ProductType != "waste"
      hand <- 0
      d_ub <- stats::approxfun(pv$diameterPosition, pv$DiameterValueUB, rule = 2)
      for (j in which(nw)) hand <- hand + hand_cyl_sum(d_ub, lg$StartPos[j], lg$StopPos[j])
      dev6 <- max(dev6, abs(hand - tr$commercial_volume_m3))
      last_top <- if (any(nw)) max(lg$StopPos[nw]) else 10
      stem_to_last <- hand_cyl_sum(d_ub, 0, last_top)
      if (tr$commercial_volume_m3 > stem_to_last + 1e-12) over_stem <- over_stem + 1L
      n_trees6 <- n_trees6 + 1L
    }
  }
  rec("Check 06: Commercial volume = non-waste logs only (recomputed per tree)",
      if (dev6 <= 1e-9 && waste_nonzero == 0L && over_stem == 0L) PASS else FAIL,
      sprintf("%d trees (spruce + pine runs) | max |backend - hand| = %.2e m3 | waste logs with volume: %d | trees above stem-to-last-log volume: %d",
              n_trees6, dev6, waste_nonzero, over_stem))
}


# ==============================================================================
# Check 07: denominator unaffected by prices
# ==============================================================================
section("07. Denominator unaffected by prices")

if (all(runs_ok)) {
  pm_x15 <- load_price_matrix_long(root_dir)
  pm_x15$PriceNOKperM3 <- pm_x15$PriceNOKperM3 * 1.5
  tmp <- tempfile(fileext = ".csv"); readr::write_csv(pm_x15, tmp)
  r15 <- run_sampled_plot_bucking(runs$spruce, price_matrix_path = tmp, root = root_dir)
  file.remove(tmp)
  r1  <- res$spruce
  # Compare per tree. Scaling prices changes floating-point rounding in the DP,
  # which can flip between equal-value cutting patterns (e.g. two splits of the
  # same flat-priced pulpwood stretch). Such ties are legitimate, but they must
  # not change the tree's commercial volume or its value (x1.5).
  ts1 <- r1$tree_summary; ts15 <- r15$tree_summary
  m   <- merge(ts1, ts15, by = c("plot_id_github", "treid"), suffixes = c("", ".x15"))
  key <- function(sl) paste(sl$StartPos, sl$StopPos, sl$ProductKey)
  pattern_differs <- vapply(seq_len(nrow(m)), function(i) {
    a <- r1$selected_logs_all;  a <- a[a$plot_id_github == m$plot_id_github[i] & a$treid == m$treid[i], ]
    b <- r15$selected_logs_all; b <- b[b$plot_id_github == m$plot_id_github[i] & b$treid == m$treid[i], ]
    !identical(sort(key(a)), sort(key(b)))
  }, logical(1))
  dev_tree_vol <- max(abs(m$commercial_volume_m3.x15 - m$commercial_volume_m3))
  dev_tree_val <- max(abs(m$total_value_nok.x15 - 1.5 * m$total_value_nok))
  same_logs <- nrow(m) == nrow(ts1) && dev_tree_vol <= 1e-12 &&
    dev_tree_val <= 1e-9 && !any(pattern_differs)
  t1  <- r1$per_m3_summary[r1$per_m3_summary$scope == "Total", ]
  t15 <- r15$per_m3_summary[r15$per_m3_summary$scope == "Total", ]
  dev_vol   <- abs(t15$commercial_volume_m3 - t1$commercial_volume_m3)
  dev_ratio <- abs(t15$nok_per_m3 / t1$nok_per_m3 - 1.5)

  # Fixed pattern: same logs, two price matrices -> identical helper result.
  lg <- r1$selected_logs_all
  lg2 <- lg; lg2$PriceNOKperM3 <- lg2$PriceNOKperM3 * 3; lg2$value_nok <- lg2$value_nok * 3
  cyl_pos <- seq(0L, 3000L, by = 10L)
  fixed_same <- identical(commercial_volume_ub(cyl_pos, 400 - 0.1 * cyl_pos, lg),
                          commercial_volume_ub(cyl_pos, 400 - 0.1 * cyl_pos, lg2))

  rec("Check 07: All prices x1.5 -> same commercial volume per tree, NOK/m3 x1.5",
      if (same_logs && dev_vol <= 1e-12 && dev_ratio <= 1e-9 && fixed_same) PASS else FAIL,
      sprintf("%d trees | max |tree volume diff| %.1e m3, max |value x1.5 diff| %.1e NOK | trees with a different pattern: %d | total %.6f vs %.6f m3 | NOK/m3 %.3f -> %.3f (ratio - 1.5 = %.1e) | fixed pattern identical: %s",
              nrow(m), dev_tree_vol, dev_tree_val, sum(pattern_differs),
              t1$commercial_volume_m3, t15$commercial_volume_m3,
              t1$nok_per_m3, t15$nok_per_m3, dev_ratio, fixed_same))
}


# ==============================================================================
# Check 08: price lookup on under-bark top diameter
# ==============================================================================
section("08. Price class from under-bark top diameter")

if (all(runs_ok)) {
  sl <- do.call(rbind, lapply(res, function(r) r$selected_logs_all))
  saw <- sl[sl$ProductType == "sawlog", ]
  cls <- floor(saw$TopDiameter_ub / 10)
  # TopDiameter_ub is stored rounded to 0.1 mm; the class is computed from the
  # unrounded diameter, so a top within 0.05 mm of a class boundary may differ.
  boundary <- abs(saw$TopDiameter_ub - 10 * round(saw$TopDiameter_ub / 10)) <= 0.05
  mism <- saw$PriceDiameterClass_cm_uncapped != cls & !boundary
  le   <- saw$TopDiameter_ub <= saw$TopDiameter_ob
  n_zero_bark <- sum(saw$TopDiameter_ub == saw$TopDiameter_ob)
  rec("Check 08: Sawlog price class = floor(TopDiameter_ub / 10); TopDiameter_ub <= TopDiameter_ob",
      if (nrow(saw) > 0L && !any(mism) && all(le)) PASS else FAIL,
      sprintf("%d sawlogs (spruce + pine) | class mismatches: %d (boundary cases skipped: %d) | ub > ob: %d | tops with zero bark: %d | mean top bark %.1f mm",
              nrow(saw), sum(mism), sum(boundary), sum(!le), n_zero_bark,
              mean(saw$TopDiameter_ob - saw$TopDiameter_ub)))
}


# ==============================================================================
# Check 09: total NOK/m3
# ==============================================================================
section("09. Total NOK/m3")

if (all(runs_ok)) {
  dev9 <- max(vapply(res, function(r) {
    t <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
    abs(t$nok_per_m3 - sum(r$selected_logs_all$value_nok) /
          sum(r$selected_logs_all$commercial_volume_m3))
  }, numeric(1)))
  rec("Check 09: Total NOK/m3 = total value / total commercial volume",
      if (dev9 <= 1e-9) PASS else FAIL,
      sprintf("spruce %.1f, pine %.1f NOK/m3 | max deviation %.2e",
              res$spruce$per_m3_summary$nok_per_m3[1],
              res$pine$per_m3_summary$nok_per_m3[1], dev9))
}


# ==============================================================================
# Check 10: pulpwood price volume equals its commercial volume
# ==============================================================================
section("10. Pulpwood price volume = commercial volume")

if (all(runs_ok)) {
  sl10 <- do.call(rbind, lapply(res, function(r) r$selected_logs_all))
  pw   <- sl10[sl10$ProductType == "pulpwood", ]
  d10  <- abs(pw$price_volume_m3 - pw$commercial_volume_m3)
  rec("Check 10: Pulpwood price volume equals commercial volume (1e-9 m3)",
      if (nrow(pw) > 0L && all(d10 <= 1e-9)) PASS else FAIL,
      sprintf("%d pulpwood logs (spruce + pine) | over tolerance: %d | max |diff| %.3e m3 | mean diff (price - commercial) %.3e m3 | totals %.5f vs %.5f m3",
              nrow(pw), sum(d10 > 1e-9), max(d10),
              mean(pw$price_volume_m3 - pw$commercial_volume_m3),
              sum(pw$price_volume_m3), sum(pw$commercial_volume_m3)))
}


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 18 complete.\n")
cat("Results written to:", out_dir, "\n")
