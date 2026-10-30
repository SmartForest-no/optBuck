# ==============================================================================
# audits/audit_33_price_tuning.R
#
# Purpose : Price-matrix tuning (roadmap 7, session 2): R/helpers_price_tuning.R
#           (tuning_grid(), apply_grid_prices(), tuning_target(), tally_grid(),
#           fg_grid(), update_prices(), tune_prices()) and the backend's
#           swap_job_prices() / subset_job_stems().
#
# Private data : checks 3-8 read the owner's real .hpr file5 (REDACTED28
#           in C:/apter_private/hpr/, outside the repository, read in place).
#           No audit before this one reads a private file. When the file is
#           missing these checks are recorded as REVIEW ("skipped", verdict
#           ACCEPTED WITH REVIEW), not FAIL. Only counts and numbers are written.
#           The product is file5's Spruce "140 SCA" sawlog.
#
# Checks  :
#  1.  fg_grid hand case: one class, shares 30,13,19,19,19 vs 20 each -> FG 90
#  2.  update_prices on 2x2 grids, expected values computed by hand (in the
#      comments): plain step (r = 1), stagnation (r = 2), calibration (r = 11),
#      floor (never above P1), unavailable and non-eligible cells equal P1
#  3.  tuning_target: neutral and long sum to 100 per class; real gives 30/45/25
#      on the 43/49/55 dm classes and 0 on 37 dm (430/490/550 and 370 cm)
#  4.  apply_grid_prices round trip: tuning_grid(apply(grid$price))$price is
#      identical to grid$price, and unavailable cells are untouched
#  5.  Determinism: tune_prices (real target, n = 200, seed 1, max_iter 3) twice
#      -> identical trace (elapsed time left out) and best_price
#  6.  Iteration-1 value check: the product's value in the result equals
#      sum(V * P1) within 0.5 NOK, and value_orig of iteration 1 equals the
#      run's total value
#  7.  tally_grid holds every log: counts and price volume of the grid equal the
#      product's totals in selected_logs_all (original prices, all stems); a
#      result with a log outside the grid stops with the difference
#  8.  The hidden Distribution column of the price matrix (Phase 7a): the
#      imported pm has it, apply_grid_prices() leaves it identical, and
#      swap_job_prices() does not pass it to the backend
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/price_tuning/audit_33_price_tuning.csv
#   outputs/price_tuning/audit_33_price_tuning.txt
#
# Usage   : Rscript audits/audit_33_price_tuning.R
# ==============================================================================

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
Sys.setenv(APTER_ROOT = root_dir)

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)
source(file.path(root_dir, "R", "helpers_price_tuning.R"))
source(file.path(root_dir, "scripts", "roadmap7_helpers.R"))   # read_hpr_distribution()

out_dir <- path_output("price_tuning", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_33_price_tuning.txt")
audit_csv <- file.path(out_dir, "audit_33_price_tuning.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"; REVIEW <- "REVIEW"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
near <- function(a, b, tol = 1e-9) {
  if (length(a) != length(b) || !identical(as.vector(is.na(a)), as.vector(is.na(b)))) return(FALSE)
  h <- !is.na(a); isTRUE(all(abs(as.vector(a)[h] - as.vector(b)[h]) < tol))
}
m22 <- function(...) matrix(c(...), 2L, 2L, byrow = TRUE)        # filled by rows

# ==============================================================================
section("1. fg_grid hand case")
# One diameter class, five length classes. Shares 30,13,19,19,19 (counts, total
# 100) against 20 each: |10|+|7|+|1|+|1|+|1| = 20; FG = 100 - 0.5 * 20 = 90.
tal1 <- list(n = matrix(c(30, 13, 19, 19, 19), 5L, 1L), v = matrix(1, 5L, 1L))
f1 <- fg_grid(tal1, matrix(20, 5L, 1L), "number")
rec("Check 1: fg_grid hand case gives FG = 90 (overall and class)",
    ok(near(f1$overall, 90) && near(unname(f1$by_class), 90)), sprintf("overall %.6f", f1$overall))

# ==============================================================================
section("2. update_prices on 2x2 grids (rows = length classes, columns = diameter classes)")
# Common data: W = 50 everywhere; S = [60 40; 30 70] (each column sums to 100);
# D = S - W = [10 -10; -20 20]; P = P1 = [100 200; 300 400]; g = 0.95 for r < 3;
# V = V1 = 1 everywhere unless stated; floor 0; both classes eligible.
P1 <- m22(100, 200, 300, 400); W <- matrix(50, 2L, 2L); S <- m22(60, 40, 30, 70)
V1 <- matrix(1, 2L, 2L); fl0 <- matrix(0, 2L, 2L); el <- c(TRUE, TRUE)
# (a) r = 1, plain step: P - 0.95 * D = [100-9.5, 200+9.5; 300+19, 400-19]
a <- update_prices(P1, P1, S, NULL, W, 1L, el, fl0, V1, V1)
ok_a <- near(a, m22(90.5, 209.5, 319, 381))
# (b) r = 2, S_prev = [59.8 50; 40 70]: |S - S_prev| = [0.2 10; 10 0]. Cells
# (1,1) and (2,2) differ by < 0.5: stagnated, keep P (100, 400). Cells (1,2) and
# (2,1) step: 200 + 0.95*10 = 209.5, 300 + 0.95*20 = 319.
b <- update_prices(P1, P1, S, m22(59.8, 50, 40, 70), W, 2L, el, fl0, V1, V1)
ok_b <- near(b, m22(100, 209.5, 319, 400))
# (c) r = 11: g = 0.95^9 = 0.630249409724609. Class 2 has S = W (D = 0): no change
# before calibration. Class 1: D = (10, -10), step: 100 - 10g, 300 + 10g. With
# the latest volumes V = (3, 1) and the first V1 = (1, 1) in class 1:
# cbar_1 = (100 + 300)/2 = 200; cbar_new = (3(100-10g) + (300+10g))/4 = 150 - 5g;
# shift = cbar_new - cbar_1 = -50 - 5g; calibrated: 150 - 5g = 146.848752951377
# and 350 + 15g = 359.453741145869 (their V-weighted mean is 200 again).
# Class 2 stays (200, 400); its means are equal (300) so its shift is 0.
Sc <- m22(60, 50, 40, 50); Vc <- m22(3, 1, 1, 1)
cc <- update_prices(P1, P1, Sc, m22(0, 0, 0, 0), W, 11L, el, fl0, Vc, V1)
ok_c <- near(cc, m22(146.848752951377, 200, 359.453741145869, 400), 1e-9) &&
  near(sum(Vc[, 1] * cc[, 1]) / sum(Vc[, 1]), 200, 1e-9)
# (d) floor: floor_ij = min(P1, 250) = [100 200; 250 250]. Case (a) gives
# [90.5 209.5; 319 381]; cell (1,1) 90.5 is raised to 100 (= P1, not above);
# the others are above their floor and stay.
d <- update_prices(P1, P1, S, NULL, W, 1L, el, pmin(P1, 250), V1, V1)
ok_d <- near(d, m22(100, 209.5, 319, 381)) && all(d[1L, 1L] <= P1[1L, 1L])
# (e) unavailable and non-eligible cells equal P1: P1 has NA at (2,2), W is NA
# there, class 2 is not eligible. Class 1 steps as in (a): (90.5, 319); class 2
# stays (200, NA).
P1e <- m22(100, 200, 300, NA); We <- m22(50, 50, 50, NA)
e <- update_prices(P1e, P1e, S, NULL, We, 1L, c(TRUE, FALSE), fl0, V1, V1)
ok_e <- near(e, m22(90.5, 200, 319, NA))
rec("Check 2: update_prices: plain step, stagnation, calibration, floor, unavailable and non-eligible cells",
    ok(ok_a && ok_b && ok_c && ok_d && ok_e),
    sprintf("a %s, b %s, c %s, d %s, e %s", ok_a, ok_b, ok_c, ok_d, ok_e))

# ==============================================================================
# Private data: file5 (REDACTED28)
f5 <- "C:/apter_private/hpr/REDACTED28"
have5 <- file.exists(f5)
skipped <- function(n, what) rec(sprintf("Check %d: %s", n, what), REVIEW,
                                 "skipped: file5 (C:/apter_private/hpr/REDACTED28) is not available")
if (!have5) {
  skipped(3, "targets on file5 spruce 140 SCA"); skipped(4, "apply_grid_prices round trip on file5")
  skipped(5, "tune_prices determinism on file5"); skipped(6, "iteration-1 value check on file5")
  skipped(7, "tally_grid holds every log (file5)")
  skipped(8, "hidden Distribution column kept by apply_grid_prices (file5)")
} else {
  section("Setup: file5 products, grid and the original-price run of all stems")
  hp <- read_hpr(f5); stopifnot(isTRUE(hp$ok))
  smap_h <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap_h); stopifnot(isTRUE(sc$ok))
  pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
  products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
  dst <- read_hpr_distribution(f5)
  key <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" &
                                  grepl("140 sca", products$pd$ProductName, ignore.case = TRUE)][1L]
  hk <- sc$defs$HprProductKey[match(key, assortment_table_keys(sc$defs))]
  hpr <- hpr_tuning_info(dst, hk)
  grid <- tuning_grid(products, key, hpr)
  st <- read_hpr_stems(f5); stopifnot(isTRUE(st$ok))
  smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
  rm(st); invisible(gc())
  res <- finish_job(job, buck_job(job))

  # ============================================================================
  section("3. tuning_target")
  Wn <- tuning_target(grid, "neutral"); Wl <- tuning_target(grid, "long"); Wr <- tuning_target(grid, "real")
  av <- !is.na(grid$price)
  has <- colSums(av) > 0
  ok3a <- near(colSums(Wn[, has, drop = FALSE], na.rm = TRUE), rep(100, sum(has))) &&
    near(colSums(Wl[, has, drop = FALSE], na.rm = TRUE), rep(100, sum(has))) &&
    identical(is.na(Wn), !av) && identical(is.na(Wl), !av)
  # real: 30 / 45 / 25 on the 43 / 49 / 55 dm classes and 0 on 37 dm, in every class
  exp_real <- matrix(c(0, 30, 45, 25), nrow(Wr), ncol(Wr))
  rownames(exp_real) <- rownames(Wr)
  ok3b <- identical(rownames(Wr), c("37", "43", "49", "55")) && near(Wr, exp_real)
  # long: weights 1..k from the shortest: 10/20/30/40 (4 lengths, 1+2+3+4 = 10)
  ok3c <- near(Wl[, 1L], c(10, 20, 30, 40))
  rec("Check 3: neutral and long sum to 100 per class (NA where unavailable); real = 0/30/45/25 on 37/43/49/55 dm; long = 10/20/30/40",
      ok(ok3a && ok3b && ok3c), sprintf("grid %d lengths x %d classes; neutral/long %s, real %s, long weights %s",
                                         nrow(Wr), ncol(Wr), ok3a, ok3b, ok3c))

  # ============================================================================
  section("4. apply_grid_prices round trip")
  P_try <- grid$price; P_try[!is.na(P_try)] <- P_try[!is.na(P_try)] + 7.5
  prod2 <- apply_grid_prices(products, grid, P_try)
  g2 <- tuning_grid(prod2, key, hpr)
  rt <- tuning_grid(apply_grid_prices(products, grid, grid$price), key, hpr)
  other <- products$pm$ProductKey != key
  na_same <- identical(is.na(products$pm$PriceNOKperM3), is.na(prod2$pm$PriceNOKperM3))
  ok4 <- identical(rt$price, grid$price) && identical(g2$price, P_try) &&
    identical(products$pm[other, ], prod2$pm[other, ]) && na_same &&
    identical(apply_grid_prices(products, grid, grid$price)$pm, products$pm)
  rec("Check 4: tuning_grid(apply_grid_prices(P))$price == P; other products and unavailable cells untouched; the original grid reproduces pm",
      ok(ok4), sprintf("%d Apter rows of the product", sum(!other)))

  # ============================================================================
  section("5. tune_prices determinism")
  run_tune <- function() tune_prices(job, products, key, "real", n = 200, seed = 1, max_iter = 3, hpr = hpr)
  ta <- run_tune(); tb <- run_tune()
  drop_t <- function(x) x[setdiff(names(x), "elapsed_s")]
  ok5 <- identical(drop_t(ta$trace), drop_t(tb$trace)) && identical(ta$best_price, tb$best_price) &&
    nrow(ta$trace) == 3L && ta$settings$basis == "volume"
  rec("Check 5: tune_prices twice (real, n = 200, seed 1, max_iter 3) gives an identical trace and best_price",
      ok(ok5), sprintf("iterations %d, best iteration %d, FG %s", nrow(ta$trace), ta$best_iter,
                        paste(round(ta$trace$fg, 2), collapse = " -> ")))

  # ============================================================================
  section("6. iteration-1 value check")
  sl <- res$selected_logs_all
  tal <- tally_grid(res, key, grid)
  v_prod <- sum(sl$value_nok[as.character(sl$ProductKey) == key])
  v_sum <- sum(tal$v[!is.na(grid$price)] * grid$price[!is.na(grid$price)])
  tot <- sum(res$tree_summary$total_value_nok)
  # value_orig of iteration 1 (prices = original): total - sum(V*P) + sum(V*P1) = total
  ok6 <- abs(v_prod - v_sum) < 0.5 && abs(ta$trace$value_orig[1L] - ta$trace$value_total[1L]) < 1e-6
  rec("Check 6: the product's value equals sum(V * P1) within 0.5 NOK (all stems); value_orig of iteration 1 equals the total value",
      ok(ok6), sprintf("result %.3f NOK, sum(V*P1) %.3f NOK, difference %.6f; total value %.1f NOK",
                        v_prod, v_sum, v_prod - v_sum, tot))

  # ============================================================================
  section("7. tally_grid holds every log")
  nl <- sum(as.character(sl$ProductKey) == key); vl <- sum(sl$price_volume_m3[as.character(sl$ProductKey) == key])
  ok7a <- sum(tal$n) == nl && abs(sum(tal$v) - vl) < 1e-9 * max(1, vl) && nl > 0
  bad <- res; i <- which(as.character(bad$selected_logs_all$ProductKey) == key)[1L]
  bad$selected_logs_all$LengthClass_dm[i] <- 999L              # a length class the product does not have
  err <- tryCatch({ tally_grid(bad, key, grid); "" }, error = function(e) conditionMessage(e))
  ok7b <- grepl("difference", err, fixed = TRUE) && grepl("counts", err, fixed = TRUE)
  rec("Check 7: tally_grid counts and price volume equal the product's totals in selected_logs_all; a log outside the grid stops with the difference",
      ok(ok7a && ok7b), sprintf("%d logs, %.4f m3; grid %d, %.4f m3 | error: %s", nl, vl, sum(tal$n), sum(tal$v),
                                substr(err, 1L, 160L)))

  # ============================================================================
  section("8. hidden Distribution column")
  D <- "Distribution"
  d_has <- D %in% names(products$pm)
  d_same <- identical(prod2$pm[[D]], products$pm[[D]]) && identical(names(prod2$pm), names(products$pm))
  job_sw <- swap_job_prices(subset_job_stems(job, 1:20), prod2)
  d_off <- !D %in% names(job_sw$pps[[1L]]$pm)
  rec("Check 8: file5's price matrix has the hidden Distribution column; apply_grid_prices keeps it identical; swap_job_prices passes it to no run",
      ok(d_has && d_same && d_off), sprintf("column present %s (%d non-NA cells), identical after apply %s, absent in the run's pm %s",
                                            d_has, if (d_has) sum(!is.na(products$pm[[D]])) else 0L, d_same, d_off))
}

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 33 complete.\n")
