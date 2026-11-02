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
#  9.  tuning_sample_size: 10200 stems / 7598 logs -> 1343; 7283 / 1469 -> 4958;
#      7283 / 800 -> 7283; 100 / 0 -> 100 (and the bounds)
# 10.  tune_prices on_iter is called exactly max_iter times (early_stop = FALSE)
# 11.  tuning_products on file5: spruce 140 SCA listed with has_file_target TRUE,
#      spruce 154 Kubb (one length class) not listed
# 14.  Single-product invariant (restructure R3.1): tune_prices() on file5 spruce
#      140 SCA (n 200, max_iter 3, early_stop FALSE) equals the values stored
#      with the code before the refactor (outputs/restructure/, derived from a
#      private file, never committed; REVIEW when missing)
# 15.  Several products at once on file5: spruce 140 SCA with its file target and
#      spruce 148 with an Even target set by the editor helpers (n 1000,
#      max_iter 3): one FG per product, both products' prices change, the overall
#      FG is the iteration-1 weighted mean
# 16.  tuning_matrix_view(): the four matrix views of the tuning tab
# 13.  swap_job_prices keeps the job's stem-grade options: on file5 with both
#      options TRUE (and FALSE), a 1000-stem sample bucked before and after
#      swap_job_prices(same products) is identical; a short tune_prices() with
#      both options TRUE completes and its iteration 1 value equals a normal
#      run with grades on for the same sample
# 12.  tuning_hpr_info (from the hidden FileDiameterClass_mm) gives the same grid (lengths,
#      file classes, cm_map, prices, Distribution) and the same targets as the
#      file's own classes (read_hpr_distribution), for every tunable product of
#      file1, file3, file4 and file5 (alphabetical private files)
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
source(file.path(root_dir, "R", "helpers_distribution_edit.R"))   # Even target for check 15 (restructure R2)
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
section("9. tuning_sample_size")
ok9 <- identical(tuning_sample_size(10200L, 7598L), 1343L) && identical(tuning_sample_size(7283L, 1469L), 4958L) && identical(tuning_sample_size(7283L, 800L), 7283L) &&
  identical(tuning_sample_size(100L, 0L), 100L) && identical(tuning_sample_size(100L, 1000L), 100L) &&
  tuning_sample_size(5L, 1e6) == 1L && tuning_sample_size(10L, 1001L) <= 10L
rec("Check 9: tuning_sample_size 10200/7598 -> 1343; 7283/1469 -> 4958; 7283/800 -> 7283; 100/0 -> 100; at least 1, at most n_stems",
    ok(ok9), sprintf("%s, %s, %s", tuning_sample_size(10200L, 7598L), tuning_sample_size(7283L, 1469L), tuning_sample_size(100L, 0L)))


# ==============================================================================
section("16. tuning_matrix_view()")
g16 <- list(lengths = c(37, 43, 49), diam_classes = c(142, 150),
            price = matrix(c(500, 510, 520, 505, NA, 525), 3, 2, dimnames = list(c("37", "43", "49"), c("142", "150"))))
W16 <- matrix(c(20, 30, 50, 100/3, NA, 200/3), 3, 2, dimnames = dimnames(g16$price))
S16 <- matrix(c(10, 40, 50, 0, NA, 100), 3, 2, dimnames = dimnames(g16$price))
by16 <- list(grid = g16, W = W16, S_orig = S16, S_tuned = W16, fg_orig = c(`142` = 90, `150` = 66.6667), fg_tuned = c(`142` = 100, `150` = 100),
             P1 = g16$price, Pb = g16$price + matrix(c(-3, 0, 4, 0, NA, -6), 3, 2), eligible = c(`142` = TRUE, `150` = FALSE))
v_t <- tuning_matrix_view(by16, "target"); v_o <- tuning_matrix_view(by16, "orig")
v_u <- tuning_matrix_view(by16, "tuned"); v_c <- tuning_matrix_view(by16, "change")
ok16 <- identical(dim(v_t$m), c(2L, 3L)) && identical(rownames(v_t$m), c("142", "150")) && identical(colnames(v_t$m), c("370", "430", "490")) &&
  identical(unname(v_t$m[1, ]), c(20, 30, 50)) && identical(unname(v_t$m[2, ]), c(33, NA, 67)) && is.null(v_t$fg) &&
  identical(unname(v_o$m[1, ]), c(10, 40, 50)) && identical(v_o$fg, c(90, 66.6667)) && identical(v_u$fg, c(100, 100)) &&
  identical(unname(v_c$m[1, ]), c(-3, NA, 4)) && all(is.na(v_c$m[2, ])) && is.null(v_c$fg) &&   # class 150 is not eligible: nothing steered
  # R4 D5: unchanged priced cells (the 0 change of class 142, the whole non-steered class 150) are dash cells; changed cells are not
  identical(unname(v_c$dash[1, ]), c(FALSE, TRUE, FALSE)) && identical(unname(v_c$dash[2, ]), c(TRUE, FALSE, TRUE)) &&
  !any(v_t$dash) && !any(v_o$dash)
# R4 D5: classes without logs: their priced cells are dash cells and their fordelingsgrad is empty (outcome views only)
by16b <- c(by16, list(n_orig = c(`142` = 30, `150` = 0), n_tuned = c(`142` = 30, `150` = 12)))
v_ob <- tuning_matrix_view(by16b, "orig"); v_ub <- tuning_matrix_view(by16b, "tuned")
ok16 <- ok16 && identical(unname(v_ob$dash[2, ]), c(TRUE, FALSE, TRUE)) && !any(v_ob$dash[1, ]) && is.na(v_ob$fg[2]) && !is.na(v_ob$fg[1]) &&
  !any(v_ub$dash) && !anyNA(v_ub$fg) && all(is.na(v_ob$m[2, ]))
rec("Check 16: tuning_matrix_view(): classes in rows and lengths (cm) in columns; target and outcome shares in whole percent (NA where there is no price); FG per class for the outcome views only; price change for the steered cells only",
    ok(ok16), sprintf("target %s | outcome %s | change %s", paste(v_t$m, collapse = ","), paste(v_o$m, collapse = ","), paste(v_c$m, collapse = ",")))

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
  skipped(10, "tune_prices on_iter (file5)"); skipped(11, "tuning_products (file5)")
  skipped(13, "swap_job_prices keeps the stem-grade options (file5)")
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

  # ============================================================================
  section("10. on_iter")
  calls <- list()
  tn <- tune_prices(job, products, key, "real", n = 200, seed = 1, max_iter = 3, hpr = hpr, early_stop = FALSE,
                    on_iter = function(r, m, fg) calls[[length(calls) + 1L]] <<- c(r = r, m = m, fg = fg))
  ok10 <- length(calls) == 3L && identical(vapply(calls, `[[`, 0, "r"), c(1, 2, 3)) &&
    all(vapply(calls, `[[`, 0, "m") == 3) && near(vapply(calls, `[[`, 0, "fg"), tn$trace$fg)
  rec("Check 10: on_iter is called exactly max_iter times (early_stop = FALSE) with (r, max_iter, fg)",
      ok(ok10), sprintf("%d calls, FG %s", length(calls), paste(round(vapply(calls, `[[`, 0, "fg"), 2), collapse = " -> ")))

  # ============================================================================
  section("11. tuning_products")
  tp <- tuning_products(sc$defs, pm)
  i140 <- grepl("140 sca", tp$name, ignore.case = TRUE) & tp$species == "spruce"
  ok11 <- sum(i140) == 1L && isTRUE(tp$has_file_target[i140]) &&
    !any(grepl("154", tp$name) & grepl("kubb", tp$name, ignore.case = TRUE))
  rec("Check 11: tuning_products lists spruce 140 SCA sawlog with has_file_target TRUE and not spruce 154 Kubb",
      ok(ok11), sprintf("%d tunable products: %s", nrow(tp), paste(tp$product_id, collapse = ", ")))

  # ============================================================================
  section("13. swap_job_prices keeps the stem-grade options")
  st13 <- read_hpr_stems(f5); stopifnot(isTRUE(st13$ok))
  smap13 <- setNames(guess_hpr_species(st13$species$name, st13$species$user_id), st13$species$key)
  mk_job <- function(g) prepare_hpr_stems_job(st13, smap13, price_matrix_path = price_path, products = pr$pd,
                                              length_classes = pr$lc, grade_opts = list(use_grades = g, cut_at_changes = g))
  same_res <- function(a, b) identical(a$selected_logs_all, b$selected_logs_all) && identical(a$tree_summary, b$tree_summary)
  sample_of <- function(j, n) { set.seed(1); subset_job_stems(j, sample(length(j$pvs), min(n, length(j$pvs)))) }
  vals <- list(); same13 <- logical(0)
  for (g in c(TRUE, FALSE)) {
    j13 <- mk_job(g); js13 <- sample_of(j13, 1000L)
    r1 <- finish_job(js13, buck_job(js13)); r2 <- finish_job(swap_job_prices(js13, products), buck_job(swap_job_prices(js13, products)))
    same13[as.character(g)] <- same_res(r1, r2); vals[[as.character(g)]] <- sum(r1$tree_summary$total_value_nok)
    if (g) job13 <- j13
    rm(j13, js13, r1, r2); invisible(gc())
  }
  tg <- tune_prices(job13, products, key, "real", n = 200, seed = 1, max_iter = 3, hpr = hpr, early_stop = FALSE)
  js200 <- sample_of(job13, 200L); v200 <- sum(finish_job(js200, buck_job(js200))$tree_summary$total_value_nok)
  ok13 <- all(same13) && nrow(tg$trace) == 3L && abs(tg$trace$value_total[1L] - v200) < 1e-6 &&
    abs(vals[["TRUE"]] - vals[["FALSE"]]) > 1
  rec("Check 13: swap_job_prices keeps the stem-grade options: sample results identical before and after swapping (grades on and off); tune_prices with grades on runs, iteration 1 equals a normal run with grades on",
      ok(ok13), sprintf("identical on %s, off %s | sample value grades on %.0f, off %.0f NOK | tune iteration 1 %.3f vs normal run %.3f | FG %s",
                        same13[["TRUE"]], same13[["FALSE"]], vals[["TRUE"]], vals[["FALSE"]], tg$trace$value_total[1L], v200,
                        paste(round(tg$trace$fg, 2), collapse = " -> ")))
  rm(job13, st13); invisible(gc())

  # ============================================================================
  section("14. Single-product invariant")
  exp_f <- file.path(root_dir, "outputs", "restructure", "tuning_single_expected.rds")
  if (!file.exists(exp_f)) {
    rec("Check 14: tune_prices() on one product equals the values stored before the multi-product change", REVIEW,
        "skipped: outputs/restructure/tuning_single_expected.rds is missing (scripts/capture_tuning_single_baseline.R, run on the code before R3.1)")
  } else {
    e14 <- readRDS(exp_f)
    t14 <- tune_prices(job, products, key, "real", n = 200, seed = 1, max_iter = 3, hpr = hpr, early_stop = FALSE)
    tr14 <- t14$trace[setdiff(names(t14$trace), "elapsed_s")]
    same14 <- c(trace = identical(tr14, e14$trace), best_price = identical(t14$best_price, e14$best_price),
                best_iter = identical(t14$best_iter, e14$best_iter), fg = identical(t14$trace$fg, e14$fg),
                value = identical(t14$trace$value_total, e14$value))
    rec("Check 14: tune_prices() on one product (file5 spruce 140 SCA, n 200, max_iter 3) gives the same trace, best prices, FG and value as before the multi-product change",
        ok(all(same14)), sprintf("%s | FG %s", paste(sprintf("%s %s", names(same14), same14), collapse = ", "),
                                 paste(round(t14$trace$fg, 2), collapse = " -> ")))
  }

  # ============================================================================
  section("15. Several products at once")
  defs15 <- sc$defs; pm15 <- pm
  k148 <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" &
                                   grepl("148", products$pd$ProductName, fixed = TRUE)][1L]
  if (is.na(k148)) {
    rec("Check 15: several products tuned at once on file5", REVIEW, "skipped: no spruce 148 sawlog product in file5")
  } else {
    pm15 <- dist_apply_target(pm15, k148, "neutral")
    defs15 <- dist_init_fields(dist_clear_fields(defs15, k148), k148)
    pr15 <- list(pd = products$pd, lc = products$lc, pm = pm15)
    ids15 <- c(key, k148)
    hp15 <- setNames(lapply(ids15, function(k) tuning_hpr_info(defs15, pm15, k)), ids15)
    t15 <- tune_prices(job, pr15, ids15, "real", n = 1000, seed = 1, max_iter = 3, hpr = hp15, early_stop = FALSE)
    cols15 <- paste0("fg_", ids15)
    by15 <- t15$by_product
    changed <- vapply(ids15, function(k) any(abs(by15[[k]]$prices[[3L]] - by15[[k]]$P_first) > 1e-9, na.rm = TRUE), logical(1))
    ok15a <- nrow(t15$trace) == 3L && all(cols15 %in% names(t15$trace)) && all(!is.na(t15$trace[, cols15]))
    ok15b <- all(changed)
    # iteration-1 overall FG = mean of the products' FG weighted by their iteration-1 logs (number) or volume
    ok15c <- all(vapply(ids15, function(k) is.finite(by15[[k]]$fg[1L]), logical(1))) && is.finite(t15$trace$fg[1L]) &&
      t15$trace$fg[1L] >= min(vapply(ids15, function(k) by15[[k]]$fg[1L], 0)) - 1e-9 &&
      t15$trace$fg[1L] <= max(vapply(ids15, function(k) by15[[k]]$fg[1L], 0)) + 1e-9
    ok15d <- identical(names(t15$best_price), ids15) && t15$best_iter %in% 1:3
    rec("Check 15: Two products tuned together (file5 spruce 140 SCA with its file target, spruce 148 with an Even target; n 1000, max_iter 3): completes, one FG column per product plus the overall FG, both products' prices change, the overall FG lies between the products' FGs",
        ok(ok15a && ok15b && ok15c && ok15d),
        sprintf("FG overall %s | per product %s | prices changed %s | best iteration %s | sample %d stems",
                paste(round(t15$trace$fg, 2), collapse = " -> "),
                paste(sprintf("%s %s", c("140", "148"), vapply(ids15, function(k) paste(round(by15[[k]]$fg, 1), collapse = ">"), "")), collapse = "; "),
                paste(changed, collapse = "/"), t15$best_iter, t15$settings$sample_size))
  }
}

# ==============================================================================
section("12. tuning_hpr_info against the file's own classes (file1, file3, file4, file5)")
files12 <- c(file1 = "REDACTED1", file3 = "REDACTED33",
             file4 = "REDACTED27", file5 = "REDACTED28")
paths12 <- file.path("C:/apter_private/hpr", files12)
if (!all(file.exists(paths12))) {
  rec("Check 12: tuning_hpr_info equals the file's classes and targets for every tunable product (file1, 3, 4, 5)", REVIEW,
      sprintf("skipped: missing %s", paste(names(files12)[!file.exists(paths12)], collapse = ", ")))
} else {
  diffs <- character(0); n_prod <- 0L; n_tgt <- 0L
  for (fi in seq_along(files12)) {
    h12 <- read_hpr(paths12[fi]); sm12 <- setNames(guess_hpr_species(h12$species$name, h12$species$user_id), h12$species$key)
    s12 <- hpr_to_scenario(h12, sm12); if (!isTRUE(s12$ok)) { diffs <- c(diffs, sprintf("%s: scenario not ok", names(files12)[fi])); next }
    pm12 <- pm_minimal(s12$pm); pr12 <- products_from_defs(s12$defs, pm12); P12 <- list(pd = pr12$pd, lc = pr12$lc, pm = pm12)
    d12 <- read_hpr_distribution(paths12[fi])
    tp12 <- tuning_products(s12$defs, pm12)
    for (k in tp12$product_id) {
      n_prod <- n_prod + 1L
      hk12 <- s12$defs$HprProductKey[match(k, assortment_table_keys(s12$defs))]
      g_file <- tuning_grid(P12, k, hpr_tuning_info(d12, hk12))
      g_der <- tuning_grid(P12, k, tuning_hpr_info(s12$defs, pm12, k))
      bits <- c(lengths = identical(g_file$lengths, g_der$lengths), classes = identical(g_file$diam_classes, g_der$diam_classes),
                cm_map = identical(unname(g_file$cm_map), unname(g_der$cm_map)),
                price = identical(g_file$price, g_der$price), dist = identical(g_file$distribution, g_der$distribution),
                basis = identical(g_file$basis_default, g_der$basis_default))
      if (!all(bits)) diffs <- c(diffs, sprintf("%s %s: differs in %s | file classes (mm) %s | derived %s", names(files12)[fi], k,
                                                paste(names(bits)[!bits], collapse = ","),
                                                paste(g_file$diam_classes, collapse = " "), paste(g_der$diam_classes, collapse = " ")))
      else if (any(g_file$distribution > 0)) {
        n_tgt <- n_tgt + 1L
        if (!identical(tuning_target(g_file, "real"), tuning_target(g_der, "real")))
          diffs <- c(diffs, sprintf("%s %s: real target differs", names(files12)[fi], k))
      }
    }
  }
  rec("Check 12: tuning_hpr_info equals the file's classes (lengths, classes, cm_map, prices, Distribution, basis) and real targets for every tunable product (file1, 3, 4, 5)",
      ok(length(diffs) == 0L && n_prod > 0L),
      sprintf("%d tunable products, %d with a file target compared%s", n_prod, n_tgt,
              if (length(diffs)) paste0(" | DIFFERENCES: ", paste(diffs, collapse = " ;; ")) else ""))
}

# ==============================================================================
section("17. Tuned prices are whole NOK and are the prices evaluated and applied (R4)")
for (e in parse(file.path(root_dir, "app", "tuning.R"), keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]]) %in% c("TUNE_MIN_LOGS", "tuning_run")) eval(e)
FX17 <- file.path(root_dir, "inputs", "test_fixtures", "hpr_tuning_synthetic.hpr")
h17 <- read_hpr(FX17); st17 <- read_hpr_stems(FX17)
sc17 <- hpr_to_scenario(h17, setNames(guess_hpr_species(h17$species$name, h17$species$user_id), h17$species$key))
pm17 <- pm_minimal(sc17$pm); d17 <- sc17$defs
k17 <- assortment_table_keys(d17)[grep("140 SCA", d17$Name, fixed = TRUE)[1L]]
pm17 <- dist_apply_target(pm17, k17, "neutral"); d17 <- dist_init_fields(dist_clear_fields(d17, k17), k17)
pr17 <- products_from_defs(d17, pm17)
prod17 <- list(pd = pr17$pd, lc = pr17$lc, pm = pm17)
pp17 <- tempfile(fileext = ".csv"); pm_export(pm17, pp17)
job17 <- prepare_hpr_stems_job(st17, setNames(guess_hpr_species(st17$species$name, st17$species$user_id), st17$species$key),
                               price_matrix_path = pp17, products = pr17$pd, length_classes = pr17$lc)
r17 <- tuning_run(job17, prod17, d17, k17, 3L, list(use_grades = FALSE, cut_at_changes = FALSE))
ok17 <- !isTRUE(r17$too_few)
if (ok17) {
  Pb17 <- r17$by[[k17]]$Pb; g17 <- r17$by[[k17]]$grid
  applied <- apply_grid_prices(prod17, g17, Pb17)$pm           # what "Use tuned prices" writes into the other scenario
  ap <- applied[applied$ProductKey == k17 & !is.na(applied$LengthClass_dm) & !is.na(applied$PriceNOKperM3), , drop = FALSE]
  want <- Pb17[cbind(match(ap$LengthClass_dm, g17$lengths), match(g17$cm_map[as.character(ap$PriceDiameterClass_cm)], g17$diam_classes))]
  # the evaluation used the same prices: re-evaluating the job with the applied prices gives the reported value
  cur17 <- apply_grid_prices(prod17, g17, Pb17)
  res17 <- finish_job(swap_job_prices(job17, cur17), buck_job(swap_job_prices(job17, cur17)))
  sl17 <- res17$selected_logs_all
  ok17 <- all(Pb17[!is.na(Pb17)] == round(Pb17[!is.na(Pb17)])) && all(ap$PriceNOKperM3 == round(ap$PriceNOKperM3)) &&
    nrow(ap) > 0L && !anyNA(want) && all(abs(ap$PriceNOKperM3 - want) < 1e-9) &&
    abs(sum(sl17$price_volume_m3[as.character(sl17$ProductType) == "sawlog"]) - r17$tuned$saw_vol) < 1e-6
}
rec("Check 17: tuning_run() returns whole-NOK best prices; the prices applied to the other scenario equal them and re-bucking with them reproduces the reported tuned sawlog volume",
    ok(ok17), sprintf("too few: %s | prices whole: %s | cells applied %s", isTRUE(r17$too_few),
                      if (ok17) "yes" else "no", if (exists("ap")) nrow(ap) else NA))

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 33 complete.\n")