# ==============================================================================
# audits/audit_20_assortments_and_volume_methods.R
#
# Purpose : Audit of Phase 4b (2026-10-13): per-product price-volume methods
#           (R/helpers_volume_methods.R), custom assortments
#           (R/helpers_products.R, R/helpers_price_matrix.R) and the
#           per-product price-diameter cap (R/helpers_valuation.R).
#
# Checks  :
#  1.  Top cylinder by hand: Dt 187.6 mm rounded down to 180, L 400 cm
#      -> pi * 90^2 * 400 / 1e8 = 0.10179 m3
#  2.  Top (frustum, 1 cm/m) by hand: Dt 200, L 400 -> Db 240,
#      pi*400*(200^2+200*240+240^2)/12/1e8 = 0.15247 m3; with rounding,
#      Dt 187.6 -> 180, Db 220 -> 0.12608 m3
#  3.  Calculated Norwegian mid by hand (Maalereglement for skogsvirke,
#      Generelle bestemmelser 2026, 4.1.3-4.1.4): Dt 187.6 mm -> 18 cm class,
#      + 0.5 cm, L 40 dm: V = 3.14/4 * (18.5 + 40/20)^2 * 40/100
#      = 131.9585 dm3 -> 0.13196 m3 at 5 decimals (optBuck as it was,
#      round(Dt) + 0.5 mm and pi: 0.13657)
#  4.  Solid volume: explicit sum of top-diameter cylinders on a cone equals
#      price_volume_m3() and section_volume_fun()
#  5.  End to end on a synthetic cone: each method's candidate price volume
#      equals the hand value; with DiameterTopPosition = 10 the top
#      diameter (limits, price class, volume) is read 10 cm below the cut
#  6.  A custom assortment that should win: high-priced poles are selected
#  7.  A custom assortment that should not win: priced low it loses; outside
#      its butt limit it is never selected even at a very high price; the
#      reference and fast optimizers agree in every case
#  8.  Default products unchanged: backend results for audit 16's three
#      strata equal the pre-Phase-4b baseline (outputs/assortments/
#      baseline_phase4b_default.rds; rows sorted, since arrow returns the
#      taper cache in no fixed order; run with the pre-Phase-4c base
#      products, pre_phase4c_limits()); default sawlog candidate volumes
#      equal the old top-cylinder formula on a taperNOR stem
#  9.  validate_price_matrix_against_products(): default + custom matrix +
#      custom flat passes; a custom key without a price, or priced both
#      ways, FAILs
# 10.  validate_products() and validate_assortment_defs(): one broken
#      definition per rule gives its error
# 11.  Round trips: assortments xlsx export -> import (definitions and both
#      price sets identical); the scenario long CSV carries custom keys; a
#      wide CSV grid for a custom matrix is accepted
# 12.  Per-product cap: a custom matrix up to 70 cm prices a 65 cm log in
#      the 65 class and a 75 cm log in 70; default sawlogs still cap at 50;
#      re-pricing under a matrix with another cap uses that matrix's cap
# 13.  Comparison with a scenario-only assortment (backend runs): trees using
#      it count as changed; per-m3 shows the class in Total and the
#      ProductKey within spruce; commercial volume of a fixed pattern does
#      not depend on the volume method
# 14.  Editing an assortment keeps prices: after renaming it, dropping a
#      length, adding a length and raising the max class, every cell that
#      still exists keeps its default and its scenario price, new cells get
#      the starting price, removed cells are dropped; a flat price survives a
#      length change, a newly added species starts fresh, flat -> grid keeps
#      nothing
# 15.  Mid (StanForD VolumeDiameterCategory 3, 2026-10-21) by hand on
#      synthetic stems: Dmid read under bark at the log's middle (off the
#      grid for a 370 cm log), rounded down to cm, pi/4 * Dmid^2 * L; on a
#      curved stem Dmid is interpolated, not top + taper, and the price class
#      still comes from the top; the backend's usable-only path agrees
#
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/assortments/audit_20_assortments_and_volume_methods.csv
#   outputs/assortments/audit_20_assortments_and_volume_methods.txt
#
# Usage   : Rscript audits/audit_20_assortments_and_volume_methods.R
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
Sys.setenv(APTER_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))


out_dir <- path_output("assortments", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_20_assortments_and_volume_methods.txt")
audit_csv <- file.path(out_dir, "audit_20_assortments_and_volume_methods.csv")
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
cat("  Audit: custom assortments and volume methods\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

pd0 <- load_products(root_dir); lc0 <- load_length_classes(root_dir)
pm0 <- pm_minimal(load_price_matrix_long(root_dir))
TOL <- 1e-9

# A custom assortment definition (app units, cm), overridable.
mk_def <- function(...) {
  d <- data.frame(Name = "Poles", Class = "poles", Species = "spruce",
                  MinTopDiameter_cm = 12, MaxTopDiameter_cm = 25,
                  MinButtDiameter_cm = 0, MaxButtDiameter_cm = Inf,
                  AllowedLengths_cm = "600;800;1000", Priority = 1,
                  DiameterUnderBark = TRUE, DiameterTopPosition_cm = 0,
                  VolumeDiameterCategory = "All diameters (solid volume)",
                  VolumeDiameterAdjustment = "None", PriceType = "flat",
                  UseIn = "both", stringsAsFactors = FALSE)
  a <- list(...); for (n in names(a)) d[[n]] <- a[[n]]
  .type_assortment_defs(d)
}

# Synthetic cone: over bark 400 mm at the ground to 0 at 20 m; under bark
# 10 mm less (floored at 0). Profile on the 10 cm grid.
cone_pv <- function() {
  pos <- seq(0L, 2000L, by = 10L)
  ob  <- 400 - 0.2 * pos
  list(treid = "C1", plot_id = "P0", species_group = "spruce", DBH = 400 - 0.2 * 130,
       height_cm = 2000L, diameterPosition = pos, DiameterValue = ob,
       DiameterValueUB = pmax(ob - 10, 0))
}
cone_ub <- function(z) pmax(400 - 0.2 * z - 10, 0)

# Price, then run both optimizers; returns list(ref, fast) selected logs.
optimise_both <- function(cands, pm) {
  lk <- build_price_lookup(pm)
  el <- cands[cands$eligible & !is.na(cands$price_volume_m3), , drop = FALSE]
  el <- prepare_price_join_keys(el, lk)
  el$PriceNOKperM3 <- lookup_prices(el, lk)
  el$value_nok <- el$price_volume_m3 * el$PriceNOKperM3
  el <- el[!is.na(el$value_nok), , drop = FALSE]
  list(ref = optimize_one_tree_dp(el)$selected_logs, fast = .optimize_dp_fast(el)$selected_logs)
}
pattern <- function(s) {
  if (nrow(s) == 0L) character(0) else sort(paste(s$StartPos, s$StopPos, s$ProductKey))
}


# ==============================================================================
# Checks 01-03: formula methods by hand
# ==============================================================================
section("01-03. Formula methods by hand")

v1 <- price_volume_m3("Top cylinder", 187.6, 400, adjustment = VOLUME_ROUND_DOWN_CM)
rec("Check 01: Top cylinder, Dt 187.6 rounded to 180 mm, L 400 cm = 0.10179 m3",
    if (abs(v1 - 0.10179) <= TOL) PASS else FAIL, sprintf("got %.5f", v1))

v2a <- price_volume_m3("Top", 200, 400)
v2b <- price_volume_m3("Top", 187.6, 400, adjustment = VOLUME_ROUND_DOWN_CM)
rec("Check 02: Top (frustum 1 cm/m): Dt 200, L 400 = 0.15247; Dt 187.6 rounded = 0.12608 m3",
    if (abs(v2a - 0.15247) <= TOL && abs(v2b - 0.12608) <= TOL) PASS else FAIL,
    sprintf("got %.5f and %.5f", v2a, v2b))

v3   <- price_volume_m3("Calculated Norwegian mid", 187.6, 400)
v3ob <- round(pi / 4 * (round(187.6) + 400 / 2 * 0.1 + 0.5)^2 * 400 / 1e8, 5)
v3nr <- price_volume_m3("Calculated Norwegian mid", 187.6, 400, adjustment = "None")
# Official formula in its own units: Dt cm (class 18 + 0.5), L dm, V dm3.
v3dm3 <- 3.14 / 4 * (18.5 + 40 / 20)^2 * 40 / 100
rec("Check 03: Calculated Norwegian mid, Dt 187.6, L 400 = 3.14/4*(18.5+2)^2*40/100 dm3 = 0.13196 m3 (always floors Dt)",
    if (abs(v3dm3 - 131.9585) <= 1e-9 && abs(v3 - 0.13196) <= TOL && v3nr == v3) PASS else FAIL,
    sprintf("got %.5f m3 (adjustment None: %.5f); official %.4f dm3; optBuck as it was: %.5f (+%.1f %%)",
            v3, v3nr, v3dm3, v3ob, 100 * (v3ob / v3 - 1)))


# ==============================================================================
# Check 04: solid volume
# ==============================================================================
section("04. Solid volume = sum of top-diameter cylinders")

pv <- cone_pv()
sv <- section_volume_fun(pv$diameterPosition, pv$DiameterValueUB, 10L)
tops  <- seq(20, 410, by = 10)            # sections (10, 410]: tops at 20..410
hand4 <- sum(pi * (cone_ub(tops) / 2)^2 * 10 / 1e8)
v4    <- price_volume_m3("All diameters (solid volume)", NA, 400, 10, 410, solid_vol = sv)
rec("Check 04: Solid volume of [10, 410] cm on the cone equals the explicit cylinder sum",
    if (abs(v4 - hand4) <= TOL && abs(sv(10, 410) - hand4) <= TOL) PASS else FAIL,
    sprintf("hand %.8f, price_volume_m3 %.8f, section_volume_fun %.8f", hand4, v4, sv(10, 410)))


# ==============================================================================
# Check 05: end to end through candidate generation
# ==============================================================================
section("05. Methods through generate_candidate_logs_one_tree()")

dt410 <- cone_ub(410)                     # 308 mm at the cut of a 4 m log from 10 cm
dt400 <- cone_ub(400)                     # 310 mm, 10 cm below that cut
hand5 <- c(
  "Top cylinder"  = round(pi * (floor(dt410 / 10) * 10 / 2)^2 * 400 / 1e8, 5),
  "Top"           = round(pi * 400 * (300^2 + 300 * 340 + 340^2) / 12 / 1e8, 5),
  "Calculated Norwegian mid" = round(3.14 / 4 * (300 + 5 + 20)^2 * 400 / 1e8, 5),
  "All diameters (solid volume)" = hand4)
adj5 <- c("Top cylinder" = VOLUME_ROUND_DOWN_CM, "Top" = VOLUME_ROUND_DOWN_CM,
          "Calculated Norwegian mid" = "None", "All diameters (solid volume)" = "None")
got5 <- vapply(names(hand5), function(m) {
  p <- products_for_run(pd0, lc0, mk_def(Name = "Test", MaxTopDiameter_cm = Inf,
                                         AllowedLengths_cm = "400", VolumeDiameterCategory = m,
                                         VolumeDiameterAdjustment = adj5[[m]]), "default")
  c5 <- generate_candidate_logs_one_tree(pv, p$pd, p$lc)
  c5$price_volume_m3[c5$ProductKey == "spruce_test" & c5$StartPos == 10 & c5$LogLength == 400]
}, numeric(1))
rec("Check 05a: Candidate price volume equals the hand value for all four methods (4 m log from 10 cm, Dt_ub 308 mm)",
    if (all(abs(got5 - hand5) <= TOL)) PASS else FAIL,
    paste(sprintf("%s %.5f/%.5f", names(hand5), got5, hand5), collapse = " | "))

p5 <- products_for_run(pd0, lc0, mk_def(Name = "Test", MaxTopDiameter_cm = Inf,
                                        AllowedLengths_cm = "400", DiameterTopPosition_cm = 10,
                                        VolumeDiameterCategory = "Calculated Norwegian mid"),
                       "default")
c5b <- generate_candidate_logs_one_tree(pv, p5$pd, p5$lc)
r5b <- c5b[c5b$ProductKey == "spruce_test" & c5b$StartPos == 10 & c5b$LogLength == 400, ]
want5b <- round(3.14 / 4 * (floor(dt400 / 10) * 10 + 5 + 20)^2 * 400 / 1e8, 5)
rec("Check 05b: DiameterTopPosition = 10 reads Dt 10 cm below the cut (310 mm): top diameter, price class 31, volume",
    if (nrow(r5b) == 1L && abs(r5b$TopDiameter - dt400) <= 0.05 &&
        r5b$PriceDiameterClass_cm == 31L && abs(r5b$price_volume_m3 - want5b) <= TOL) PASS else FAIL,
    sprintf("TopDiameter %.1f mm, class %s, volume %.5f (want %.5f)",
            r5b$TopDiameter, r5b$PriceDiameterClass_cm, r5b$price_volume_m3, want5b))


# ==============================================================================
# Checks 06-07: a custom assortment that should and should not win
# ==============================================================================
section("06-07. Custom assortment wins / does not win")

poles_case <- function(price, ...) {
  d  <- mk_def(...)
  p  <- products_for_run(pd0, lc0, d, "default")
  pm <- pm_minimal(rbind(pm0, pm_new_assortment_prices(d, price)))
  optimise_both(generate_candidate_logs_one_tree(pv, p$pd, p$lc), pm)
}
w <- poles_case(3000)
rec("Check 06: Poles at 3000 NOK/m3 (eligible on the cone) are selected; both optimizers agree",
    if (any(w$ref$ProductKey == "spruce_poles") && identical(pattern(w$ref), pattern(w$fast)))
      PASS else FAIL,
    paste(pattern(w$ref), collapse = "; "))

l <- poles_case(1)
rec("Check 07a: Poles at 1 NOK/m3 are not selected; both optimizers agree",
    if (!any(l$ref$ProductKey == "spruce_poles") && identical(pattern(l$ref), pattern(l$fast)))
      PASS else FAIL,
    paste(pattern(l$ref), collapse = "; "))

# Butt limit 15 cm: a 6 m pole must then start above 12.5 m, where its top
# would be below the 12 cm minimum, so no pole is ever eligible.
x <- poles_case(1e6, MaxButtDiameter_cm = 15)
rec("Check 07b: Poles outside their butt limit are never selected, even at 1e6 NOK/m3; optimizers agree",
    if (!any(x$ref$ProductKey == "spruce_poles") && identical(pattern(x$ref), pattern(x$fast)))
      PASS else FAIL,
    paste(pattern(x$ref), collapse = "; "))


# ==============================================================================
# Check 08: default products unchanged
# ==============================================================================
section("08. Default products unchanged")

# Roadmap 8: the baseline with the NFI tree numbers translated to the permuted ids (values untouched).
base_path <- file.path(root_dir, "outputs", "assortments", "baseline_phase4b_default_newids.rds")
if (!file.exists(base_path)) base_path <- file.path(root_dir, "outputs", "assortments", "baseline_phase4b_default.rds")
if (!file.exists(base_path)) {
  rec("Check 08a: Baseline file present", FAIL, base_path)
} else {
  b <- readRDS(base_path)
  srt <- function(d, k) { d <- d[do.call(order, unname(as.list(d[k]))), , drop = FALSE]
                          rownames(d) <- NULL; d }
  keys8 <- list(selected_logs_all = c("plot_id_github", "treid", "StartPos"),
                tree_summary = c("plot_id_github", "treid"), product_summary = "ProductKey",
                per_m3_summary = "scope", per_m3_by_assortment = c("scope", "assortment"),
                unpriced_summary = c("ProductKey", "LengthClass_dm"))
  det <- character(0); ok8 <- TRUE
  for (n in setdiff(names(b), "meta")) {
    # The baseline predates the Phase 4c diameter limits: run with the
    # pre-Phase-4c base products, so that nothing else may differ.
    r <- run_sampled_plot_bucking(b$meta$strata[[n]], "default_2021", root_dir,
                                  products = pre_phase4c_limits(load_products(root_dir)))
    # Phase 5 added tree_summary$height_source ("NFI" for NFI trees); the
    # baseline predates it, so it is checked separately and left out here.
    if (!all(r$tree_summary$height_source == "NFI")) {
      ok8 <- FALSE; det <- c(det, sprintf("%s: height_source not all 'NFI'", n))
    }
    r$tree_summary$height_source <- NULL
    for (k in names(keys8)) {
      a <- srt(r[[k]], keys8[[k]]); e <- srt(b[[n]][[k]], keys8[[k]])
      # Rows identical; sums over trees may differ in the last bit only
      # because of the row order, hence the tolerance for per-m3 tables.
      same <- if (startsWith(k, "per_m3")) isTRUE(all.equal(a, e, tolerance = 1e-12))
              else identical(a, e)
      if (!same) { ok8 <- FALSE; det <- c(det, sprintf("%s/%s differs", n, k)) }
    }
    det <- c(det, sprintf("%s: %d logs", n, nrow(r$selected_logs_all)))
  }
  rec("Check 08a: Backend results for 3 strata identical to the pre-Phase-4b baseline",
      if (ok8) PASS else FAIL,
      sprintf("baseline from commit %s | %s", substr(b$meta$commit, 1, 7),
              paste(det, collapse = "; ")))
}

z  <- seq(0.1, 22, by = 0.1)
pvt <- list(treid = "T1", plot_id = "P0", species_group = "spruce", DBH = 300,
            height_cm = 2200L, diameterPosition = as.integer(round(z * 100)),
            DiameterValue = taperNOR::taperNOR(z, 30, 22, "spruce", with_bark = TRUE) * 10)
c8  <- generate_candidate_logs_one_tree(pvt, pd0, lc0)
s8  <- c8[c8$ProductKey == "spruce_sawlog", ]
top <- floor(s8$TopDiameter / 10) * 10    # TopDiameter is stored to 0.1 mm
ub  <- add_under_bark_profile(pvt)$DiameterValueUB
raw <- stats::approxfun(pvt$diameterPosition, ub, rule = 2)(s8$StopPos)
old <- ifelse(s8$eligible & floor(raw / 10) * 10 > 0,
              round(pi * (floor(raw / 10) * 10 / 2)^2 * s8$LogLength / 1e8, 5), NA_real_)
rec("Check 08b: Default sawlog candidate volumes equal the old top-cylinder formula (taperNOR spruce 30 cm / 22 m)",
    if (identical(s8$price_volume_m3, old)) PASS else FAIL,
    sprintf("%d sawlog candidates, %d eligible", nrow(s8), sum(s8$eligible)))


# ==============================================================================
# Check 09: price validation with custom assortments
# ==============================================================================
section("09. validate_price_matrix_against_products() with custom assortments")

defs9 <- rbind(mk_def(), mk_def(Name = "Large sawlog", Class = "large sawlog", Species = "both",
                                MinTopDiameter_cm = 30, MaxTopDiameter_cm = 70,
                                AllowedLengths_cm = "400;500", VolumeDiameterCategory = "Top",
                                VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM,
                                PriceType = "matrix"))
p9  <- products_for_run(pd0, lc0, defs9, "default")
pm9 <- pm_minimal(rbind(pm0, pm_new_assortment_prices(defs9[1, ], 900),
                        pm_new_assortment_prices(defs9[2, ], 700)))
fails <- function(v) v$checks$check[v$checks$result == "FAIL"]
v9a <- validate_price_matrix_against_products(p9$pd, p9$lc, pm9)
v9b <- validate_price_matrix_against_products(p9$pd, p9$lc,
                                              pm9[pm9$ProductKey != "spruce_poles", ])
v9c <- validate_price_matrix_against_products(p9$pd, p9$lc, rbind(pm9, data.frame(
  ProductKey = "pine_large_sawlog", LengthClass_dm = NA, PriceDiameterClass_cm = NA,
  PriceNOKperM3 = 500)))
rec("Check 09a: Default + custom flat (poles) + custom matrix (large sawlog, both species) passes",
    if (length(fails(v9a)) == 0L) PASS else FAIL, paste(fails(v9a), collapse = "; "))
rec("Check 09b: Custom assortment without a price FAILs",
    if (any(grepl("spruce_poles has a price entry", fails(v9b), fixed = TRUE))) PASS else FAIL,
    paste(fails(v9b), collapse = "; "))
rec("Check 09c: Custom matrix assortment also given a flat price FAILs",
    if (any(grepl("priced as their PriceType", fails(v9c), fixed = TRUE))) PASS else FAIL,
    paste(fails(v9c), collapse = "; "))


# ==============================================================================
# Check 10: product-definition validation
# ==============================================================================
section("10. validate_products() / validate_assortment_defs(): broken definitions")

cases10 <- list(
  "unknown species"      = list(d = mk_def(Species = "birch"), pat = "Species must be"),
  "no lengths"           = list(d = mk_def(AllowedLengths_cm = ""), pat = "Allowed lengths"),
  "length not 10 cm"     = list(d = mk_def(AllowedLengths_cm = "605"), pat = "multiples of 10"),
  "min > max top"        = list(d = mk_def(MinTopDiameter_cm = 30), pat = "MaxTopDiameter must be >="),
  "min > max butt"       = list(d = mk_def(MinButtDiameter_cm = 30, MaxButtDiameter_cm = 20),
                                pat = "MaxButtDiameter must be >="),
  "unknown method"       = list(d = mk_def(VolumeDiameterCategory = "Butt mid"),
                                pat = "VolumeDiameterCategory must be"),
  "solid + rounding"     = list(d = mk_def(VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM),
                                pat = "solid volume is never rounded"),
  "class waste"          = list(d = mk_def(Class = "waste"), pat = "reserved"),
  "bad PriceType"        = list(d = mk_def(PriceType = "grid"), pat = "PriceType must be"),
  "top position >= log"  = list(d = mk_def(DiameterTopPosition_cm = 600),
                                pat = "must be shorter than the shortest log"),
  "top position not 10"  = list(d = mk_def(DiameterTopPosition_cm = 5),
                                pat = "multiple of 10"),
  "bad UseIn"            = list(d = mk_def(UseIn = "never"), pat = "UseIn must be"),
  "duplicate name"       = list(d = rbind(mk_def(), mk_def()), pat = "Duplicated assortment name"),
  "clash with base"      = list(d = mk_def(Name = "Sawlog"), pat = "clashes with a base product"))
for (nm in names(cases10)) {
  v <- validate_assortment_defs(cases10[[nm]]$d, pd0, lc0)
  hit <- any(grepl(cases10[[nm]]$pat, v$errors, fixed = TRUE))
  rec(sprintf("Check 10: %s -> rejected", nm), if (!v$ok && hit) PASS else FAIL,
      paste(v$errors, collapse = " / "))
}
v10 <- validate_assortment_defs(defs9, pd0, lc0)
rec("Check 10: valid definitions (poles, large sawlog) -> accepted",
    if (v10$ok) PASS else FAIL, paste(v10$errors, collapse = " / "))


# ==============================================================================
# Check 11: round trips
# ==============================================================================
section("11. Upload / download round trips")

defs11 <- defs9; defs11$UseIn <- c("scenario", "both")
pd_d <- products_for_run(pd0, lc0, defs11, "default")
pd_s <- products_for_run(pd0, lc0, defs11, "scenario")
def_pm <- pm_minimal(rbind(pm0, pm_new_assortment_prices(defs11[2, ], 700)))
scn_pm <- pm_minimal(rbind(pm0, pm_new_assortment_prices(defs11[1, ], 900),
                           pm_new_assortment_prices(defs11[2, ], 750)))
f11 <- tempfile(fileext = ".xlsx")
assortments_export(defs11, def_pm, scn_pm, f11)
imp <- assortments_import(f11, pd0, lc0, pm0, pm0)
same_df <- function(a, b) isTRUE(all.equal(a, b, check.attributes = FALSE))
rec("Check 11a: Assortments xlsx export -> import: definitions, default and scenario prices identical",
    if (isTRUE(imp$ok) && same_df(imp$defs, defs11) && same_df(imp$default_pm, def_pm) &&
        same_df(imp$scenario_pm, scn_pm)) PASS else FAIL,
    paste(c(imp$errors, imp$warnings), collapse = " / "))

f11b <- tempfile(fileext = ".csv"); pm_export(scn_pm, f11b)
u11b <- apply_price_upload(f11b, f11b, pm0, pm0, pd_s$pd, pd_s$lc)
rec("Check 11b: Scenario long CSV with custom keys -> accepted, custom prices kept",
    if (isTRUE(u11b$ok) && all(c("spruce_poles", "pine_large_sawlog") %in% u11b$pm$ProductKey))
      PASS else FAIL,
    paste(u11b$errors, collapse = " / "))

g11 <- pm_to_grid(scn_pm, "spruce_large_sawlog"); g11[1, 1] <- 999
f11c <- tempfile(fileext = ".csv")
w11 <- cbind(data.frame(x = as.integer(rownames(g11))), g11)
names(w11) <- c("spruce_large_sawlog", colnames(g11))
utils::write.csv(w11, f11c, row.names = FALSE)
u11c <- apply_price_upload(f11c, f11c, scn_pm, pm0, pd_s$pd, pd_s$lc)
cell <- u11c$pm$PriceNOKperM3[u11c$pm$ProductKey == "spruce_large_sawlog" &
                                u11c$pm$PriceDiameterClass_cm == as.integer(rownames(g11)[1]) &
                                u11c$pm$LengthClass_dm == as.integer(colnames(g11)[1])]
rec("Check 11c: Wide CSV grid for a custom matrix -> accepted, edited cell applied",
    if (isTRUE(u11c$ok) && identical(cell, 999)) PASS else FAIL,
    paste(c(u11c$errors, sprintf("cell %s", paste(cell, collapse = ","))), collapse = " / "))


# ==============================================================================
# Check 12: per-product price-diameter cap
# ==============================================================================
section("12. Per-product price-diameter cap")

big <- pm_new_assortment_prices(defs9[2, ], 0)
big$PriceNOKperM3 <- 1000 + big$PriceDiameterClass_cm     # price reveals the class
pm12 <- pm_minimal(rbind(pm0, big))
lk12 <- build_price_lookup(pm12)
logs12 <- data.frame(ProductKey = c("spruce_large_sawlog", "spruce_large_sawlog", "spruce_sawlog"),
                     LengthClass_dm = c(40L, 40L, 43L),
                     PriceDiameterClass_cm_uncapped = c(65L, 75L, 55L))
pr12 <- lookup_prices(logs12, lk12)
def50 <- pm0$PriceNOKperM3[pm0$ProductKey == "spruce_sawlog" & pm0$LengthClass_dm == 43 &
                             pm0$PriceDiameterClass_cm == 50]
rec("Check 12a: Custom matrix up to 70 cm prices a 65 cm log in class 65 and a 75 cm log in 70",
    if (identical(pr12[1:2], c(1065, 1070)) &&
        identical(unname(lk12$max_class["spruce_large_sawlog"]), 70L)) PASS else FAIL,
    sprintf("prices %s (want 1065, 1070); cap %s", paste(pr12[1:2], collapse = ", "),
            lk12$max_class["spruce_large_sawlog"]))
rec("Check 12b: Default spruce sawlog still caps at 50: a 55 cm log is priced at class 50",
    if (identical(pr12[3], def50) && identical(unname(lk12$max_class["spruce_sawlog"]), 50L))
      PASS else FAIL,
    sprintf("price %s, class-50 price %s", pr12[3], def50))
pm12b <- pm12[!(pm12$ProductKey == "spruce_large_sawlog" & pm12$PriceDiameterClass_cm > 60), ]
pr12b <- lookup_prices(logs12[1:2, ], pm12b)
rec("Check 12c: Re-pricing under a matrix ending at 60 cm uses that matrix's cap (65 and 75 -> 60)",
    if (identical(pr12b, c(1060, 1060))) PASS else FAIL,
    sprintf("prices %s", paste(pr12b, collapse = ", ")))


# ==============================================================================
# Check 13: comparison with a scenario-only assortment
# ==============================================================================
section("13. Comparison, scenario-only assortment (backend runs)")

s13 <- list(species_group = "spruce", hogstkl = c(41L, 42L), abonitet = c(14L, 17L),
            n_plots = 8L, seed = 42L)
d13 <- mk_def(UseIn = "scenario", MaxTopDiameter_cm = 30)
ps  <- products_for_run(pd0, lc0, d13, "scenario")
pm13 <- pm_minimal(rbind(pm0, pm_new_assortment_prices(d13, 2000)))
f13 <- tempfile(fileext = ".csv"); pm_export(pm13, f13)
r1 <- run_sampled_plot_bucking(s13, "default_2021", root_dir)
r2 <- run_sampled_plot_bucking(s13, f13, root_dir, products = ps$pd, length_classes = ps$lc)
cmp <- compare_cutting_patterns(r1, r2, pm0, pm13)
uses <- unique(paste(r2$selected_logs_all$plot_id_github, r2$selected_logs_all$treid,
                     sep = "|")[r2$selected_logs_all$ProductKey == "spruce_poles"])
chg  <- paste(cmp$plot_id_github, cmp$treid, sep = "|")[cmp$changed]
rec("Check 13a: Every tree using the scenario-only assortment counts as changed",
    if (length(uses) > 0L && all(uses %in% chg)) PASS else FAIL,
    sprintf("%d trees use spruce_poles; %d changed in total", length(uses), length(chg)))
ba <- r2$per_m3_by_assortment
cp <- compare_per_m3(r1, r2)
rec("Check 13b: Per-m3 shows 'poles' in Total and 'spruce_poles' within spruce (and in the comparison, 0 in default)",
    if (any(ba$scope == "Total" & ba$assortment == "poles") &&
        any(ba$scope == "spruce" & ba$assortment == "spruce_poles") &&
        any(cp$assortment == "poles" & cp$nok_per_m3_default == 0 & cp$nok_per_m3_scenario > 0))
      PASS else FAIL,
    sprintf("Total poles share %.3f", ba$volume_share[ba$scope == "Total" & ba$assortment == "poles"][1]))

logs13 <- data.frame(StartPos = c(10L, 610L), StopPos = c(610L, 1010L),
                     ProductType = c("poles", "pulpwood"))
cv_a <- commercial_volume_ub(pv$diameterPosition, pv$DiameterValueUB, logs13)
vm13 <- vapply(VOLUME_METHODS, function(m) {
  p <- products_for_run(pd0, lc0, mk_def(VolumeDiameterCategory = m, MaxTopDiameter_cm = Inf), "default")
  c <- generate_candidate_logs_one_tree(pv, p$pd, p$lc)
  c$price_volume_m3[c$ProductKey == "spruce_poles" & c$StartPos == 10 & c$LogLength == 600]
}, numeric(1))
rec("Check 13c: Commercial volume of a fixed pattern does not depend on the volume method (price volumes do)",
    if (length(unique(round(vm13, 9))) == length(VOLUME_METHODS) &&
        abs(cv_a[1] - sv(10, 610)) <= TOL) PASS else FAIL,
    sprintf("commercial %.5f m3; price volumes %s", cv_a[1],
            paste(sprintf("%s %.5f", names(vm13), vm13), collapse = ", ")))


# ==============================================================================
# Check 14: editing an assortment keeps the prices of cells that still exist
# ==============================================================================
section("14. Editing an assortment keeps existing prices")

old14 <- defs9[2, ]                                   # large sawlog, both, 30-70 cm, 400;500
start14 <- pm_new_assortment_prices(old14, 700)
setp <- function(pm, k, l, d, p) {
  pm$PriceNOKperM3[pm$ProductKey == k & pm$LengthClass_dm == l & pm$PriceDiameterClass_cm == d] <- p
  pm
}
def14 <- setp(start14, "spruce_large_sawlog", 40L, 35L, 811)
scn14 <- setp(setp(start14, "spruce_large_sawlog", 40L, 35L, 911), "pine_large_sawlog", 50L, 45L, 922)
# Edit: renamed, 500 cm dropped, 600 cm added, max top 70 -> 75 cm.
new14 <- mk_def(Name = "Big sawlog", Class = "large sawlog", Species = "both",
                MinTopDiameter_cm = 30, MaxTopDiameter_cm = 75, AllowedLengths_cm = "400;600",
                VolumeDiameterCategory = "Top", VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM,
                PriceType = "matrix")
cells14 <- pm_new_assortment_prices(new14, 650)
kd <- pm_keep_assortment_prices(cells14, def14, old14, new14)
ks <- pm_keep_assortment_prices(cells14, scn14, old14, new14)
at <- function(pm, k, l, d) pm$PriceNOKperM3[pm$ProductKey == k & pm$LengthClass_dm %in% l &
                                             pm$PriceDiameterClass_cm %in% d]
n_kept_want <- 2L * 41L                               # 2 species x classes 30-70 at 40 dm
ok14a <- identical(at(kd, "spruce_big_sawlog", 40L, 35L), 811) &&
  identical(at(ks, "spruce_big_sawlog", 40L, 35L), 911) &&
  identical(at(kd, "pine_big_sawlog", 40L, 50L), 700) &&      # kept cell, unedited
  identical(at(ks, "spruce_big_sawlog", 60L, 35L), 650) &&    # new length
  identical(at(kd, "pine_big_sawlog", 40L, 73L), 650) &&      # new diameter class
  !any(kd$LengthClass_dm == 50L | ks$LengthClass_dm == 50L) && # removed length dropped
  identical(attr(kd, "n_kept"), n_kept_want) && identical(attr(ks, "n_kept"), n_kept_want) &&
  nrow(kd) == 2L * 46L * 2L
rec("Check 14a: Edited grid (renamed, a length removed and one added, max class raised) keeps default and scenario prices of existing cells; new cells at the starting price; removed cells dropped",
    if (ok14a) PASS else FAIL,
    sprintf("spruce 40/35: default %s (want 811), scenario %s (want 911); new 60 dm %s, new 73 cm %s (want 650); kept %s/%s of %d (want %d each); 50 dm rows left: %d",
            at(kd, "spruce_big_sawlog", 40L, 35L), at(ks, "spruce_big_sawlog", 40L, 35L),
            at(ks, "spruce_big_sawlog", 60L, 35L), at(kd, "pine_big_sawlog", 40L, 73L),
            attr(kd, "n_kept"), attr(ks, "n_kept"), nrow(kd), n_kept_want,
            sum(kd$LengthClass_dm == 50L | ks$LengthClass_dm == 50L, na.rm = TRUE)))

flat_old <- mk_def()                                  # spruce poles, flat
flat_pm  <- pm_new_assortment_prices(flat_old, 900); flat_pm$PriceNOKperM3 <- 950
flat_new <- mk_def(AllowedLengths_cm = "600;700", Species = "both")
kf <- pm_keep_assortment_prices(pm_new_assortment_prices(flat_new, 500), flat_pm, flat_old, flat_new)
km <- pm_keep_assortment_prices(pm_new_assortment_prices(mk_def(PriceType = "matrix"), 500),
                                flat_pm, flat_old, mk_def(PriceType = "matrix"))
ok14b <- identical(at(kf, "spruce_poles", NA, NA), 950) && identical(at(kf, "pine_poles", NA, NA), 500) &&
  identical(attr(km, "n_kept"), 0L) && all(km$PriceNOKperM3 == 500)
rec("Check 14b: Flat price kept when lengths change; a new species gets the starting price; flat -> grid keeps no cells",
    if (ok14b) PASS else FAIL,
    sprintf("spruce %s (want 950), pine %s (want 500); flat->grid kept %s",
            at(kf, "spruce_poles", NA, NA), at(kf, "pine_poles", NA, NA), attr(km, "n_kept")))


# ==============================================================================
# Check 15: Mid (StanForD VolumeDiameterCategory 3) by hand on synthetic stems
# ==============================================================================
section("15. Mid: diameter at the middle of the log")

# Stem with a linear under-bark profile set directly (no bark model):
# 300 mm at the ground, 1 cm/m taper, 12 m; over bark 10 mm more.
mid_pv <- function(ub) {
  pos <- seq(0L, 1200L, by = 10L)
  list(treid = "M1", plot_id = "P0", species_group = "spruce", DBH = ub(130) + 10,
       height_cm = 1200L, diameterPosition = pos, DiameterValue = ub(pos) + 10,
       DiameterValueUB = ub(pos))
}
pv15 <- mid_pv(function(z) 300 - 0.1 * z)
p15  <- products_for_run(pd0, lc0, mk_def(Name = "Test", MaxTopDiameter_cm = Inf,
                                          AllowedLengths_cm = "370;400", VolumeDiameterCategory = "Mid",
                                          VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM), "default")
log15 <- function(pv, start, len) {
  c15 <- generate_candidate_logs_one_tree(pv, p15$pd, p15$lc)
  c15[c15$ProductKey == "spruce_test" & c15$StartPos == start & c15$LogLength == len, ]
}
# 10-410: middle at 210 cm, Dmid 279 mm -> 270; pi/4 * 0.27^2 * 4.0 = 0.22902 m3.
# 10-380: middle at 195 cm (off the grid), Dmid 280.5 mm -> 280;
#         pi/4 * 0.28^2 * 3.7 = 0.22783 m3.
hand15a <- c(round(pi / 4 * 0.27^2 * 4.0, 5), round(pi / 4 * 0.28^2 * 3.7, 5))
fun15a  <- c(price_volume_m3("Mid", NA, 400, adjustment = VOLUME_ROUND_DOWN_CM, mid_d_mm = 279),
             price_volume_m3("Mid", NA, 370, adjustment = VOLUME_ROUND_DOWN_CM, mid_d_mm = 280.5))
r15a <- rbind(log15(pv15, 10, 400), log15(pv15, 10, 370))
rec("Check 15a: Mid by hand: 10-410 cm (Dmid 279 -> 270 mm) = 0.22902 m3; 10-380 cm (mid 195 cm off the grid, Dmid 280.5 -> 280 mm) = 0.22783 m3; price_volume_m3() and candidates agree",
    if (all(abs(hand15a - c(0.22902, 0.22783)) <= TOL) && all(abs(fun15a - hand15a) <= TOL) &&
        nrow(r15a) == 2L && all(abs(r15a$price_volume_m3 - hand15a) <= TOL)) PASS else FAIL,
    sprintf("hand %s; price_volume_m3 %s; candidates %s", paste(sprintf("%.5f", hand15a), collapse = ", "),
            paste(sprintf("%.5f", fun15a), collapse = ", "),
            paste(sprintf("%.5f", r15a$price_volume_m3), collapse = ", ")))

# Non-linear profile: Dmid is interpolated between the grid points around the
# middle, not computed from the top diameter. ub(z) = 320 - 0.25 z + 1e-4 z^2:
# d(190) = 276.11, d(200) = 274.00 -> Dmid(195) = 275.055 -> 270 mm,
# V = pi/4 * 0.27^2 * 3.7 = 0.21185 m3. Top at 380 cm = 239.44 mm (class 23);
# top + 1 cm/m over half the log would give 257.94 -> 250 mm (0.18162 m3).
pv15b <- mid_pv(function(z) 320 - 0.25 * z + 1e-4 * z^2)
r15b  <- log15(pv15b, 10, 370)
hand15b <- round(pi / 4 * 0.27^2 * 3.7, 5)
rec("Check 15b: Mid on a curved stem reads Dmid (275.055 -> 270 mm) at the log's middle, not top + taper; price class still from the top (239.4 mm -> 23)",
    if (nrow(r15b) == 1L && abs(hand15b - 0.21185) <= TOL &&
        abs(r15b$price_volume_m3 - hand15b) <= TOL && r15b$PriceDiameterClass_cm == 23L &&
        abs(r15b$TopDiameter - 239.44) <= 0.05) PASS else FAIL,
    sprintf("volume %.5f (want %.5f); top %.1f mm, class %s",
            r15b$price_volume_m3, hand15b, r15b$TopDiameter, r15b$PriceDiameterClass_cm))

# The backend's path (usable candidates only) prices the same volumes.
plan15 <- .candidate_species_plan("spruce", p15$pd, p15$lc)
j15 <- match("spruce_test", vapply(plan15$products, `[[`, "", "pk"))
g15 <- .candidate_columns(pv15b, plan15, usable_only = TRUE)$cols
u15 <- g15$price_volume_m3[g15$prod == j15 & g15$StartPos == 10 & g15$StopPos == 380]
err15 <- tryCatch({ price_volume_m3("Mid", 200, 400); "" }, error = conditionMessage)
rec("Check 15c: The backend's usable-only candidates give the same Mid volume; Mid without a mid diameter is an error",
    if (length(u15) == 1L && abs(u15 - hand15b) <= TOL && grepl("mid_d_mm", err15)) PASS else FAIL,
    sprintf("usable-only %s; error: %s", paste(sprintf("%.5f", u15), collapse = ", "), err15))


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 20 complete.\n")
cat("Results written to:", out_dir, "\n")
