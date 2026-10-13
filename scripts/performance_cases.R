# ==============================================================================
# scripts/performance_cases.R
#
# Purpose : The run cases of the Performance phase, shared by the main
#           baseline (scripts/performance_save_main_baseline.R), the profile
#           and benchmark scripts and audit 22, so they all run the same
#           inputs. See the baseline script for the list of cases.
#
# Needs   : the backend (scripts/app_backend/01_run_sampled_plot_bucking.R)
#           and scripts/performance_tree_lists.R, sourced first.
# ==============================================================================

#' The three NFI strata of audits 16 and 20.
PERF_NFI_STRATA <- list(
  spruce = list(species_group = "spruce", hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
  pine   = list(species_group = "pine",   hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
  both   = list(species_group = "both",   hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 10L, seed = 7L))

#' Price matrices and custom assortments used by the cases.
#'
#' scenario_pm: default 2021 prices with spruce sawlog +10 % and pine
#' pulpwood +50 NOK/m3. Custom assortments: one per UseIn value, so a
#' default/scenario pair has products only in A, only in B, and in both.
#'
#' limits = "pre_phase4c" gives the base products their limits before Phase
#' 4c (no maximum diameters; pre_phase4c_limits()), as in the main baseline.
perf_case_inputs <- function(root, limits = c("current", "pre_phase4c")) {
  limits <- match.arg(limits)
  pd0 <- load_products(root); lc0 <- load_length_classes(root)
  if (limits == "pre_phase4c") pd0 <- pre_phase4c_limits(pd0)
  pm0 <- pm_minimal(load_price_matrix_long(root))

  scn <- pm0
  i <- scn$ProductKey == "spruce_sawlog"
  scn$PriceNOKperM3[i] <- round(scn$PriceNOKperM3[i] * 1.1, 2)
  i <- scn$ProductKey == "pine_pulpwood"
  scn$PriceNOKperM3[i] <- scn$PriceNOKperM3[i] + 50

  def <- function(...) {
    d <- data.frame(Name = "Poles", Class = "poles", Species = "spruce",
                    MinTopDiameter_cm = 12, MaxTopDiameter_cm = 25,
                    MinButtDiameter_cm = 0, MaxButtDiameter_cm = Inf,
                    AllowedLengths_cm = "600;800;1000", Priority = 1,
                    DiameterUnderBark = TRUE, DiameterTopPosition_cm = 0,
                    VolumeDiameterCategory = "All diameters (solid volume)",
                    VolumeDiameterAdjustment = "None", PriceType = "flat",
                    UseIn = "default", stringsAsFactors = FALSE)
    a <- list(...); for (n in names(a)) d[[n]] <- a[[n]]
    .type_assortment_defs(d)
  }
  defs <- rbind(
    def(),
    def(Name = "Large sawlog", Class = "large sawlog", Species = "both",
        MinTopDiameter_cm = 30, MaxTopDiameter_cm = 70, AllowedLengths_cm = "400;500",
        VolumeDiameterCategory = "Top", VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM,
        PriceType = "matrix", UseIn = "scenario"),
    def(Name = "Thick poles", Class = "poles", Species = "pine",
        MinTopDiameter_cm = 18, MaxTopDiameter_cm = 35, AllowedLengths_cm = "700;900",
        DiameterTopPosition_cm = 10, VolumeDiameterCategory = "Calculated Norwegian mid",
        VolumeDiameterAdjustment = VOLUME_ROUND_DOWN_CM, UseIn = "both"))

  p_def <- products_for_run(pd0, lc0, defs, "default")
  p_scn <- products_for_run(pd0, lc0, defs, "scenario")
  # Each run's matrix holds prices for its own products only, as in the app.
  pm_def <- pm_minimal(rbind(pm0, pm_new_assortment_prices(defs[1, ], 900),
                             pm_new_assortment_prices(defs[3, ], 800)))
  pm_scn <- pm_minimal(rbind(scn, pm_new_assortment_prices(defs[2, ], 750),
                             pm_new_assortment_prices(defs[3, ], 850)))
  list(pm0 = pm0, scenario_pm = scn, defs = defs, pd0 = pd0, lc0 = lc0, limits = limits,
       custom_default = list(pd = p_def$pd, lc = p_def$lc, pm = pm_def),
       custom_scenario = list(pd = p_scn$pd, lc = p_scn$lc, pm = pm_scn))
}

#' Named list of cases. Each: source ("nfi" or "list"), params or n_trees,
#' pm (NULL = default 2021), pd/lc (NULL = default products). With limits =
#' "pre_phase4c" every case carries the pre-Phase-4c base products.
perf_cases <- function(root, tree_list_sizes = c(100L, 1000L),
                       limits = c("current", "pre_phase4c")) {
  inp <- perf_case_inputs(root, match.arg(limits))
  out <- list()
  for (nm in names(PERF_NFI_STRATA))
    out[[paste0("nfi_", nm)]] <- list(source = "nfi", params = PERF_NFI_STRATA[[nm]])
  for (n in tree_list_sizes)
    out[[paste0("tl", n)]] <- list(source = "list", n_trees = n)
  if (1000L %in% tree_list_sizes)
    out$tl1000_scn <- list(source = "list", n_trees = 1000L, pm = inp$scenario_pm)
  out$tl100_custom_default  <- c(list(source = "list", n_trees = 100L), inp$custom_default)
  out$tl100_custom_scenario <- c(list(source = "list", n_trees = 100L), inp$custom_scenario)
  if (inp$limits == "pre_phase4c")
    for (nm in names(out)) if (is.null(out[[nm]]$pd)) {
      out[[nm]]$pd <- inp$pd0; out[[nm]]$lc <- inp$lc0
    }
  out
}

#' Comparisons: pairs of cases on the same trees, run as one call (the
#' second as `scenario`). Their results must equal the two separate runs.
PERF_COMPARISONS <- list(
  tl1000_cmp        = c("tl1000", "tl1000_scn"),
  tl100_custom_cmp  = c("tl100_custom_default", "tl100_custom_scenario"))

#' Run a comparison: case a with case b as its scenario (extra arguments,
#' e.g. workers, are passed on). The result holds b's run as $scenario.
perf_run_comparison <- function(a, b, root, ...) {
  path_b <- "default_2021"
  if (!is.null(b$pm)) {
    path_b <- tempfile(pattern = "apter_perf_pm_", fileext = ".csv")
    pm_export(b$pm, path_b)
    on.exit(unlink(path_b), add = TRUE)
  }
  perf_run_case(a, root, scenario = list(price_matrix_path = path_b, products = b$pd,
                                         length_classes = b$lc), ...)
}

#' Result tables compared with the main baseline, and their sort keys (the
#' backend's row order on main follows arrow's read order).
PERF_TABLE_KEYS <- list(
  selected_logs_all    = c("plot_id_github", "treid", "StartPos"),
  tree_summary         = c("plot_id_github", "treid"),
  per_m3_summary       = "scope",
  per_m3_by_assortment = c("scope", "assortment"),
  product_summary      = "ProductKey",
  unpriced_summary     = c("ProductKey", "LengthClass_dm"),
  zero_log_trees       = c("plot_id_github", "treid"))

.perf_sorted <- function(d, k) {
  d <- d[do.call(order, unname(as.list(d[k]))), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Compare a run with a baseline case, table by table, after sorting rows.
#'
#' @return Named character vector per table: "identical", "equal within
#'   1e-12 (sums over trees in another order)" or "DIFFERS".
perf_compare <- function(r, b) {
  vapply(names(PERF_TABLE_KEYS), function(k) {
    a <- .perf_sorted(r[[k]], PERF_TABLE_KEYS[[k]])
    e <- .perf_sorted(b[[k]], PERF_TABLE_KEYS[[k]])
    if (identical(a, e)) "identical"
    else if (isTRUE(all.equal(a, e, tolerance = 1e-12))) "equal within 1e-12"
    else "DIFFERS"
  }, character(1))
}

#' Run one case with the backend runners (extra arguments, e.g. workers or
#' cache, are passed on).
perf_run_case <- function(case, root, ...) {
  path <- "default_2021"
  if (!is.null(case$pm)) {
    path <- tempfile(pattern = "apter_perf_pm_", fileext = ".csv")
    pm_export(case$pm, path)
    on.exit(unlink(path), add = TRUE)
  }
  if (identical(case$source, "nfi"))
    run_sampled_plot_bucking(params = case$params, price_matrix_path = path, root = root,
                             products = case$pd, length_classes = case$lc, ...)
  else
    run_tree_list_bucking(perf_tree_list(case$n_trees), price_matrix_path = path,
                          root = root, products = case$pd, length_classes = case$lc, ...)
}
