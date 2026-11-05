# ==============================================================================
# R/helpers_heights.R
#
# Purpose : Heights for uploaded tree lists (Phase 5, 2026-10-13). Measured
#           heights are kept; missing heights are imputed per species:
#
#   >= HEIGHT_MIN_MEASURED_PER_SPECIES (10) measured heights
#       fieldPlot (Lennart Noordermeer, https://github.com/lennartnoordermeer/
#       fieldPlot): Naslund height-diameter model with a plot random effect,
#       fitHD_Plot() (Sharma & Breidenbach 2015), or fitHD() (nls, no plot
#       effect) with one plot, no plot IDs, an error or a timeout. Trees in a
#       plot with measured heights of that species get the plot-level
#       prediction ("fieldPlot (plot)"), others the species curve
#       ("fieldPlot (species curve)").
#   1-9 measured heights
#       FinnHoyde reference curve calibrated to the measured trees as in the
#       NFI tariff method: prediction x ratio, ratio = sum(h measured) /
#       sum(FinnHoyde at their dbh) ("FinnHoyde (calibrated, n = k)"). A
#       ratio outside 0.7-1.3 is reported as a warning.
#   no measured heights
#       FinnHoyde reference curve ("FinnHoyde (uncalibrated)"): average NFI
#       curves, independent of site; reported as a warning.
#
# FinnHoyde reference curves (d = dbh in cm over bark, h in m):
#   Spruce: d < 15      h = 1.3 + 0.6 d                      (Eid & Fitje 1993)
#           d >= 15     h = -14.17 + 20.86 log10(d)          (Fitje & Vestjordet 1977)
#   Pine:   d < 10      h = 1.3 + 0.65456 d                  (Eid & Fitje 1993)
#           10 <= d < 35 h = 0.39 + 0.852 d - 0.010644 d^2   (Vestjordet 1968)
#           d >= 35     h = 13.35 + 0.107 d                  (Eid & Fitje 1993)
#
# fieldPlot is used through its exported fitHD_Plot() / fitHD(), not vendored.
# Its predictMissingHeights() is not used: it always fits species codes 1-3
# and errors when one is absent, and fitHD_Plot()'s timeout never interrupts
# nlme() (both confirmed 2026-10-13). This wrapper fits only species present
# with enough heights and enforces its own time limit (setTimeLimit()).
# ==============================================================================

# Minimum measured heights of a species for a fieldPlot model.
HEIGHT_MIN_MEASURED_PER_SPECIES <- 10L

# Calibration ratios outside this range are reported as a warning.
HEIGHT_CALIBRATION_RANGE <- c(0.7, 1.3)

# Time limit (s) for one nlme fit before falling back to fitHD().
HEIGHT_FIT_TIMEOUT_S <- 30

# Height-source labels.
HEIGHT_SOURCE_MEASURED      <- "measured"
HEIGHT_SOURCE_FP_PLOT       <- "fieldPlot (plot)"
HEIGHT_SOURCE_FP_SPECIES    <- "fieldPlot (species curve)"
HEIGHT_SOURCE_FH_UNCAL      <- "FinnHoyde (uncalibrated)"
height_source_fh_calibrated <- function(k) sprintf("FinnHoyde (calibrated, n = %d)", k)


# FinnHoyde NFI reference height (m) from dbh (cm) and species.
#
# @param d_cm    Diameter at breast height, cm (vector).
# @param species "spruce" or "pine" (vector or scalar).
finnhoyde_height <- function(d_cm, species) {
  species <- rep_len(as.character(species), length(d_cm))
  h <- rep(NA_real_, length(d_cm))
  s <- species == "spruce"; p <- species == "pine"
  h[s] <- ifelse(d_cm[s] < 15, 1.3 + 0.6 * d_cm[s], -14.17 + 20.86 * log10(d_cm[s]))
  h[p] <- ifelse(d_cm[p] < 10, 1.3 + 0.65456 * d_cm[p],
          ifelse(d_cm[p] < 35, 0.39 + 0.852 * d_cm[p] - 0.010644 * d_cm[p]^2,
                 13.35 + 0.107 * d_cm[p]))
  h
}


# Evaluate `expr` under an elapsed-time limit (interrupts nlme(), unlike
# fieldPlot's own timeout); the limit is cleared on exit.
.with_time_limit <- function(expr, seconds) {
  setTimeLimit(elapsed = seconds, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
  expr
}

# Fit fieldPlot's height model for one species' measured trees.
# Returns list(model, model_class, note).
.fit_species_heights <- function(d, h, plot, timeout_s) {
  if (!requireNamespace("fieldPlot", quietly = TRUE))
    stop("Package 'fieldPlot' is required for height imputation. Install: ",
         "remotes::install_github('lennartnoordermeer/fieldPlot')")
  n_plots <- length(unique(plot[!is.na(plot)]))
  note <- ""
  if (n_plots >= 2L) {
    t0 <- proc.time()[["elapsed"]]
    # fieldPlot's own timeout is disabled (Inf): it never interrupts nlme and
    # would only skip it; Apter's time limit does the interrupting.
    m <- tryCatch(suppressMessages(.with_time_limit(
           fieldPlot::fitHD_Plot(d, h, sp = 1L, plotID = plot, timeout = Inf),
           timeout_s)), error = function(e) e)
    el <- proc.time()[["elapsed"]] - t0
    if (inherits(m, "nlme")) return(list(model = m, model_class = "nlme", note = note))
    # fitHD_Plot() catches the time-limit error itself and returns its nls
    # fallback, so a timeout is recognised by the elapsed time.
    if (!inherits(m, "error") && el >= timeout_s)
      return(list(model = m, model_class = "nls", note = sprintf(
        "nlme stopped at the %g s time limit; nls used", timeout_s)))
    if (inherits(m, "nls")) return(list(model = m, model_class = "nls",
                                        note = "nlme did not converge; nls used"))
    note <- sprintf("nlme stopped after %.1f s (%s); nls used", el, conditionMessage(m))
  } else {
    note <- "fewer than 2 plots; nls used (no plot effect)"
  }
  m <- tryCatch(fieldPlot::fitHD(d, h), error = function(e) e)
  if (inherits(m, "error"))
    return(list(model = NULL, model_class = "none",
                note = paste0(if (nzchar(note)) paste0(note, "; ") else "",
                              "nls failed (", conditionMessage(m), ")")))
  list(model = m, model_class = "nls", note = note)
}


# Impute missing heights per species.
#
# @param trees data.frame with species_group ("spruce"/"pine"), dbh_cm,
#   height_m (measured, NA when missing) and plot_id (NA when not given).
# @param min_measured Minimum measured heights of a species for fieldPlot.
# @param timeout_s Time limit (s) for one nlme fit.
# @return list(height_m, height_source (one per tree), summary = data.frame
#   (species, n_trees, n_measured, method, model, ratio, note), warnings).
impute_heights <- function(trees, min_measured = HEIGHT_MIN_MEASURED_PER_SPECIES,
                           timeout_s = HEIGHT_FIT_TIMEOUT_S) {
  h   <- as.numeric(trees$height_m)
  d   <- as.numeric(trees$dbh_cm)
  sp  <- as.character(trees$species_group)
  plt <- if (is.null(trees$plot_id)) rep(NA_character_, nrow(trees)) else as.character(trees$plot_id)
  out <- h
  src <- ifelse(is.na(h), NA_character_, HEIGHT_SOURCE_MEASURED)
  rows <- list(); warn <- character(0)

  for (s in intersect(c("spruce", "pine"), unique(sp))) {
    idx  <- which(sp == s)
    meas <- idx[!is.na(h[idx])]
    miss <- idx[is.na(h[idx])]
    k    <- length(meas)
    ratio <- NA_real_; model <- ""; note <- ""
    f <- if (k >= min_measured) .fit_species_heights(d[meas], h[meas], plt[meas], timeout_s)
    if (!is.null(f) && is.null(f$model)) {     # no model could be fitted
      note <- paste0(f$note, "; calibrated FinnHoyde used")
      f <- NULL; model <- "none"
    }
    if (!is.null(f)) {
      model <- f$model_class; note <- f$note
      if (length(miss) > 0L) {
        nd <- data.frame(d = d[miss], plotID = plt[miss])
        if (identical(f$model_class, "nlme")) {
          p1 <- suppressWarnings(as.numeric(predict(f$model, nd, level = 1)))
          p0 <- as.numeric(predict(f$model, nd, level = 0))
          at_plot <- !is.na(p1) & plt[miss] %in% plt[meas]
          out[miss] <- ifelse(at_plot, p1, p0)
          src[miss] <- ifelse(at_plot, HEIGHT_SOURCE_FP_PLOT, HEIGHT_SOURCE_FP_SPECIES)
        } else {
          out[miss] <- as.numeric(predict(f$model, nd))
          src[miss] <- HEIGHT_SOURCE_FP_SPECIES
        }
      }
      method <- "fieldPlot"
    } else if (k >= 1L) {
      ratio <- sum(h[meas]) / sum(finnhoyde_height(d[meas], s))
      out[miss] <- ratio * finnhoyde_height(d[miss], s)
      src[miss] <- height_source_fh_calibrated(k)
      method <- height_source_fh_calibrated(k)
      if (ratio < HEIGHT_CALIBRATION_RANGE[1] || ratio > HEIGHT_CALIBRATION_RANGE[2])
        warn <- c(warn, sprintf(
          "%s: measured heights are on average %.0f %% %s the FinnHoyde curve (ratio %.2f, n = %d); check the heights or measure more trees.",
          tools::toTitleCase(s), abs(ratio - 1) * 100, if (ratio > 1) "above" else "below",
          ratio, k))
    } else {
      out[miss] <- finnhoyde_height(d[miss], s)
      src[miss] <- HEIGHT_SOURCE_FH_UNCAL
      method <- HEIGHT_SOURCE_FH_UNCAL
      warn <- c(warn, sprintf(
        "Heights for %d %s trees come from average NFI height curves (FinnHoyde, uncalibrated). These are independent of site and can be biased for your trees.",
        length(miss), s))
    }
    if (nzchar(note) && !identical(method, "fieldPlot") && model == "none")
      warn <- c(warn, sprintf("%s: %s.", tools::toTitleCase(s), note))
    rows[[s]] <- data.frame(species = s, n_trees = length(idx), n_measured = k,
                            method = method, model = model, ratio = ratio, note = note,
                            stringsAsFactors = FALSE)
  }
  summary <- if (length(rows)) do.call(rbind, rows) else
    data.frame(species = character(0), n_trees = integer(0), n_measured = integer(0),
               method = character(0), model = character(0), ratio = numeric(0),
               note = character(0), stringsAsFactors = FALSE)
  rownames(summary) <- NULL
  list(height_m = out, height_source = src, summary = summary, warnings = warn)
}
