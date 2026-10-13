# app/helpers_ui.R
#
# Purpose : UI building blocks in the ForestSens / Redwood style (cards,
#           read-only fields, status tiles, step list, tables, plots).
#           Pure functions returning tags or widgets; styles in www/apter.css,
#           colours in theme.R.


# ---- page chrome ---------------------------------------------------------------

#' Home header: icon tile, serif "Apter" title and tagline, then the stripe.
app_header <- function(tagline) {
  tagList(
    div(class = "apter-hero",
        tags$img(src = "apter-icon.svg", alt = ""),
        div(h1(class = "apter-title", "Apter"),
            p(class = "apter-tagline", tagline))),
    banner_strip()
  )
}

#' Decorative stripe below the home header (own CSS pattern).
banner_strip <- function() div(class = "banner-strip", `aria-hidden` = "true")

#' Navbar brand: small icon and app name.
navbar_brand <- function() {
  tags$span(tags$img(src = "apter-icon.svg", alt = ""), "Apter")
}

#' Footer with the release version and a back-to-top button.
app_footer <- function(version) {
  tags$footer(class = "apter-footer",
              span(sprintf("Release %s", version)),
              tags$a(class = "apter-top", href = "#", title = "Back to top",
                     `aria-label` = "Back to top", icon("chevron-up")))
}


# ---- cards ---------------------------------------------------------------------

#' A card with a plain header. collapsible = TRUE renders a <details> card
#' with the disclosure triangle from the reference (open by default).
apter_card <- function(title, ..., collapsible = FALSE, open = TRUE) {
  if (!collapsible)
    return(bslib::card(bslib::card_header(title), bslib::card_body(...)))
  tags$details(class = "apter-card card", open = if (open) NA else NULL,
               tags$summary(class = "card-header", title),
               div(class = "card-body", ...))
}


# ---- read-only fields ------------------------------------------------------------

#' Dashed, labelled box with a small grey label and a larger value.
#' wide = TRUE spans two grid columns (long values such as run IDs).
ro_field <- function(label, value, wide = FALSE) {
  div(class = paste("ro-field", if (wide) "is-wide"),
      div(class = "ro-label", label),
      div(class = "ro-value", value))
}

#' A responsive row of read-only fields.
ro_field_row <- function(...) div(class = "ro-row", ...)


# ---- status tiles ----------------------------------------------------------------

#' Home-style tile: slate square icon, title, status caption, optional lines.
#'
#' @param status  Caption shown under the title (e.g. "AVAILABLE", "PLANNED").
#' @param lines   Character vector: first line normal, the rest muted.
#' @param planned TRUE greys out the icon.
status_tile <- function(title, status, lines = NULL, icon_name = "check",
                        planned = FALSE) {
  body <- if (length(lines) > 0L)
    div(class = "tile-lines",
        div(lines[1L]),
        lapply(lines[-1L], function(l) div(class = "tile-line-muted", l)))
  div(class = "status-tile",
      div(class = "tile-head",
          div(class = paste("tile-icon", if (planned) "is-planned"),
              icon(icon_name)),
          div(div(class = "tile-title", title),
              div(class = "tile-status", status))),
      body)
}


# ---- step list -------------------------------------------------------------------

#' Step list: circle check, title, subtitle, status and time per row, with a
#' "1 - n" count at the bottom right.
#'
#' @param steps data.frame with columns title, subtitle, status, time and
#'   done (logical; FALSE shows an empty circle).
step_list <- function(steps) {
  n <- nrow(steps)
  rows <- lapply(seq_len(n), function(i) {
    s <- steps[i, ]
    div(class = "step-row",
        div(class = paste("step-icon", if (!isTRUE(s$done)) "is-pending"),
            if (isTRUE(s$done)) icon("check")),
        div(div(class = "step-title", s$title),
            div(class = "step-sub", s$subtitle)),
        div(class = "step-status", s$status),
        div(class = "step-time", s$time))
  })
  div(class = "step-list", rows,
      div(class = "step-footer", if (n > 0L) sprintf("1 - %d", n) else "0"))
}


#' Plain-language titles for the backend's stage_timings rows.
RUN_STAGE_LABELS <- data.frame(
  stage = c("load_data", "filter_stratum_and_sample_plots", "tree_filtering",
            "load_products_and_prices", "load_precomputed_taper_profiles",
            "profile_vector_conversion", "under_bark_profiles",
            "candidate_generation",
            "price_joining", "dp_optimization", "summary_assembly"),
  title = c("Load data", "Sample plots", "Filter trees",
            "Products and prices", "Taper profiles", "Profile grid",
            "Bark", "Candidate logs", "Pricing", "Optimal bucking", "Summaries"),
  subtitle = c("Sanitized NFI plots and trees",
               "Selection filter and random plot sample",
               "DBH ≥ 5 cm and height ≥ 4.5 m",
               "Load and validate the price matrix",
               "Precomputed taperNOR profiles",
               "Profiles on the 10 cm grid",
               "Under-bark diameters (barkNOR)",
               "All eligible logs per stem",
               "Join candidates to the price matrix",
               "Dynamic programming per stem",
               "Tree, plot and product summaries"),
  stringsAsFactors = FALSE
)

#' Step rows for step_list() from a run's stage timings.
#'
#' Times are the start time plus cumulative stage durations. Candidate
#' generation, pricing and optimisation run interleaved per tree, so their
#' times are the end of the accumulated stage, not of a separate pass.
#'
#' @param stage_timings data.frame(stage, elapsed_seconds) from the backend,
#'   or NULL before the first run (all steps pending).
#' @param started_at POSIXct start of the run.
run_step_table <- function(stage_timings = NULL, started_at = NULL) {
  lab <- RUN_STAGE_LABELS
  if (is.null(stage_timings) || is.null(started_at))
    return(data.frame(title = lab$title, subtitle = lab$subtitle,
                      status = "Waiting", time = "", done = FALSE,
                      stringsAsFactors = FALSE))
  st   <- stage_timings[match(lab$stage, stage_timings$stage), ]
  secs <- ifelse(is.na(st$elapsed_seconds), 0, st$elapsed_seconds)
  ends <- started_at + cumsum(secs)
  data.frame(title    = lab$title,
             subtitle = sprintf("%s · %.2f s", lab$subtitle, secs),
             status   = "Done",
             time     = format(ends, "%H:%M:%S"),
             done     = TRUE, stringsAsFactors = FALSE)
}


# ---- tables ----------------------------------------------------------------------

#' reactable in the reference style: light row separators, sortable columns
#' and "1–10 of 355" page info at the bottom right.
#'
#' @param digits Round numeric columns to this many decimals (as the former
#'   renderTable(digits = ...)); NULL leaves them as they are.
#' @param details Optional reactable row-details function (index -> tag),
#'   for detail on demand (e.g. a tree's logs under its row).
#' @param columns Optional named list of reactable::colDef().
apter_reactable <- function(df, digits = NULL, page_size = 10L,
                            details = NULL, columns = NULL) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!is.null(digits)) {
    num <- vapply(df, function(x) is.double(x), logical(1))
    df[num] <- lapply(df[num], round, digits = digits)
  }
  t <- APTER_TOKENS
  reactable::reactable(
    df,
    class           = if (nrow(df) <= page_size) "apter-single-page",
    details         = details,
    columns         = columns,
    sortable        = TRUE,
    pagination      = TRUE,
    showPagination  = TRUE,
    showPageInfo    = TRUE,
    defaultPageSize = page_size,
    paginationType  = "simple",
    highlight       = TRUE,
    compact         = TRUE,
    wrap            = FALSE,
    resizable       = TRUE,
    theme = reactable::reactableTheme(
      color            = t$ink,
      backgroundColor  = "#FFFFFF",
      borderColor      = t$border,
      highlightColor   = "#F6F5F3",
      cellPadding      = "9px 10px",
      style            = list(fontFamily = "Inter, system-ui, sans-serif"),
      headerStyle      = list(fontWeight = 500, color = t$ink_muted,
                              fontSize = "0.8rem",
                              borderBottom = paste("1px solid", t$border)),
      paginationStyle  = list(color = t$ink_muted)
    ),
    language = reactable::reactableLang(
      pageInfo     = "{rowStart} - {rowEnd} of {rows}",
      pagePrevious = "‹", pageNext = "›",
      noData       = "No rows"
    )
  )
}


# ---- base-R plots ----------------------------------------------------------------

#' Set plot chrome to the app style; returns the old par() for on.exit().
apter_par <- function(mar = c(4, 5, 3, 1)) {
  ch <- APTER_CHART
  graphics::par(mar = mar, family = "sans", bg = ch$surface,
                col.axis = ch$muted, col.lab = ch$muted, col.main = ch$ink,
                fg = ch$baseline, font.main = 1, cex.main = 1, cex.axis = 0.8,
                las = 1, bty = "n")
}

#' Bar chart with hairline grid, baseline at zero, no box, and value labels
#' in secondary ink (outside the bar end).
#'
#' @param vals   Named numeric vector (names are the bar labels).
#' @param col    One colour for every bar (single series).
#' @param fmt    Function formatting the value labels.
apter_barplot <- function(vals, col, main, ylab, fmt = format) {
  ch  <- APTER_CHART
  old <- apter_par(); on.exit(graphics::par(old))
  names(vals) <- gsub("_", "\n", names(vals))   # "spruce\nsawlog"
  rng <- range(c(0, vals), na.rm = TRUE)
  if (diff(rng) == 0) rng <- c(-1, 1)
  rng <- rng + c(-0.08, 0.15) * diff(rng) * (rng != 0)
  bp  <- graphics::barplot(vals, col = NA, border = NA, axes = FALSE,
                           ylim = rng, main = main, ylab = ylab, space = 0.9,
                           las = 1, cex.names = 0.8, padj = 0.6)
  graphics::abline(h = graphics::axTicks(2), col = ch$grid, lwd = 0.7)
  graphics::barplot(vals, col = col, border = NA, axes = FALSE, add = TRUE,
                    space = 0.9, names.arg = rep("", length(vals)))
  graphics::axis(2, at = graphics::axTicks(2), col = NA, col.ticks = NA,
                 labels = format(graphics::axTicks(2), big.mark = ",",
                                 scientific = FALSE, trim = TRUE))
  graphics::abline(h = 0, col = ch$baseline)
  graphics::text(bp, vals, labels = fmt(vals), pos = ifelse(vals >= 0, 3, 1),
                 col = ch$ink2, cex = 0.75, xpd = TRUE)
  invisible(bp)
}


#' Stem diagram: default vs. scenario bucking of one tree, along the stem.
#'
#' Two horizontal bars (default above, scenario below); each log is a block
#' from StartPos to StopPos coloured by assortment, with a 2 px surface gap
#' and a text label (length in dm) so colour is never the only cue.
#'
#' @param logs_default,logs_scenario Selected logs of one tree
#'   (StartPos, StopPos in cm; ProductType; LengthClass_dm).
#' @param height_m Tree height (m), for the x-axis extent.
#' @param title    Plot title.
apter_stem_compare <- function(logs_default, logs_scenario, height_m, title) {
  ch  <- APTER_CHART
  # Custom assortment classes share the neutral fill (see theme.R: only three
  # categorical colours pass when any two products can touch); their label
  # names the class.
  types  <- unique(as.character(c(logs_default$ProductType, logs_scenario$ProductType)))
  custom <- length(setdiff(types, names(ch$assortment))) > 0L
  old <- apter_par(mar = c(3.5, 6.5, 3, 1)); on.exit(graphics::par(old))
  x_max <- max(c(height_m, logs_default$StopPos / 100, logs_scenario$StopPos / 100), na.rm = TRUE)
  graphics::plot(NA, xlim = c(0, x_max), ylim = c(0.4, 2.9), axes = FALSE,
                 xlab = "", ylab = "", main = title)
  graphics::abline(v = graphics::axTicks(1), col = ch$grid, lwd = 0.7)
  graphics::axis(1, col = NA, col.ticks = NA)
  graphics::mtext("Height along the stem (m)", side = 1, line = 2.2,
                  col = ch$muted, cex = 0.8)
  graphics::axis(2, at = c(2, 1), labels = c("Default", "Scenario"),
                 col = NA, col.ticks = NA, col.axis = ch$ink)
  draw <- function(l, y) {
    graphics::rect(0, y - 0.32, height_m, y + 0.32, col = "#F1EFED", border = NA)
    if (nrow(l) == 0L) return(invisible())
    fill <- ch$assortment[as.character(l$ProductType)]
    fill[is.na(fill)] <- ch$neutral
    graphics::rect(l$StartPos / 100, y - 0.32, l$StopPos / 100, y + 0.32,
                   col = fill, border = ch$surface, lwd = 2)
    is_custom <- !as.character(l$ProductType) %in% names(ch$assortment)
    lab <- ifelse(is.na(l$LengthClass_dm), "", paste0(l$LengthClass_dm))
    lab[is_custom] <- paste(l$ProductType[is_custom], lab[is_custom])
    wide <- (l$StopPos - l$StartPos) / 100 > x_max / 25 * ifelse(is_custom, 3, 1)
    graphics::text(((l$StartPos + l$StopPos) / 200)[wide], y, lab[wide],
                   col = "#FFFFFF", cex = 0.75, font = 2)
  }
  draw(logs_default, 2); draw(logs_scenario, 1)
  leg <- c(ch$assortment, if (custom) c("custom assortments" = ch$neutral))
  graphics::legend("top", horiz = TRUE, bty = "n", inset = c(0, -0.02),
                   legend = names(leg), fill = leg,
                   border = NA, text.col = ch$ink, cex = 0.8, xpd = TRUE)
  graphics::mtext(if (custom) "Labels: log length (dm); custom: class and length"
                  else "Labels: log length (dm)", side = 3, line = 0.1, adj = 1,
                  col = ch$muted, cex = 0.7)
}

#' rhandsontable renderer: highlight scenario cells that differ from the
#' default grid passed as params$def (row-major), class "pm-diff".
PM_DIFF_RENDERER <- "
function(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.NumericRenderer.apply(this, arguments);
  td.classList.remove('pm-diff');
  var d = instance.params && instance.params.def;
  if (d && d[row] !== undefined) {
    var dv = d[row][col];
    var empty = (value === null || value === '' || value === undefined);
    var same = (dv === null && empty) ||
               (dv !== null && !empty && Math.abs(Number(value) - dv) <= 1e-9);
    if (!same) td.classList.add('pm-diff');
  }
  return td;
}"

#' rhandsontable renderer for the read-only default: mark interpolated cells
#' (params$interp, row-major logicals), class "pm-interp".
PM_INTERP_RENDERER <- "
function(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.NumericRenderer.apply(this, arguments);
  td.classList.remove('pm-interp');
  var m = instance.params && instance.params.interp;
  if (m && m[row] !== undefined && m[row][col]) td.classList.add('pm-interp');
  return td;
}"
