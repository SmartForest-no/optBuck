# app/helpers_ui.R
#
# Purpose : UI building blocks in the ForestSens / Redwood style (cards,
#           read-only fields, tables, plots).
#           Pure functions returning tags or widgets; styles in www/apter.css,
#           colours in theme.R.


# ---- page chrome ---------------------------------------------------------------

#' Navbar brand: small icon and the name shown in the app ("optBuck";
#' the code and repository keep the name Apter).
APP_DISPLAY_NAME <- "optBuck"
navbar_brand <- function() {
  tags$span(tags$img(src = "apter-icon.svg", alt = ""), APP_DISPLAY_NAME)
}


#' Asset URL with a version tag (file modification time), so a browser never
#' keeps an old copy of the stylesheet or script after an update.
asset_url <- function(file) {
  t <- file.mtime(file.path(APP_DIR, "www", file))
  if (is.na(t)) file else sprintf("%s?v=%s", file, format(t, "%Y%m%d%H%M%S"))
}

#' SmartForest logo for the right end of the navbar: exactly as tall as the
#' optBuck icon (26 px). The height is also set on the element itself, so the
#' 2560 x 595 px image never shows at its natural size, even without the
#' stylesheet.
NAVBAR_ICON_PX <- 26L
navbar_logo <- function()
  tags$img(src = asset_url("smartforest_logo.jpg"), alt = "SmartForest", class = "navbar-logo",
           height = NAVBAR_ICON_PX)


# ---- language (Phase 1, 2026-10-27) ------------------------------------------------
# Static text is a span carrying its id (and arguments): the page is built in
# APP_LANG, and www/apter.js swaps the text when the viewer switches language.
# The server renders its own outputs with tr(..., lang = lang()).

#' A static text by id (R/helpers_i18n.R), switchable in the browser.
t_ <- function(id, ...) {
  args <- list(...)
  tags$span(`data-i18n` = id,
            `data-i18n-args` = if (length(args))
              as.character(jsonlite::toJSON(unname(lapply(args, as.character)), auto_unbox = FALSE)),
            tr(id, ..., lang = APP_LANG))
}

#' Both languages' `ui` texts for the browser, and the start language.
i18n_head <- function() {
  t <- i18n_table(); u <- t[t$kind == "ui", , drop = FALSE]
  dict <- lapply(stats::setNames(APTER_LANGS, APTER_LANGS),
                 function(l) as.list(stats::setNames(u[[l]], u$id)))
  json <- gsub("</", "<\\/", as.character(jsonlite::toJSON(dict, auto_unbox = TRUE)), fixed = TRUE)
  tagList(
    tags$meta(name = "apter-lang", content = APP_LANG),
    tags$script(id = "apter-i18n", type = "application/json", HTML(json)))
}

#' The language switch in the navbar: Norsk | English.
lang_switch <- function() {
  div(class = "lang-switch", role = "group", `aria-label` = "Språk / Language",
      tags$button(type = "button", class = "lang-btn", `data-lang` = "no", lang = "nb", "Norsk"),
      tags$button(type = "button", class = "lang-btn", `data-lang` = "en", lang = "en", "English"))
}

#' fileInput with a switchable button and placeholder.
apter_file_input <- function(id, label, accept = NULL, width = "100%") {
  htmltools::tagQuery(
    fileInput(id, label, accept = accept, width = width,
              buttonLabel = t_("file_browse"), placeholder = tr("file_none", lang = APP_LANG))
  )$find("input.form-control")$addAttrs(`data-i18n-ph` = "file_none")$allTags()
}

#' A file input rendered by the server (modals, renderUI), in `lang`.
file_input_lang <- function(id, label, accept, lang, width = "100%", multiple = FALSE)
  fileInput(id, label, accept = accept, width = width, multiple = multiple,
            buttonLabel = tr("file_browse", lang = lang), placeholder = tr("file_none", lang = lang))


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


# ---- tables ----------------------------------------------------------------------

#' reactable in the reference style: light row separators, sortable columns
#' and "1–10 of 355" page info at the bottom right.
#'
#' @param digits Round numeric columns to this many decimals (as the former
#'   renderTable(digits = ...)); NULL leaves them as they are.
#' @param details Optional reactable row-details function (index -> tag),
#'   for detail on demand (e.g. a tree's logs under its row).
#' @param columns Optional named list of reactable::colDef().
#' @param row_class Optional function(index) -> CSS class of the row.
#' @param selection "single" for row selection (getReactableState()).
#' @param sortable FALSE for page slices of a server-side pager.
#' @param on_click Optional reactable::JS() row-click handler (overrides the
#'   "select" click of `selection`).
#'
#' Columns are sized to their content (Phase 4d): the width fits the longest
#' value and the longest word of the header, headers wrap, and the table is
#' only as wide as its columns.
apter_reactable <- function(df, digits = NULL, page_size = 10L,
                            details = NULL, columns = NULL, row_class = NULL,
                            selection = NULL, sortable = TRUE, pagination = TRUE,
                            on_click = NULL, lang = "en") {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!is.null(digits)) {
    num <- vapply(df, function(x) is.double(x), logical(1))
    df[num] <- lapply(df[num], round, digits = digits)
  }
  columns <- .fit_columns(df, columns)
  if (identical(lang, "no")) columns <- .decimal_comma(df, columns)
  t <- APTER_TOKENS
  reactable::reactable(
    df,
    class           = if (!pagination || nrow(df) <= page_size) "apter-single-page",
    details         = details,
    columns         = columns,
    rowClass        = row_class,
    selection       = selection,
    onClick         = if (!is.null(on_click)) on_click else if (!is.null(selection)) "select",
    fullWidth       = FALSE,
    sortable        = sortable,
    pagination      = pagination,
    showPagination  = pagination,
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
      borderColor      = t$table_row,
      highlightColor   = t$panel,
      cellPadding      = "9px 10px",
      style            = list(fontFamily = "Inter, system-ui, sans-serif"),
      headerStyle      = list(fontWeight = 500, color = t$ink_muted, background = t$panel,
                              fontSize = "0.8rem", whiteSpace = "normal",
                              borderBottom = paste("1px solid", t$table_head)),
      paginationStyle  = list(color = t$ink_muted)
    ),
    language = reactable::reactableLang(
      pageInfo     = tr("rt_page_info", lang = lang),
      pagePrevious = "‹", pageNext = "›",
      noData       = tr("rt_no_rows", lang = lang)
    )
  )
}


# Norsk: numbers with a decimal comma (and the value's own decimals), drawn
# from the numeric value, so sorting stays numeric. NA shows the column's
# `na` text.
.decimal_comma <- function(df, columns) {
  for (n in names(df)) {
    if (!is.numeric(df[[n]])) next
    cd <- columns[[n]]
    if (!is.null(cd$cell)) next
    cd$cell <- local({
      na <- or_default(cd$na, "")
      function(value) if (is.na(value)) na
                      else format(value, decimal.mark = ",", big.mark = "", scientific = FALSE, trim = TRUE)
    })
    columns[[n]] <- cd
  }
  columns
}

# Column widths (px) from the content: the longest value (of up to 2,000
# rows) and the header (its longest word, and at most two lines).
.fit_columns <- function(df, columns = NULL) {
  if (is.null(columns)) columns <- list()
  rows <- utils::head(df, 2000L)
  for (n in names(df)) {
    v <- rows[[n]]
    txt <- if (is.numeric(v)) format(v, big.mark = "", trim = TRUE) else as.character(v)
    w_val <- if (length(txt)) max(nchar(txt), na.rm = TRUE) else 0
    w_val <- if (is.finite(w_val)) w_val else 0
    # The header wraps to at most two lines and is never cut off.
    w_hdr <- max(nchar(strsplit(n, "[ ]")[[1L]]), ceiling(nchar(n) / 2) + 1, 0)
    w <- max(56, ceiling(7.4 * max(w_val, w_hdr)) + 26)
    cd <- columns[[n]]
    if (is.null(cd)) cd <- reactable::colDef()
    if (is.null(cd$width) && is.null(cd$minWidth)) cd$width <- w
    columns[[n]] <- cd
  }
  columns
}


# ---- server-side paged tables --------------------------------------------------
# reactable 0.4.5 sends every row to the browser; results with tens of
# thousands of rows (all plots of a stratum) are paged on the server
# instead: one page is sent at a time, sorting happens on the server, and
# the full table is a CSV download.

#' UI of a paged table: sort, previous/next, page info, CSV download.
paged_table_ui <- function(id) {
  tagList(
    div(class = "pager-bar",
        selectInput(paste0(id, "_sort"), NULL, choices = character(0), width = "260px"),
        actionButton(paste0(id, "_prev"), t_("btn_prev"), class = "btn-secondary btn-sm"),
        span(class = "pager-info", textOutput(paste0(id, "_info"), inline = TRUE)),
        actionButton(paste0(id, "_next"), t_("btn_next"), class = "btn-secondary btn-sm"),
        downloadButton(paste0(id, "_csv"), t_("btn_dl_all_csv"), class = "btn-secondary btn-sm")),
    reactable::reactableOutput(id))
}

#' Server of a paged table.
#'
#' @param data Reactive data.frame (readable column names, in the language of
#'   the moment), in its original order.
#' @param lang Reactive language: sort labels, page info, numbers and the CSV
#'   format. `filename` may be a function(lang).
#' @param selection TRUE: single-row selection; the return value then gives
#'   the selected row of data() (NULL when none).
#' @return Reactive: the selected row index in data(), or NULL.
paged_table_server <- function(input, output, session, id, data, page_size = 25L,
                               selection = FALSE, filename = paste0(id, ".csv"), digits = NULL,
                               lang = reactive("en")) {
  page <- reactiveVal(1L)
  # The sort choice is the column's position and direction, so it survives
  # a language switch (only the labels change).
  observeEvent(list(names(data()), lang()), {
    n <- names(data()); j <- seq_along(n)
    cur <- isolate(input[[paste0(id, "_sort")]])
    ch <- c(stats::setNames("", tr("sort_original", lang = lang())),
            stats::setNames(c(paste0(j, "|asc"), paste0(j, "|desc")), c(paste(n, "↑"), paste(n, "↓"))))
    updateSelectInput(session, paste0(id, "_sort"), choices = ch,
                      selected = if (!is.null(cur) && cur %in% ch) cur else "")
  })
  # New data starts at page 1; the same data in another language keeps its page.
  seen_lang <- NULL
  observeEvent(data(), {
    l <- isolate(lang())
    if (identical(seen_lang, l)) page(1L)
    seen_lang <<- l
  })
  ord <- reactive({
    df <- data(); s <- input[[paste0(id, "_sort")]]
    if (is.null(s) || !nzchar(s)) return(seq_len(nrow(df)))
    p <- strsplit(s, "|", fixed = TRUE)[[1L]]; j <- suppressWarnings(as.integer(p[1L]))
    if (is.na(j) || j > ncol(df)) return(seq_len(nrow(df)))
    order(df[[j]], decreasing = identical(p[2L], "desc"), na.last = TRUE, method = "radix")
  })
  observeEvent(input[[paste0(id, "_sort")]], page(1L), ignoreInit = TRUE)
  n_pages <- reactive(max(1L, ceiling(nrow(data()) / page_size)))
  observeEvent(input[[paste0(id, "_prev")]], page(max(1L, page() - 1L)))
  observeEvent(input[[paste0(id, "_next")]], page(min(n_pages(), page() + 1L)))
  rows <- reactive({
    o <- ord(); p <- min(page(), n_pages())
    o[seq.int((p - 1L) * page_size + 1L, length.out = min(page_size, length(o) - (p - 1L) * page_size))]
  })
  output[[paste0(id, "_info")]] <- renderText({
    n <- nrow(data()); r <- rows(); l <- lang()
    if (n == 0L) tr("rt_no_rows", lang = l)
    else tr("pager_info", fmt_num((page() - 1L) * page_size + 1L, lang = l),
            fmt_num((page() - 1L) * page_size + length(r), lang = l), fmt_num(n, lang = l), lang = l)
  })
  output[[id]] <- reactable::renderReactable({
    df <- data()
    apter_reactable(df[rows(), , drop = FALSE], digits = digits, page_size = page_size,
                    sortable = FALSE, pagination = FALSE,
                    selection = if (selection) "single", lang = lang())
  })
  output[[paste0(id, "_csv")]] <- downloadHandler(
    filename = function() if (is.function(filename)) filename(lang()) else filename,
    content  = function(file) write_download_csv(data()[ord(), , drop = FALSE], file, lang()))
  reactive({
    if (!selection) return(NULL)
    sel <- reactable::getReactableState(id, "selected", session = session)
    if (is.null(sel) || length(sel) == 0L) return(NULL)
    r <- rows()
    if (sel[1L] > length(r)) return(NULL)
    r[sel[1L]]
  })
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


#' Horizontal bars, one row per assortment, sorted by value (largest on top),
#' so every assortment name is shown however many there are (2026-10-22:
#' .hpr files bring 17 or more). Value labels sit at the bar ends, where
#' they fit: the figure's height gives every bar a line of its own
#' (hbar_height()), so labels never overlap, and the value axis keeps room
#' for them. A missing bar (NA) gets no label.

#' Plot height (px) for `n` rows; `ab`: two bars (A and B) per row.
hbar_height <- function(n, ab = FALSE) 100 + max(n, 1L) * if (ab) 38 else 28

.hbar_fmt_axis <- function(lang) function(v)
  format(v, big.mark = if (lang == "no") "\u00a0" else ",", decimal.mark = if (lang == "no") "," else ".",
         scientific = FALSE, trim = TRUE)

#' Single-series horizontal bars (e.g. value change B - A): positive values
#' right of zero, negative left; sorted with the largest value on top.
#'
#' @param vals   Numeric vector; labels: one name per value.
#' @param fmt    Function formatting the value labels.
apter_hbar <- function(vals, labels, title, xlab, fmt = format, col = APTER_CHART$neutral, lang = "en") {
  ch <- APTER_CHART
  o <- order(vals)                                   # bottom to top: smallest first
  d <- data.frame(name = factor(labels[o], levels = unique(labels[o])), value = vals[o])
  d$lab <- fmt(d$value)
  d$hjust <- ifelse(d$value >= 0, -0.12, 1.12)
  neg <- any(d$value < 0); pos <- any(d$value > 0)
  ggplot2::ggplot(d, ggplot2::aes(x = value, y = name)) +
    ggplot2::geom_col(fill = col, width = 0.7) +
    ggplot2::geom_vline(xintercept = 0, colour = ch$baseline, linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = lab, hjust = hjust), colour = ch$ink2, size = 3,
                       family = APTER_PLOT_FONT) +
    ggplot2::scale_x_continuous(labels = .hbar_fmt_axis(lang),
                                expand = ggplot2::expansion(mult = c(if (neg) 0.2 else 0.02, if (pos) 0.2 else 0.02))) +
    ggplot2::labs(x = xlab, y = NULL, title = title) +
    theme_apter() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = ch$grid, linewidth = 0.4),
                   axis.line.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = ch$ink, size = ggplot2::rel(0.9)))
}

#' Scenario A and B side by side per assortment, horizontal, sorted by the
#' larger of the two values (largest on top). NA (not in that scenario):
#' no bar and no label.
apter_hbar_ab <- function(a, b, labels, title, xlab, fmt = format, lang = "en") {
  ch <- APTER_CHART
  sc <- tr(c("scenario_a", "scenario_b"), lang = lang)
  o <- order(pmax(a, b, na.rm = TRUE))
  lv <- unique(labels[o])
  d <- rbind(data.frame(name = labels, value = a, Scenario = sc[1L]),
             data.frame(name = labels, value = b, Scenario = sc[2L]))
  d <- d[!is.na(d$value), , drop = FALSE]
  d$name <- factor(d$name, levels = lv)
  # B first in the factor so that A is the upper bar of each pair.
  d$Scenario <- factor(d$Scenario, levels = rev(sc))
  d$lab <- fmt(d$value)
  dodge <- ggplot2::position_dodge(width = 0.8, preserve = "single")
  ggplot2::ggplot(d, ggplot2::aes(x = value, y = name, fill = Scenario)) +
    ggplot2::geom_col(position = dodge, width = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = lab, group = Scenario), position = dodge, hjust = -0.12,
                       colour = ch$ink2, size = 2.8, family = APTER_PLOT_FONT) +
    ggplot2::scale_fill_manual(values = stats::setNames(c(ch$series1, ch$series2), sc), breaks = sc) +
    ggplot2::scale_x_continuous(labels = .hbar_fmt_axis(lang), limits = c(0, NA),
                                expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(x = xlab, y = NULL, title = title) +
    theme_apter() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(colour = ch$grid, linewidth = 0.4),
                   axis.line.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(colour = ch$ink, size = ggplot2::rel(0.9)))
}


#' Stem diagram: the bucking of one tree under scenarios A and B.
#'
#' Two horizontal bars (A above, B below); each log is a block from StartPos
#' to StopPos coloured by assortment, with a 2 px surface gap and a text label
#' (length in dm) so colour is never the only cue.
#'
#' @param logs_default,logs_scenario Selected logs of one tree under A and B
#'   (StartPos, StopPos in cm; ProductType; LengthClass_dm). logs_scenario
#'   NULL (a single run): one bar, Scenario A (2026-10-23).
#' @param height_m Tree height (m), for the x-axis extent.
#' @param title    Plot title.
#' @param lang     Language of the axis, legend and note.
apter_stem_compare <- function(logs_default, logs_scenario, height_m, title, lang = "en") {
  ch  <- APTER_CHART
  # Custom assortment classes share the charcoal "other" fill (theme.R); their
  # label names the class.
  types  <- unique(as.character(c(logs_default$ProductType, logs_scenario$ProductType)))
  custom <- length(setdiff(types, names(ch$assortment))) > 0L
  old <- apter_par(mar = c(3.5, 7.5, 3, 1)); on.exit(graphics::par(old))
  x_max <- max(c(height_m, logs_default$StopPos / 100, logs_scenario$StopPos / 100), na.rm = TRUE)
  two <- !is.null(logs_scenario)
  graphics::plot(NA, xlim = c(0, x_max), ylim = if (two) c(0.4, 2.9) else c(1.2, 2.9), axes = FALSE,
                 xlab = "", ylab = "", main = title)
  graphics::abline(v = graphics::axTicks(1), col = ch$grid, lwd = 0.7)
  graphics::axis(1, col = NA, col.ticks = NA)
  graphics::mtext(tr("fig_height_along_stem", lang = lang), side = 1, line = 2.2,
                  col = ch$muted, cex = 0.8)
  sc <- tr(c("scenario_a", "scenario_b"), lang = lang)
  graphics::axis(2, at = if (two) c(2, 1) else 2, labels = if (two) sc else sc[1L],
                 col = NA, col.ticks = NA, col.axis = ch$ink)
  draw <- function(l, y) {
    graphics::rect(0, y - 0.32, height_m, y + 0.32, col = APTER_TOKENS$panel, border = NA)
    if (nrow(l) == 0L) return(invisible())
    type <- as.character(l$ProductType)
    is_custom <- !type %in% names(ch$assortment)
    slot <- ifelse(is_custom, "other", type)
    fill <- c(ch$assortment, other = ch$other)[slot]
    graphics::rect(l$StartPos / 100, y - 0.32, l$StopPos / 100, y + 0.32,
                   col = fill, border = ch$surface, lwd = 2)
    lab <- ifelse(is.na(l$LengthClass_dm), "", paste0(l$LengthClass_dm))
    lab[is_custom] <- paste(tr_value(l$ProductType[is_custom], lang), lab[is_custom])
    wide <- (l$StopPos - l$StartPos) / 100 > x_max / 25 * ifelse(is_custom, 3, 1)
    graphics::text(((l$StartPos + l$StopPos) / 200)[wide], y, lab[wide],
                   col = ch$label_on[slot][wide], cex = 0.75, font = 2)
  }
  draw(logs_default, 2); if (two) draw(logs_scenario, 1)
  leg <- c(ch$assortment, if (custom) c(custom = ch$other))
  names(leg) <- tr(paste0("leg_", names(leg)), lang = lang)
  graphics::legend("top", horiz = TRUE, bty = "n", inset = c(0, -0.02),
                   legend = names(leg), fill = leg,
                   border = NA, text.col = ch$ink, cex = 0.8, xpd = TRUE)
  graphics::mtext(tr(if (custom) "fig_stem_note_custom" else "fig_stem_note", lang = lang),
                  side = 3, line = 0.1, adj = 1,
                  col = ch$muted, cex = 0.7)
}

#' rhandsontable renderer: highlight cells that differ from the other
#' scenario's grid passed as params$def (row-major; NA where the other
#' scenario has no such cell), class "pm-diff".
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


# ---- ggplot2 figures (layout-and-figures) -------------------------------------------
# Styled by theme_apter() (theme.R): Inter, white background, light grid.

#' DBH and height distributions side by side (2026-10-23, Trees tab): two
#' panels of overlapping density curves per species (alpha 0.45, thin
#' outline in the species colour, legend when both species are present),
#' each on its own axis.
#'
#' @param dbh,height Numeric per tree (cm, m); species: "spruce"/"pine".
#' @param height_lbl Panel title of the heights ("Height (m)", or "Measured
#'   length (m)" for .hpr stems), in `lang`.
apter_distribution_figure <- function(dbh, height, species, height_lbl = "Height (m)", lang = "en") {
  panels <- c(tr("col_dbh_cm", lang = lang), height_lbl)
  sp <- factor(tools::toTitleCase(species), levels = names(APTER_SPECIES))
  d <- rbind(data.frame(panel = panels[1], x = dbh, Species = sp),
             data.frame(panel = panels[2], x = height, Species = sp))
  d$panel <- factor(d$panel, panels)
  d <- d[is.finite(d$x) & !is.na(d$Species), , drop = FALSE]
  n <- table(d$panel, d$Species)                         # a density needs two values
  d <- d[n[cbind(as.character(d$panel), as.character(d$Species))] >= 2L, , drop = FALSE]
  if (nrow(d) == 0L) return(NULL)
  two_sp <- length(unique(d$Species)) > 1L
  ggplot2::ggplot(d, ggplot2::aes(x = x, fill = Species, colour = Species)) +
    ggplot2::geom_density(alpha = 0.45, linewidth = 0.5) +
    ggplot2::facet_wrap(ggplot2::vars(panel), nrow = 1L, scales = "free") +
    ggplot2::scale_fill_manual(values = APTER_SPECIES, drop = TRUE, labels = function(x) tr_value(x, lang)) +
    ggplot2::scale_colour_manual(values = APTER_SPECIES, drop = TRUE) +
    ggplot2::scale_x_continuous(labels = .hbar_fmt_axis(lang)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), labels = .hbar_fmt_axis(lang)) +
    ggplot2::guides(fill = if (two_sp) ggplot2::guide_legend(title = tr("col_species", lang = lang)) else "none",
                    colour = "none") +
    ggplot2::labs(x = NULL, y = tr("fig_density", lang = lang), title = tr("fig_distributions", lang = lang)) +
    theme_apter()
}

#' NOK per m³ by DBH class (2026-10-23: lines only; the tree counts are in
#' the distribution figure): NOK/m³ per class and species, points and lines;
#' Scenario A solid, B dashed after a comparison.
#'
#' @param a,b dbh_class_summary() of Scenario A and (optionally) B.
apter_dbh_class_figure <- function(a, b = NULL, lang = "en") {
  scn <- tr(c("scenario_a", "scenario_b"), lang = lang)
  cls <- function(d) d[d$dbh_class != "All", , drop = FALSE]
  a <- cls(a); if (nrow(a) == 0L) return(NULL)
  lv <- unique(a$dbh_class[order(a$class_lo)])
  sp <- function(x) factor(tools::toTitleCase(x), levels = names(APTER_SPECIES))
  nok <- function(d, nm) data.frame(class = factor(d$dbh_class, lv), Species = sp(d$species_group),
                                    Scenario = nm, value = d$nok_per_m3)
  lines <- rbind(nok(a, scn[1L]), if (!is.null(b)) nok(cls(b), scn[2L]))
  lines <- lines[is.finite(lines$value), , drop = FALSE]
  if (nrow(lines) == 0L) return(NULL)
  lines$Scenario <- factor(lines$Scenario, scn)
  two_sp <- length(unique(lines$Species)) > 1L
  ggplot2::ggplot(lines, ggplot2::aes(x = class, y = value, colour = Species, linetype = Scenario,
                                      group = interaction(Species, Scenario))) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::scale_colour_manual(values = APTER_SPECIES, drop = TRUE, labels = function(x) tr_value(x, lang)) +
    ggplot2::scale_linetype_manual(values = stats::setNames(c("solid", "22"), scn), drop = TRUE) +
    ggplot2::scale_x_discrete(labels = function(x) i18n_decimals(x, lang)) +
    ggplot2::scale_y_continuous(labels = .hbar_fmt_axis(lang)) +
    ggplot2::guides(colour = if (two_sp) "legend" else "none",
                    linetype = if (!is.null(b)) "legend" else "none") +
    ggplot2::labs(x = tr("col_dbh_class_cm", lang = lang), y = tr("unit_nok_m3", lang = lang),
                  colour = NULL, linetype = NULL, title = tr("fig_nok_by_dbh_class", lang = lang)) +
    theme_apter()
}

#' Log length distribution (Logs tab): logs per length class (dm), stacked by
#' type group (Sawlog, Pulpwood, Other; waste left out). After a comparison
#' two panels, A above B, on shared axes.
#'
#' @param a,b log_length_distribution() of Scenario A and (optionally) B.
apter_log_length_figure <- function(a, b = NULL, lang = "en") {
  scn <- tr(c("scenario_a", "scenario_b"), lang = lang)
  d <- rbind(if (nrow(a)) cbind(a, Scenario = scn[1L]),
             if (!is.null(b) && nrow(b)) cbind(b, Scenario = scn[2L]))
  if (is.null(d) || nrow(d) == 0L) return(NULL)
  d$Scenario <- factor(d$Scenario, scn)
  ch <- APTER_CHART
  cols <- c(Sawlog = ch$assortment[["sawlog"]], Pulpwood = ch$assortment[["pulpwood"]], Other = ch$other)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = length_class_dm, y = logs, fill = type)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_manual(values = cols, breaks = names(cols)[names(cols) %in% d$type],
                               labels = function(x) tr_value(x, lang)) +
    ggplot2::scale_x_continuous(breaks = function(l) seq(ceiling(l[1L]), floor(l[2L]), by = 1),
                                labels = .hbar_fmt_axis(lang)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05)), labels = .hbar_fmt_axis(lang)) +
    ggplot2::labs(x = tr("col_length_class_dm", lang = lang), y = tr("tally_logs", lang = lang),
                  title = tr("fig_log_lengths", lang = lang)) +
    theme_apter()
  if (!is.null(b)) p <- p + ggplot2::facet_wrap(ggplot2::vars(Scenario), ncol = 1L)
  p
}
