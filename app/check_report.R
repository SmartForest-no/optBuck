# app/check_report.R
#
# Purpose : The tables, figures and the PDF report of tab "5 Apteringsanalyse" (roadmap 8, 2026-11-05).
#           chk_tables() and the two figures are used by the page (app/check.R) and by the PDF, so the
#           report shows the same sections as the screen. The PDF is drawn with grDevices::cairo_pdf
#           (A4, UTF-8, no LaTeX or pandoc), grid and ggplot2, with gridExtra::tableGrob for the tables
#           (gridExtra is in Suggests).
# Texts   : every text comes from inputs/i18n/translations.csv (ids chk_*).

#' The display tables of a bucking_check() result in language `lang`, as a named list of data.frames
#' (translated, formatted text) plus the notes shown under them.
chk_tables <- function(r, lang = "en") {
  L <- function(id, ...) tr(id, ..., lang = lang)
  num <- function(x, d = 0L) fmt_num(x, d, lang)
  pct <- function(x, d = 1L) ifelse(is.na(x), "–", fmt_num(100 * x, d, lang))
  sp_name <- function(x) unname(vapply(as.character(x), function(v) tr_value(tools::toTitleCase(v), lang), ""))
  pn <- function(x) unname(vapply(as.character(x), function(v) tr_value(v, lang), ""))
  df <- function(d, nm) { d <- as.data.frame(d, stringsAsFactors = FALSE); names(d) <- nm; rownames(d) <- NULL; d }
  out <- list()
  out$file <- df(data.frame(num(r$n_stems_file), num(if (r$no_profile) 0 else sum(r$stems$use)),
                            num(if (r$no_profile) r$n_stems_file else r$n_stems_file - sum(r$stems$use)), num(r$n_logs)),
                 c(L("chk_k_stems_file"), L("chk_k_stems_used"), L("chk_k_stems_out"), L("chk_k_logs")))
  # permitted quality codes per product: the StanForD ProductKey, the product, the species, digits only
  pgs <- r$overview$permitted_grades; smap <- r$overview$species_map
  digits <- function(g) { g <- sort(unique(g[!is.na(g)])); if (length(g)) paste(g, collapse = ", ") else "–" }
  if (!is.null(pgs) && nrow(pgs)) {
    pgs <- pgs[!pgs$ProductKey %in% r$overview$unclassified_keys, , drop = FALSE]
    if (nrow(pgs)) {
      g <- do.call(rbind, lapply(split(pgs, paste(pgs$ProductKey, pgs$species_key)), function(d) {
        sp <- unname(smap[d$species_key[1L]])
        data.frame(a = d$ProductKey[1L], b = pn(d$ProductName[1L]), c = if (is.na(sp) || sp == "none") "–" else sp_name(sp),
                   d = digits(d$grade), e = digits(d$grade[d$not_sawlog %in% TRUE]), stringsAsFactors = FALSE) }))
      g <- g[order(suppressWarnings(as.numeric(g$a)), g$c), , drop = FALSE]
      out$grades <- df(g, c(L("chk_c_pkey"), L("chk_c_product"), L("col_species"), L("chk_c_grades"), L("chk_c_nosaw")))
    }
  }
  m <- r$manual
  out$manual <- if (isTRUE(r$no_profile))
    df(data.frame(pct(m$all), pct(m$sawlogs), pct(m$butt_sawlogs)), c(L("chk_m_all"), L("chk_m_saw"), L("chk_m_butt")))
  else df(data.frame(pct(m$all), pct(m$sawlogs), pct(m$butt_sawlogs), pct(m$yield_with), pct(m$yield_without)),
          c(L("chk_m_all"), L("chk_m_saw"), L("chk_m_butt"), L("chk_m_with"), L("chk_m_without")))
  limits <- c("chk_lim_a", "chk_lim_b", "chk_lim_c", "chk_lim_e", "chk_lim_g", "chk_lim_i", "chk_lim_h")
  out$limits <- vapply(limits, L, "")
  if (isTRUE(r$no_profile)) {
    a <- r$assortments$recorded
    out$assort <- df(data.frame(pn(a$ProductName), num(a$logs), num(a$price_m3, 1L)), c(L("chk_c_product"), L("chk_c_logs"), L("chk_c_volume")))
    out$limits <- vapply("chk_lim_e", L, "")
    return(out)
  }
  kf <- r$key_figures
  out$keyfig <- df(data.frame(ifelse(seq_len(nrow(kf)) == 1L, L("chk_c_all"), ifelse(kf$scope == "without_broken", L("chk_c_nobroken"), sp_name(kf$scope))),
                              num(kf$stems), num(kf$value_rec), num(kf$value_opt), num(kf$value_per_m3_rec), num(kf$value_per_m3_opt), num(kf$lost), pct(kf$yield)),
                   c(L("chk_c_species"), L("chk_k_stems_used"), paste(L("chk_c_recorded"), "–", L("chk_c_value")), paste(L("chk_c_best"), "–", L("chk_c_value")),
                     paste(L("chk_c_recorded"), "–", L("chk_c_value_m3")), paste(L("chk_c_best"), "–", L("chk_c_value_m3")), L("chk_c_lost"), L("chk_c_degree")))
  out$left_out <- L("chk_left_out", num(r$n_left_out))
  a <- r$assortments
  at <- merge(a$recorded[, c("key", "ProductName", "logs", "price_m3", "value")], a$optimal[, c("key", "logs", "price_m3", "value")],
              by = "key", all = TRUE, suffixes = c("_r", "_o"))
  at$ProductName[is.na(at$ProductName)] <- r$pname[at$key[is.na(at$ProductName)]]
  at <- at[order(-ifelse(is.na(at$value_r), 0, at$value_r)), ]
  out$assort <- df(data.frame(pn(ifelse(at$key == "waste", "Unclassified", at$ProductName)), num(at$logs_r), num(at$logs_o), num(at$price_m3_r, 1L), num(at$price_m3_o, 1L),
                              num(at$value_r), num(at$value_o)),
                   c(L("chk_c_product"), paste(L("chk_c_logs"), L("chk_c_recorded")), paste(L("chk_c_logs"), L("chk_c_best")), paste(L("chk_c_volume"), L("chk_c_recorded")),
                     paste(L("chk_c_volume"), L("chk_c_best")), paste(L("chk_c_value"), L("chk_c_recorded")), paste(L("chk_c_value"), L("chk_c_best"))))
  out$sawshare <- paste0(L("chk_c_sawshare"), ": ", pct(a$saw_share_rec), " / ", pct(a$saw_share_opt))
  ld <- r$length_dist
  if (!is.null(ld$fg) && nrow(ld$fg))
    out$fg <- df(data.frame(pn(r$pname[ld$fg$key]), pct(ld$fg$fg_recorded / 100), pct(ld$fg$fg_best / 100), ifelse(is.na(ld$fg$maxdev), "–", num(ld$fg$maxdev, 1L))),
                 c(L("chk_c_product"), paste(L("chk_c_fit"), L("chk_c_recorded")), paste(L("chk_c_fit"), L("chk_c_best")), L("chk_c_maxdev")))
  d <- r$defect
  out$defect <- df(data.frame(pct(d$share_total), pct(d$share_dim), pct(d$share_butt_dim), pct(d$share_waste)),
                   c(L("chk_d_total"), L("chk_d_dim"), L("chk_d_butt"), L("chk_d_waste")))
  tp <- r$top
  tpd <- Reduce(function(x, y) merge(x, y, by = "sp", all = TRUE), list(
    setNames(tp$last_rec[, c("sp", "mean_cm")], c("sp", "a")), setNames(tp$last_opt[, c("sp", "mean_cm")], c("sp", "b")),
    setNames(tp$sawlast_rec[, c("sp", "mean_cm")], c("sp", "c")), setNames(tp$sawlast_opt[, c("sp", "mean_cm")], c("sp", "d"))))
  out$top <- df(data.frame(sp_name(tpd$sp), num(tpd$a, 1L), num(tpd$b, 1L), num(tpd$c, 1L), num(tpd$d, 1L)),
                c(L("chk_c_species"), paste(L("chk_t_last"), L("chk_c_recorded")), paste(L("chk_t_last"), L("chk_c_best")),
                  paste(L("chk_t_sawlast"), L("chk_c_recorded")), paste(L("chk_t_sawlast"), L("chk_c_best"))))
  out
}

# Family of the figures: the screen uses the app's Inter (registered with systemfonts for ragg); the
# PDF device is cairo, which knows the system fonts only.
.chk_font <- function(pdf) if (pdf) ggplot2::theme(text = ggplot2::element_text(family = "sans")) else ggplot2::theme()

#' Density of the per-stem bucking yield index (%), one line per species (colours and style of the DBH
#' density plots on 1 Stammer). NULL when there are too few stems.
chk_fig_density <- function(r, lang = "en", pdf = FALSE) {
  s <- r$stems[r$stems$use & is.finite(r$stems$yield), , drop = FALSE]
  d <- data.frame(Species = factor(tools::toTitleCase(s$species_group), levels = names(APTER_SPECIES)), x = 100 * s$yield)
  n <- table(d$Species); d <- d[n[as.character(d$Species)] >= 2L, , drop = FALSE]
  if (nrow(d) == 0L) return(NULL)
  ggplot2::ggplot(d, ggplot2::aes(x = x, fill = Species, colour = Species)) +
    ggplot2::geom_density(alpha = 0.45, linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = APTER_SPECIES, drop = TRUE, labels = function(x) tr_value(x, lang)) +
    ggplot2::scale_colour_manual(values = APTER_SPECIES, drop = TRUE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::guides(fill = if (length(unique(d$Species)) > 1L) ggplot2::guide_legend(title = tr("col_species", lang = lang)) else "none", colour = "none") +
    ggplot2::labs(title = tr("chk_fig_degree", lang = lang), x = tr("chk_ax_degree", lang = lang), y = tr("chk_ax_density", lang = lang)) +
    ggplot2::coord_cartesian(xlim = c(0, 130)) + theme_apter() + .chk_font(pdf)      # a few stems with a tiny best value reach several hundred %
}

#' Number of facets (assortments) of the length distribution figure.
chk_length_facets <- function(r) { t <- r$length_dist$tally; if (is.null(t) || nrow(t) == 0L) 0L else length(unique(t$key)) }

#' Logs per length class (dm), recorded and best possible, one facet per assortment.
chk_fig_length <- function(r, lang = "en", pdf = FALSE) {
  t <- r$length_dist$tally; if (is.null(t) || nrow(t) == 0L) return(NULL)
  t$name <- unname(vapply(r$pname[t$key], function(x) tr_value(x, lang), ""))
  long <- rbind(data.frame(t[c("name", "length_dm")], n = t$recorded, src = tr("chk_c_recorded", lang = lang)),
                data.frame(t[c("name", "length_dm")], n = t$best, src = tr("chk_c_best", lang = lang)))
  thin <- function(x) x[seq(1L, length(x), by = max(1L, ceiling(length(x) / 12L)))]      # readable x ticks
  ggplot2::ggplot(long, ggplot2::aes(factor(length_dm), n, fill = src)) +
    ggplot2::geom_col(position = "dodge", width = 0.8) + ggplot2::facet_wrap(~name, scales = "free", ncol = 1) +
    ggplot2::scale_fill_manual(values = setNames(c(APTER_CHART$series1, APTER_CHART$series2), c(tr("chk_c_recorded", lang = lang), tr("chk_c_best", lang = lang)))) +
    ggplot2::scale_x_discrete(breaks = thin) + ggplot2::scale_y_continuous(breaks = function(l) pretty(l, n = 4L)) +
    ggplot2::labs(x = tr("chk_ax_class", lang = lang), y = tr("chk_ax_logs", lang = lang), fill = NULL) + theme_apter() + .chk_font(pdf)
}

#' The report as a PDF in language `lang`: title, file name, date and the sections and figures of the
#' page (not the interactive per-stem table). Returns `file`.
chk_report_pdf <- function(r, lang, file, file_name, date = Sys.Date()) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) stop(tr("chk_pdf_needs", lang = lang))
  L <- function(id, ...) tr(id, ..., lang = lang)
  tb <- chk_tables(r, lang)
  grDevices::cairo_pdf(file, width = 8.27, height = 11.69, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  page_h <- 267; page_w <- 180                     # mm inside 15 mm margins
  y <- 0; grid::grid.newpage()
  place <- function(g, h, w = page_w) {
    if (y + h > page_h && y > 0) { grid::grid.newpage(); y <<- 0 }
    grid::pushViewport(grid::viewport(x = grid::unit(15, "mm"), y = grid::unit(297 - 15 - y, "mm"), width = grid::unit(w, "mm"),
                                      height = grid::unit(h, "mm"), just = c("left", "top")))
    grid::grid.draw(g); grid::popViewport(); y <<- y + h + 3
  }
  text <- function(s, size = 8.5, face = "plain", width = 105, colour = "#1F2421") {
    s <- unlist(lapply(s, function(x) strwrap(x, width = width * 9 / size)))
    h <- length(s) * size * 0.46 + 1
    place(grid::textGrob(paste(s, collapse = "\n"), x = 0, y = 1, just = c("left", "top"), gp = grid::gpar(fontsize = size, fontface = face, col = colour, lineheight = 1.1)), h)
  }
  heading <- function(id) { if (y + 14 > page_h && y > 0) { grid::grid.newpage(); y <<- 0 }; text(L(id), size = 11, face = "bold") }
  table <- function(d) {
    if (is.null(d) || nrow(d) == 0L) return(invisible(NULL))
    size <- 7.5; repeat {
      dd <- d; names(dd) <- vapply(names(d), function(s) paste(strwrap(s, 18), collapse = "\n"), "")
      g <- gridExtra::tableGrob(dd, rows = NULL, theme = gridExtra::ttheme_default(base_size = size, base_family = "sans",
             core = list(fg_params = list(hjust = 0, x = 0.04)), colhead = list(fg_params = list(hjust = 0, x = 0.04, fontface = "bold"))))
      w <- grid::convertWidth(sum(g$widths), "mm", valueOnly = TRUE); h <- grid::convertHeight(sum(g$heights), "mm", valueOnly = TRUE)
      if (w <= page_w || size <= 4.5) break; size <- size - 0.5
    }
    place(g, h, w)
  }
  figure <- function(p, h) if (!is.null(p)) place(ggplot2::ggplotGrob(p), h)

  text(L("chk_title"), size = 17, face = "bold")
  text(L("chk_pdf_file", file_name), size = 10)
  text(L("chk_pdf_date", format(date, "%d.%m.%Y")), size = 10)
  msgs <- unique(r$warnings); if (length(msgs)) text(vapply(msgs, tr_msg, "", lang = lang), colour = "#8A5A00")
  heading("chk_h_file"); table(tb$file)
  if (!is.null(tb$grades)) { heading("chk_h_overview"); table(tb$grades) }
  if (isTRUE(r$no_profile)) {
    text(L("chk_no_profile"))
    heading("chk_h_assort"); table(tb$assort)
    heading("chk_h_manual"); table(tb$manual)
  } else {
    heading("chk_h_degree"); table(tb$keyfig); text(tb$left_out, colour = "#6A726E")
    figure(chk_fig_density(r, lang, pdf = TRUE), 70)
    heading("chk_h_assort"); table(tb$assort); text(tb$sawshare, colour = "#6A726E")
    heading("chk_h_length")
    nf <- chk_length_facets(r); if (nf > 0L) figure(chk_fig_length(r, lang, pdf = TRUE), min(250, 12 + 38 * nf))
    table(tb$fg)
    heading("chk_h_manual"); table(tb$manual)
    heading("chk_h_defect"); table(tb$defect)
    heading("chk_h_top"); table(tb$top)
  }
  heading("chk_h_limits"); text(tb$limits)
  invisible(file)
}
