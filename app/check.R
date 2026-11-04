# app/check.R
#
# Purpose : The "5 Bucking check" tab (roadmap 8, 2026-11-04): how the harvester bucked the
#           stems of an .hpr file compared with the best possible bucking under the file's own
#           instruction (R/helpers_check.R, bucking_check()). The file is the first .hpr loaded
#           under 1 Stems, or one uploaded here for this tab only. The check runs as a background
#           job (ExtendedTask, run_check_async() in helpers_jobs.R) with a progress bar; the app
#           stays responsive (about 40-125 s for 3,000-10,000 stems on 15 workers).
# Texts   : every text comes from inputs/i18n/translations.csv (ids chk_*).

check_page <- nav_panel(
  t_("tab_check"), value = "check",
  card(card_body(p(class = "page-intro", t_("chk_intro")),
                 uiOutput("chk_main"),
                 uiOutput("chk_results"))))

#' @param hs_own  Reactive: the files uploaded under 1 Stems (list(name, path)) or NULL.
#' @param hs_map  Reactive: the species map of those stems (or an error when not ready).
check_server <- function(input, output, session, lang, hs_own, hs_map) {
  L <- function(id, ...) tr(id, ..., lang = lang())
  chk_own <- reactiveVal(NULL)             # a file uploaded for this tab only
  res <- reactiveVal(NULL)                 # bucking_check() result
  uploads <- character(0)
  session$onSessionEnded(function() unlink(uploads, recursive = TRUE))

  observeEvent(input$chk_upload, {
    f <- input$chk_upload; req(f)
    uploads <<- unique(c(uploads, dirname(f$datapath)))
    chk_own(list(name = f$name[1L], path = f$datapath[1L])); res(NULL)
  })
  observeEvent(input$chk_reset, { chk_own(NULL); res(NULL) })

  # The file: an upload for this tab, else the first file loaded under 1 Stems.
  chk_file <- reactive({
    o <- chk_own()
    if (!is.null(o)) return(list(name = o$name, path = o$path, src = "own"))
    s <- hs_own()
    if (!is.null(s)) list(name = s$name[1L], path = s$path[1L], src = "stems")
  })

  output$chk_main <- renderUI({
    f <- chk_file()
    tagList(
      div(class = "note-muted", L("chk_note_own")),
      if (is.null(f)) div(class = "note-muted", L("chk_file_none"))
      else div(class = "chk-file", strong(L("chk_file_label")), ": ",
               L(if (f$src == "own") "chk_file_own" else "chk_file_stems", f$name)),
      checkboxInput("chk_used_only", L("chk_used_only"), value = TRUE),
      div(class = "note-muted", L("chk_used_only_note")),
      div(class = "pm-actions",
          fileInput("chk_upload", NULL, accept = c(".hpr", ".gz", ".zip"), buttonLabel = L("chk_upload"),
                    placeholder = "", width = "320px"),
          if (!is.null(chk_own())) actionButton("chk_reset", L("chk_reset"), class = "btn-secondary btn-sm"),
          if (!is.null(f)) actionButton("chk_run", L("chk_run"), class = "btn-primary")))
  })

  # The check as a background job; the file name travels with the task so a late result is
  # never shown for another file.
  chk_prog <- reactiveVal(NULL)
  chk_task <- ExtendedTask$new(function(spec) {
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    l <- isolate(lang())
    steps <- c(read = 0.05, opt = 0.15, val = 0.8, tab = 0.9)
    texts <- c(read = "chk_p_read", opt = "chk_p_opt", val = "chk_p_val", tab = "chk_p_tab")
    prog$set(message = tr("chk_p_read", lang = l), value = 0)
    p <- run_check_async(spec, progress = function(step, done, total) {
      v <- steps[[step]]
      if (step == "opt" && !is.na(total) && total > 0L) v <- 0.15 + 0.65 * done / total
      prog$set(message = tr(texts[[step]], lang = l), value = v)
    })
    p <- promises::then(p, function(r) { prog$set(message = tr("chk_p_tab", lang = l), value = 0.9); r$file_name <- spec$name; r })
    p <- promises::catch(p, function(e)
      list(ok = FALSE, errors = sprintf("The check failed: %s", conditionMessage(e)), warnings = character(0), file_name = spec$name))
    promises::finally(p, function() prog$close())
  })
  observe(updateActionButton(session, "chk_run", disabled = chk_task$status() == "running"))
  observeEvent(input$chk_run, {
    f <- chk_file(); req(f)
    smap <- if (identical(f$src, "stems")) tryCatch(hs_map(), error = function(e) NULL) else NULL
    res(NULL)
    chk_task$invoke(list(path = f$path, name = f$name, species_map = smap, reject_zones = TRUE, used_only = isTRUE(input$chk_used_only)))
  })
  observe({ r <- chk_task$result(); res(r) })

  num <- function(x, d = 0L) fmt_num(x, d, lang())
  pct <- function(x, d = 1L) ifelse(is.na(x), "–", fmt_num(100 * x, d, lang()))
  sp_name <- function(x) tr_value(tools::toTitleCase(as.character(x)), lang())
  pname_t <- function(x) tr_value(as.character(x), lang())
  tbl <- function(df, ...) apter_reactable(df, pagination = nrow(df) > 15L, page_size = 15L, lang = lang(), ...)

  output$chk_results <- renderUI({
    r <- res(); if (is.null(r)) return(NULL)
    l <- lang()
    if (!isTRUE(r$ok)) return(div(class = "note-warning", paste(vapply(r$errors, tr_msg, "", lang = l), collapse = " ")))
    msgs <- unique(c(r$warnings))
    sec <- function(id, ...) tagList(h5(class = "section-label", L(id)), ...)
    head <- tagList(
      div(class = "note-muted", L(if (isTRUE(r$used_only)) "chk_mode_used" else "chk_mode_all", num(r$n_products))),
      if (length(msgs)) div(class = "note-warning", paste(vapply(msgs, tr_msg, "", lang = l), collapse = " ")),
      sec("chk_h_file",
          tbl(setNames(data.frame(num(r$n_stems_file), num(if (r$no_profile) 0 else sum(r$stems$use)),
                                  num(if (r$no_profile) r$n_stems_file else r$n_stems_file - sum(r$stems$use)), num(r$n_logs),
                                  stringsAsFactors = FALSE),
                       c(L("chk_k_stems_file"), L("chk_k_stems_used"), L("chk_k_stems_out"), L("chk_k_logs"))))))
    ov <- r$overview
    prods <- ov$products[!ov$products$Type %in% "waste", , drop = FALSE]
    prods$Species <- vapply(prods$Species, sp_name, ""); prods$ProductName <- vapply(prods$ProductName, pname_t, "")
    pgs <- ov$permitted_grades
    gname <- ov$grade_names
    grade_txt <- function(g, sk) {
      if (!length(g) || all(is.na(g))) return("–")
      nm <- if (!is.null(gname) && nrow(gname)) gname$name[match(paste(sk, g), paste(gname$species_key, gname$grade))] else NA
      paste(ifelse(is.na(nm) | nm == as.character(g), as.character(g), sprintf("%s (%s)", g, nm)), collapse = ", ")
    }
    gtab <- if (nrow(pgs)) do.call(rbind, lapply(split(pgs, paste(pgs$ProductKey, pgs$species_key)), function(d)
      data.frame(a = pname_t(d$ProductName[1L]), b = grade_txt(d$grade, d$species_key[1L]),
                 c = if (any(d$not_sawlog %in% TRUE)) paste(d$grade[d$not_sawlog %in% TRUE], collapse = ", ") else "–",
                 stringsAsFactors = FALSE)))
    over <- tagList(sec("chk_h_overview",
      tbl(setNames(transform(prods, Condition = ifelse(is.na(Condition), "\u2013", vapply(BUTT_POSITION_VALUES[match(Condition, BUTT_CONDITIONS)], function(v) tr_value(v, lang()), "")))[, c("ProductName", "Species", "MinTop_cm", "MaxTop_cm", "Condition")],
                   c(L("chk_c_product"), L("col_species"), L("chk_c_mintop"), L("chk_c_maxtop"), L("chk_c_butt")))),
      if (!is.null(gtab)) tbl(setNames(gtab, c(L("chk_c_product"), L("chk_c_grades"), L("chk_c_nosaw"))))))
    if (isTRUE(r$no_profile))
      return(tagList(head, div(class = "note-warning", L("chk_no_profile")), over,
        sec("chk_h_assort", tbl(setNames(cbind(pname_t(r$assortments$recorded$ProductName), num(r$assortments$recorded$logs),
              num(r$assortments$recorded$price_m3, 1L)), c(L("chk_c_product"), L("chk_c_logs"), L("chk_c_volume"))))),
        sec("chk_h_manual", tbl(setNames(data.frame(pct(r$manual$all), pct(r$manual$sawlogs), pct(r$manual$butt_sawlogs)),
              c(L("chk_m_all"), L("chk_m_saw"), L("chk_m_butt"))))),
        sec("chk_h_limits", p(L("chk_lim_e")))))
    kf <- r$key_figures; k0 <- kf[1L, ]
    summ <- p(class = "chk-summary", strong(L("chk_summary", num(k0$stems), num(sum(r$stems$use)), pct(k0$yield),
                                              num(k0$value_rec), num(k0$value_opt))))
    kft <- data.frame(scope = ifelse(seq_len(nrow(kf)) == 1L, L("chk_c_all"), ifelse(kf$scope == "without_broken", L("chk_c_nobroken"), vapply(kf$scope, sp_name, ""))),
                      stems = num(kf$stems), rec = num(kf$value_rec), opt = num(kf$value_opt),
                      rec_m3 = num(kf$value_per_m3_rec), opt_m3 = num(kf$value_per_m3_opt), lost = num(kf$lost), yield = pct(kf$yield),
                      stringsAsFactors = FALSE)
    names(kft) <- c(L("chk_c_species"), L("chk_k_stems_used"), paste(L("chk_c_recorded"), "–", L("chk_c_value")), paste(L("chk_c_best"), "–", L("chk_c_value")),
                    paste(L("chk_c_recorded"), "–", L("chk_c_value_m3")), paste(L("chk_c_best"), "–", L("chk_c_value_m3")), L("chk_c_lost"), L("chk_c_degree"))
    degree <- tagList(sec("chk_h_degree", summ, tbl(kft), plotOutput("chk_fig_degree", height = "320px"),
                          h6(L("chk_stem_title")), div(class = "note-muted", L("chk_stem_hint")),
                          uiOutput("chk_stem_table_ui"), plotOutput("chk_stem_plot", height = "260px")))
    a <- r$assortments
    at <- merge(a$recorded[, c("key", "ProductName", "logs", "price_m3", "value")], a$optimal[, c("key", "logs", "price_m3", "value")],
                by = "key", all = TRUE, suffixes = c("_r", "_o"))
    at$ProductName[is.na(at$ProductName)] <- r$pname[at$key[is.na(at$ProductName)]]
    at <- at[order(-ifelse(is.na(at$value_r), 0, at$value_r)), ]
    att <- data.frame(p = vapply(ifelse(at$key == "waste", "Unclassified", at$ProductName), pname_t, ""),
                      lr = num(at$logs_r), lo = num(at$logs_o), vr = num(at$price_m3_r, 1L), vo = num(at$price_m3_o, 1L),
                      wr = num(at$value_r), wo = num(at$value_o), stringsAsFactors = FALSE)
    names(att) <- c(L("chk_c_product"), paste(L("chk_c_logs"), L("chk_c_recorded")), paste(L("chk_c_logs"), L("chk_c_best")),
                    paste(L("chk_c_volume"), L("chk_c_recorded")), paste(L("chk_c_volume"), L("chk_c_best")),
                    paste(L("chk_c_value"), L("chk_c_recorded")), paste(L("chk_c_value"), L("chk_c_best")))
    asm <- tagList(sec("chk_h_assort", tbl(att), p(L("chk_c_sawshare"), ": ", pct(a$saw_share_rec), " / ", pct(a$saw_share_opt))))
    ld <- r$length_dist
    lens <- tagList(sec("chk_h_length", plotOutput("chk_fig_length", height = "300px"),
      if (!is.null(ld$fg) && nrow(ld$fg)) tbl(setNames(data.frame(pname_t(r$pname[ld$fg$key]), pct(ld$fg$fg_recorded / 100), pct(ld$fg$fg_best / 100),
                         ifelse(is.na(ld$fg$maxdev), "–", num(ld$fg$maxdev, 1L)), stringsAsFactors = FALSE),
         c(L("chk_c_product"), paste(L("chk_c_fit"), L("chk_c_recorded")), paste(L("chk_c_fit"), L("chk_c_best")), L("chk_c_maxdev"))))))
    m <- r$manual
    man <- tagList(sec("chk_h_manual", tbl(setNames(data.frame(pct(m$all), pct(m$sawlogs), pct(m$butt_sawlogs), pct(m$yield_with), pct(m$yield_without),
                       stringsAsFactors = FALSE), c(L("chk_m_all"), L("chk_m_saw"), L("chk_m_butt"), L("chk_m_with"), L("chk_m_without"))))))
    d <- r$defect
    dft <- tagList(sec("chk_h_defect", tbl(setNames(data.frame(pct(d$share_total), pct(d$share_dim), pct(d$share_butt_dim), pct(d$share_waste),
                       stringsAsFactors = FALSE), c(L("chk_d_total"), L("chk_d_dim"), L("chk_d_butt"), L("chk_d_waste"))))))
    tp <- r$top
    tpd <- Reduce(function(x, y) merge(x, y, by = "sp", all = TRUE), list(
      setNames(tp$last_rec[, c("sp", "mean_cm")], c("sp", "a")), setNames(tp$last_opt[, c("sp", "mean_cm")], c("sp", "b")),
      setNames(tp$sawlast_rec[, c("sp", "mean_cm")], c("sp", "c")), setNames(tp$sawlast_opt[, c("sp", "mean_cm")], c("sp", "d"))))
    topt <- tagList(sec("chk_h_top", tbl(setNames(data.frame(vapply(tpd$sp, sp_name, ""), num(tpd$a, 1L), num(tpd$b, 1L), num(tpd$c, 1L), num(tpd$d, 1L),
                       stringsAsFactors = FALSE), c(L("chk_c_species"), paste(L("chk_t_last"), L("chk_c_recorded")), paste(L("chk_t_last"), L("chk_c_best")),
                                                    paste(L("chk_t_sawlast"), L("chk_c_recorded")), paste(L("chk_t_sawlast"), L("chk_c_best")))))))
    ex <- r$excluded
    ext <- if (nrow(ex)) { t <- as.data.frame(table(ex$reason), stringsAsFactors = FALSE); t[[1L]] <- vapply(t[[1L]], tr_msg, "", lang = l)
      setNames(t, c(L("chk_x_reason"), L("chk_x_count"))) }
    fl <- r$flagged
    flt <- c(if (length(fl$broken_top)) sprintf("%s: %s", L("chk_x_broken_kept"), num(length(fl$broken_top))),
             if (length(fl$outside)) sprintf("%s: %s", L("chk_x_outside"), num(length(fl$outside))),
             if (length(fl$rec_gt_opt)) sprintf("%s: %s", L("chk_x_over"), num(length(fl$rec_gt_opt))))
    exc <- tagList(sec("chk_h_excl", if (!is.null(ext)) tbl(ext), lapply(flt, function(x) div(class = "note-muted", x)),
                       div(class = "note-muted", sprintf("%s: %s / %s (%s %%)", L("chk_x_broken"), num(r$broken$n), num(r$broken$n_stems), pct(r$broken$share)))))
    lim <- tagList(sec("chk_h_limits", lapply(c("chk_lim_a", "chk_lim_b", "chk_lim_c", "chk_lim_d", "chk_lim_e", "chk_lim_f"), function(i) p(L(i)))))
    ct <- r$control
    ctl <- tagList(sec("chk_h_control", tbl(setNames(data.frame(num(ct$harvester_m3, 1L), num(ct$apter_m3, 1L), num(ct$diff_pct, 2L), num(ct$share_above_2pct, 1L),
                       num(ct$n_outside), num(ct$n_unpriced), stringsAsFactors = FALSE),
                       c(paste(L("chk_c_volume"), L("chk_c_recorded")), paste(L("chk_c_volume"), L("chk_ctl_calc")), L("chk_ctl_diff"), L("chk_ctl_logs2"),
                         L("chk_ctl_outside"), L("chk_ctl_unpriced")))),
      if (!is.null(ct$unpriced_by_product)) tagList(h6(L("chk_ctl_unp_h")),
        tbl(setNames(data.frame(unname(vapply(r$pname[ct$unpriced_by_product$key], pname_t, "")), num(ct$unpriced_by_product$logs), num(ct$unpriced_by_product$m3, 1L),
                                num(ct$unpriced_by_product$share_pct, 1L), stringsAsFactors = FALSE),
                     c(L("chk_c_product"), L("chk_c_logs"), L("chk_c_volume"), L("chk_ctl_unp_share")))),
        div(class = "note-muted", sprintf("%s: %s", L("chk_ctl_unp_total"), num(ct$unpriced_share_pct, 2L))))))
    dls <- div(class = "pm-actions", downloadButton("chk_dl_stems", L("chk_dl_stems"), class = "btn-secondary btn-sm"),
               downloadButton("chk_dl_logs", L("chk_dl_logs"), class = "btn-secondary btn-sm"))
    tagList(head, over, degree, asm, lens, man, dft, topt, exc, lim, ctl, dls)
  })

  # ---- figures ------------------------------------------------------------------------------------
  output$chk_fig_degree <- renderPlot(res = 96, {
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile))
    y <- sort(100 * r$stems$yield[r$stems$use & !is.na(r$stems$yield)])
    d <- data.frame(x = seq_along(y), y = y)
    ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_line(colour = APTER_CHART$series1, linewidth = 0.7) +
      ggplot2::geom_hline(yintercept = 100, colour = APTER_CHART$baseline) +
      ggplot2::labs(title = L("chk_fig_degree"), x = L("chk_ax_stems"), y = L("chk_ax_degree")) + theme_apter()
  })
  output$chk_fig_length <- renderPlot(res = 96, {
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile)); t <- r$length_dist$tally; req(!is.null(t), nrow(t) > 0L)
    t$name <- vapply(r$pname[t$key], function(x) tr_value(x, lang()), "")
    long <- rbind(data.frame(t[c("name", "length_dm")], n = t$recorded, src = L("chk_c_recorded")),
                  data.frame(t[c("name", "length_dm")], n = t$best, src = L("chk_c_best")))
    ggplot2::ggplot(long, ggplot2::aes(factor(length_dm), n, fill = src)) +
      ggplot2::geom_col(position = "dodge", width = 0.8) + ggplot2::facet_wrap(~name, scales = "free", ncol = 1) +
      ggplot2::scale_fill_manual(values = setNames(c(APTER_CHART$series1, APTER_CHART$series2), c(L("chk_c_recorded"), L("chk_c_best")))) +
      ggplot2::labs(x = L("chk_ax_class"), y = L("chk_ax_logs"), fill = NULL) + theme_apter()
  })

  # ---- the per-stem table: click a stem to see its bucking -------------------------------------------
  stems_tbl <- reactive({
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile))
    s <- r$stems[r$stems$use, , drop = FALSE]; s <- s[order(s$yield), , drop = FALSE]; s
  })
  output$chk_stem_table_ui <- renderUI({
    s <- stems_tbl()
    d <- data.frame(StemKey = s$StemKey, sp = vapply(s$species_group, sp_name, ""), rec = num(s$value_rec), opt = num(s$value_opt),
                    y = pct(s$yield), stringsAsFactors = FALSE)
    names(d) <- c("StemKey", L("chk_c_species"), paste(L("chk_c_recorded"), "–", L("chk_c_value")),
                  paste(L("chk_c_best"), "–", L("chk_c_value")), L("chk_c_degree"))
    d$.key <- s$StemKey
    apter_reactable(d, page_size = 10L, lang = lang(), sortable = FALSE,
                    columns = list(.key = reactable::colDef(show = FALSE)),
                    on_click = reactable::JS("function(rowInfo) { Shiny.setInputValue('chk_click', rowInfo.original['.key'], {priority: 'event'}); }"))
  })
  output$chk_stem_plot <- renderPlot(res = 96, {
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile)); s <- stems_tbl(); req(nrow(s) > 0L)
    k <- or_default(input$chk_click, s$StemKey[1L])
    rec <- r$logs$recorded; rec <- rec[rec$stem_key == k, , drop = FALSE]
    opt <- r$logs$optimal; opt <- opt[as.character(opt$treid) == k, , drop = FALSE]
    rec <- data.frame(ProductType = ifelse(rec$waste, "waste", r$ptype[rec$akey]), StartPos = rec$StartPos, StopPos = rec$StopPos,
                      LengthClass_dm = round(rec$LogLength / 10), stringsAsFactors = FALSE)
    h <- max(c(rec$StopPos, opt$StopPos), na.rm = TRUE) / 100
    apter_stem_compare(rec, opt, h, sprintf("%s %s", L("chk_stem_title"), k), lang = lang(),
                       labels = c(L("chk_stem_recorded"), L("chk_stem_best")))
  })

  # ---- downloads --------------------------------------------------------------------------------------
  output$chk_dl_stems <- downloadHandler(
    filename = function() L("chk_file_stems_csv"),
    content = function(file) {
      r <- res(); req(isTRUE(r$ok))
      d <- if (isTRUE(r$no_profile)) data.frame(StemKey = unique(r$logs$stem_key)) else
        r$stems[, c("StemKey", "species_group", "use", "reason", "n_logs_rec", "value_rec", "value_opt", "yield", "com_rec", "com_opt", "n_manual")]
      write_download_csv(d, file, lang())
    })
  output$chk_dl_logs <- downloadHandler(
    filename = function() L("chk_file_logs_csv"),
    content = function(file) {
      r <- res(); req(isTRUE(r$ok))
      if (isTRUE(r$no_profile)) return(write_download_csv(r$logs[, c("stem_key", "LogKey", "ProductKey", "pname", "StartPos", "LogLength", "CuttingReason", "manual", "m3_price")], file, lang()))
      rec <- r$logs$recorded; opt <- r$logs$optimal
      a <- data.frame(Source = "recorded", StemKey = rec$stem_key, ProductName = rec$pname, StartPos = rec$StartPos, StopPos = rec$StopPos,
                      LogLength = rec$LogLength, price_volume_m3 = rec$price_volume_m3, PriceNOKperM3 = rec$PriceNOKperM3, value_nok = rec$value_nok,
                      manual = rec$manual, harvester_m3_price = rec$m3_price, stringsAsFactors = FALSE)
      b <- data.frame(Source = "best possible", StemKey = as.character(opt$treid), ProductName = r$pname[as.character(opt$ProductKey)], StartPos = opt$StartPos,
                      StopPos = opt$StopPos, LogLength = opt$LogLength, price_volume_m3 = opt$price_volume_m3, PriceNOKperM3 = opt$PriceNOKperM3,
                      value_nok = opt$value_nok, manual = NA, harvester_m3_price = NA_real_, stringsAsFactors = FALSE)
      write_download_csv(rbind(a, b), file, lang())
    })
  outputOptions(output, "chk_dl_stems", suspendWhenHidden = FALSE)
  invisible(NULL)
}
