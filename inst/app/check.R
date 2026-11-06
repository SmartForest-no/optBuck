# app/check.R
#
# Purpose : The "5 Apteringsanalyse" / "5 Bucking analysis" tab (roadmap 8, 2026-11-04): how the harvester bucked the
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
      if (is.null(f)) div(class = "note-muted", L("chk_file_none"))
      else div(class = "chk-file", L("chk_file_line", f$name)),
      div(class = "pm-actions",
          fileInput("chk_upload", NULL, accept = c(".hpr", ".gz", ".zip"), buttonLabel = L("chk_upload"),
                    placeholder = "", width = "320px"),
          if (!is.null(chk_own())) actionLink("chk_reset", L("chk_reset")),
          if (!is.null(f)) actionButton("chk_run", L("chk_run"), class = "btn-primary")))
  })

  # The check as a background job; the file name travels with the task so a late result is
  # never shown for another file.
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
    chk_task$invoke(list(path = f$path, name = f$name, species_map = smap, reject_zones = TRUE))
  })
  observe({ r <- chk_task$result(); res(r) })

  num <- function(x, d = 0L) fmt_num(x, d, lang())
  pct <- function(x, d = 1L) ifelse(is.na(x), "–", fmt_num(100 * x, d, lang()))
  sp_name <- function(x) tr_value(tools::toTitleCase(as.character(x)), lang())
  tbl <- function(df, ...) optbuck_reactable(df, pagination = nrow(df) > 15L, page_size = 15L, lang = lang(), ...)

  output$chk_results <- renderUI({
    r <- res(); if (is.null(r)) return(NULL)
    l <- lang()
    if (!isTRUE(r$ok)) return(div(class = "note-warning", paste(vapply(r$errors, tr_msg, "", lang = l), collapse = " ")))
    msgs <- unique(c(r$warnings))
    tb <- chk_tables(r, l)
    sec <- function(id, ...) tagList(h5(class = "section-label", L(id)), ...)
    head <- tagList(
      if (length(msgs)) div(class = "note-warning", paste(vapply(msgs, tr_msg, "", lang = l), collapse = " ")),
      sec("chk_h_file", tbl(tb$file)),
      if (!is.null(tb$grades)) sec("chk_h_overview", tbl(tb$grades)))
    lim <- sec("chk_h_limits", lapply(tb$limits, function(x) p(x)))
    if (isTRUE(r$no_profile))
      return(tagList(head, div(class = "note-warning", L("chk_no_profile")),
        sec("chk_h_assort", tbl(tb$assort)), sec("chk_h_manual", tbl(tb$manual)), lim))
    degree <- sec("chk_h_degree", tbl(tb$keyfig), div(class = "note-muted", tb$left_out), plotOutput("chk_fig_degree", height = "300px", width = "900px"),
                  h6(L("chk_stem_title")), div(class = "note-muted", L("chk_stem_hint")),
                  uiOutput("chk_stem_table_ui"), plotOutput("chk_stem_plot", height = "260px"))
    asm <- sec("chk_h_assort", tbl(tb$assort), p(tb$sawshare))
    nf <- chk_length_facets(r)
    lens <- sec("chk_h_length", if (nf > 0L) plotOutput("chk_fig_length", height = sprintf("%dpx", 220L * nf + 60L), width = "900px"), if (!is.null(tb$fg)) tbl(tb$fg))
    dls <- div(class = "pm-actions", downloadButton("chk_dl_pdf", L("chk_dl_pdf"), class = "btn-secondary btn-sm"))
    tagList(head, degree, asm, lens, sec("chk_h_manual", tbl(tb$manual)), sec("chk_h_defect", tbl(tb$defect)), sec("chk_h_top", tbl(tb$top)), lim, dls)
  })

  # ---- figures ------------------------------------------------------------------------------------
  output$chk_fig_degree <- renderPlot(res = 96, {
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile)); p <- chk_fig_density(r, lang()); req(p); p
  })
  output$chk_fig_length <- renderPlot(res = 96, {
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile)); p <- chk_fig_length(r, lang()); req(p); p
  })

  # ---- the per-stem table: click a stem to see its bucking -------------------------------------------
  stems_tbl <- reactive({
    r <- res(); req(isTRUE(r$ok), !isTRUE(r$no_profile))
    s <- r$stems[r$stems$use, , drop = FALSE]; s <- s[order(s$yield), , drop = FALSE]; s
  })
  output$chk_stem_table_ui <- renderUI({
    s <- stems_tbl()
    d <- data.frame(StemKey = s$StemKey, sp = unname(vapply(s$species_group, sp_name, "")), rec = num(s$value_rec), opt = num(s$value_opt),
                    y = pct(s$yield), stringsAsFactors = FALSE)
    names(d) <- c("StemKey", L("chk_c_species"), paste(L("chk_c_recorded"), "–", L("chk_c_value")),
                  paste(L("chk_c_best"), "–", L("chk_c_value")), L("chk_c_degree"))
    d$.key <- s$StemKey
    optbuck_reactable(d, page_size = 10L, lang = lang(), sortable = FALSE,
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
    optbuck_stem_compare(rec, opt, h, sprintf("%s %s", L("chk_stem_title"), k), lang = lang(),
                       labels = c(L("chk_stem_recorded"), L("chk_stem_best")))
  })

  # ---- download: the report as a PDF in the current language ---------------------------------------
  output$chk_dl_pdf <- downloadHandler(
    filename = function() L("chk_file_pdf"),
    content = function(file) { r <- res(); req(isTRUE(r$ok)); chk_report_pdf(r, lang(), file, or_default(r$file_name, r$file)) })
  outputOptions(output, "chk_dl_pdf", suspendWhenHidden = FALSE)
  invisible(NULL)
}
