# app/tuning.R
#
# Purpose : The "Price tuning" tab (roadmap 7, session 3, 2026-10-30; restructure
#           R3, 2026-10-30; R4 D5/D7, 2026-11-02). Tunes the prices of the sawlog
#           assortments of Scenario A or B that have a target distribution (set in
#           the target view of Assortments and prices, or read from an .hpr/.pin
#           file), several at once, on the stems of the chosen source (the built-in
#           selection, a tree list or .hpr stems), and offers the tuned prices as the
#           other scenario (R/helpers_price_tuning.R). All sawlog assortments are
#           listed; those that cannot be tuned are greyed with the reason.
#
# Method  : the stems are the ones the Simulator reads (stem_spec()); the job is
#           prepared like a run's (prepare_job_async(): NFI profiles from the cache,
#           taperNOR for a tree list, .hpr profiles on a worker); the tuning loop then
#           runs in this process (tune_prices() is synchronous and uses the workers
#           for every bucking), so the session is busy while it runs (PLAN.md,
#           deployment). Seed 1, max_iter = getOption("apter.tune_max_iter", 30),
#           early_stop = FALSE, targets always the assortments' own (R3.2).
#           The results are cleared when the tuned scenario, the assortment
#           selection or the stems change, not when the other scenario does.

TUNE_MIN_LOGS <- 20L

tuning_page <- nav_panel(
  t_("tab_tuning"), value = "tuning",
  card(card_body(p(class = "page-intro", t_("intro_tuning")),
                 uiOutput("tn_main"),
                 uiOutput("tn_results"))))

#' @param sc The scenario_server() result (defs_a, pm_a, defs_b, pm_b, grade_opts,
#'   replace_scenario).
tuning_server <- function(input, output, session, lang, sc, stem_spec, stem_id, stems_ready) {
  L <- function(id, ...) tr(id, ..., lang = lang())
  other <- function(side) if (side == "A") "B" else "A"
  defs_of <- function(side) if (side == "A") sc$defs_a() else sc$defs_b()
  pm_of   <- function(side) pm_minimal(if (side == "A") sc$pm_a() else sc$pm_b())
  side    <- reactive(or_default(input$tn_side, "A"))

  res  <- reactiveVal(NULL)       # the last tuning, or list(too_few = TRUE)
  busy <- reactiveVal(FALSE)

  # Every sawlog assortment of the scenario with the reason it cannot be tuned (R4 D5);
  # the tunable ones (with a target) are the checkboxes.
  status <- reactive({
    req(stems_ready())
    tuning_sawlog_status(defs_of(side()), pm_of(side()))
  })
  prods <- reactive({ st <- status(); st[st$reason == "ok", , drop = FALSE] })
  # "Gran – REDACTED25": the species and the assortment's name in the viewer's language.
  disp_name <- function(species, name, l = lang())
    sprintf("%s – %s", tr_value(tools::toTitleCase(as.character(species)), l), tr_value(as.character(name), l))
  disp_key <- function(r, k, l = lang()) {
    i <- match(k, assortment_table_keys(r$defs))
    disp_name(r$defs$Species[i], r$defs$Name[i], l)
  }

  output$tn_main <- renderUI({
    if (!isTRUE(stems_ready())) return(div(class = "note-muted", L("need_stems")))
    div(class = "tally-controls",
      radioButtons("tn_side", L("tn_scenario"), c("A", "B"), selected = isolate(side()), inline = TRUE),
      uiOutput("tn_products_ui"))
  })

  output$tn_products_ui <- renderUI({
    st <- status(); l <- lang()
    if (nrow(st) == 0L) return(div(class = "note-muted", L("tn_none")))
    ok <- st$reason == "ok"
    lbl <- disp_name(st$species, st$name, l)
    why <- c(no_target = L("tn_reason_no_target"), one_length = L("tn_reason_one_length"),
             one_class = L("tn_reason_one_class"), flat = L("tn_reason_flat"))
    cur <- intersect(isolate(input$tn_products), st$product_id[ok])
    tagList(
      if (any(ok))
        checkboxGroupInput("tn_products", L("tn_products"), setNames(st$product_id[ok], lbl[ok]),
                           selected = if (length(cur)) cur else st$product_id[ok])
      else div(class = "note-muted", L("tn_none")),
      if (any(!ok))
        div(class = "tn-unavailable", lapply(which(!ok), function(i)
          div(class = "tn-off", tags$input(type = "checkbox", disabled = "disabled"), tags$span(lbl[i]),
              tags$span(class = "tn-reason", why[[st$reason[i]]])))),
      if (any(ok)) actionButton("tn_run", L("tn_run"), class = "btn-primary"))
  })

  # Results stay until the tuned scenario (its assortments and prices), the
  # selection or the stems change; the other scenario may change freely.
  observe({
    r <- res(); req(r)
    same <- identical(input$tn_side, r$side) &&
      identical(sort(or_default(input$tn_products, character(0))), sort(r$requested)) &&
      identical(defs_of(r$side), r$defs) && identical(pm_of(r$side), r$pm) &&
      identical(stem_id(), r$stem_id)
    if (!same) res(NULL)
  })

  # ---- the run ---------------------------------------------------------------------------
  observeEvent(input$tn_run, {
    req(stems_ready(), !busy(), length(input$tn_products) > 0L)
    s <- side(); d <- defs_of(s); pm <- pm_of(s)
    ids <- intersect(prods()$product_id, input$tn_products)
    req(length(ids) > 0L)
    gopts <- sc$grade_opts(s)
    gwarn <- sc$grade_warnings(s)
    pr <- products_from_defs(d, pm)
    products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
    path <- write_price_file(pm_prices(pm))
    max_iter <- as.integer(getOption("apter.tune_max_iter", 30L))
    l <- isolate(lang())
    spec <- stem_spec(); sid <- stem_id(); requested <- input$tn_products
    spec$path <- path; spec$pd <- pr$pd; spec$lc <- pr$lc; spec$scenario <- NULL
    if (identical(spec$src, "hpr")) { spec$list_params <- list(); spec$grade_opts <- gopts }
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = tr_value("Preparing stem profiles", l), value = 0)
    busy(TRUE); res(NULL)
    done <- function() { prog$close(); busy(FALSE); unlink(path) }

    p <- prepare_job_async(spec, progress = function(done, total, what)
      prog$set(message = tr_value(what, l), value = if (is.na(total) || total == 0L) 0 else done / total))
    p <- promises::then(p, function(job) {
      out <- tuning_run(job, products, d, ids, max_iter, gopts,
                        on_iter = function(r, m, fg)
                          prog$set(message = tr("tn_iter_progress", r, m, lang = l), value = r / m))
      out$side <- s; out$defs <- d; out$pm <- pm; out$requested <- requested
      out$stem_id <- sid; out$grade_warn <- gwarn
      res(out)
    })
    p <- promises::catch(p, function(e)
      showNotification(tr("err_backend", tr_msg(conditionMessage(e), l), lang = l), type = "error",
                       duration = NULL, session = session))
    promises::finally(p, done)
    NULL
  })

  # ---- results ----------------------------------------------------------------------------
  fmt1 <- function(x) fmt_num(x, 1L, lang())
  # One line for each tuned assortment whose scenario has apportionment bucking switched on
  # (DistributionAllowed with MAXDeviation > 0): the tuned prices assume pure value bucking.
  apportion_notes <- function(r) {
    d <- r$defs; k <- assortment_table_keys(d)
    lines <- lapply(r$used, function(id) {
      i <- match(id, k)
      if (is.na(i) || !isTRUE(d$DistributionAllowed[i]) || !isTRUE(d$MAXDeviation[i] > 0)) return(NULL)
      x <- d$MAXDeviation[i]
      p(class = "note-muted", L("tn_apportion_note", disp_key(r, id), fmt_num(x, if (x == round(x)) 0L else 1L, lang())))
    })
    tagList(Filter(Negate(is.null), lines))
  }
  output$tn_results <- renderUI({
    r <- res(); if (is.null(r)) return(NULL)
    if (isTRUE(r$too_few)) return(div(class = "pm-msg-error", L("tn_too_few")))
    views <- c(target = L("tn_view_target"), orig = L("tn_view_orig"), tuned = L("tn_view_tuned"),
               change = L("tn_price_change"))
    div(class = "tn-block",
      p(class = "tn-scope", L("tn_scope", fmt_num(r$n, 0L, lang()), fmt_num(r$N, 0L, lang()), fmt_num(r$N, 0L, lang()))),
      div(class = "tn-hdr", L("tn_hdr_hits")),
      reactable::reactableOutput("tn_tbl_hits"),
      p(class = "note-muted", L("tn_fg_def")),
      div(class = "tn-hdr", L("tn_hdr_cost")),
      reactable::reactableOutput("tn_tbl_cost"),
      p(class = "note-muted", L("tn_cost_caption")),
      plotOutput("tn_plot", width = "700px", height = "320px"),
      p(class = "note-muted", L("tn_fig_caption")),
      apportion_notes(r),
      div(class = "tn-hdr", L("tn_matrices")),
      div(class = "tally-controls",
        selectInput("tn_matrix_product", L("col_assortment"), setNames(r$ids, vapply(r$ids, function(k) disp_key(r, k), "")),
                    selected = isolate(input$tn_matrix_product), width = "320px"),
        radioButtons("tn_matrix_view", NULL, setNames(names(views), views), inline = TRUE,
                     selected = or_default(isolate(input$tn_matrix_view), "target"))),
      reactable::reactableOutput("tn_matrix"),
      uiOutput("tn_matrix_note"),
      div(class = "tally-downloads",
          actionButton("tn_use", L("tn_use", other(r$side)), class = "btn-secondary btn-sm")))
  })
  # Hits per assortment: logs, degree of fit and volume before / after, status.
  output$tn_tbl_hits <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    by <- r$by
    df <- data.frame(
      a = vapply(r$ids, function(k) disp_key(r, k), ""),
      n = fmt_num(r$n_logs[r$ids], 0L, l),
      fg1 = fmt1(vapply(r$ids, function(k) by[[k]]$fg_o, 0)), v1 = fmt1(vapply(r$ids, function(k) by[[k]]$vol_o, 0)),
      fg2 = fmt1(vapply(r$ids, function(k) by[[k]]$fg_t, 0)), v2 = fmt1(vapply(r$ids, function(k) by[[k]]$vol_t, 0)),
      st = vapply(r$ids, function(k) if (k %in% r$used) L("tn_status_ok") else {
        s <- L("tn_status_few", fmt_num(r$n_logs[[k]], 0L, l))
        # the grade mismatch warning of the assortment, after the reason
        w <- r$grade_warn[r$grade_warn$key == k, , drop = FALSE]
        if (NROW(w)) paste(s, grade_warning_text(w, function(x) disp_key(r, x, l), l)) else s
      }, ""),
      stringsAsFactors = FALSE)
    rownames(df) <- NULL   # the named vapply() results would show as a row-name column
    cols <- list(a = reactable::colDef(name = L("col_assortment"), width = 260),
                 n = reactable::colDef(name = L("col_logs"), width = 90, align = "right"),
                 fg1 = reactable::colDef(name = L("tn_col_fg"), width = 120, align = "right"),
                 v1 = reactable::colDef(name = L("tn_col_vol"), width = 100, align = "right"),
                 fg2 = reactable::colDef(name = L("tn_col_fg"), width = 120, align = "right"),
                 v2 = reactable::colDef(name = L("tn_col_vol"), width = 100, align = "right"),
                 st = reactable::colDef(name = " ", minWidth = 420))
    apter_reactable(df, columns = cols, sortable = FALSE, pagination = FALSE, lang = l,
                    column_groups = list(reactable::colGroup(L("tn_grp_before"), c("fg1", "v1")),
                                         reactable::colGroup(L("tn_grp_after"), c("fg2", "v2"))))
  })
  # Cost for the whole harvest: value at the original prices and sawlog volume.
  output$tn_tbl_cost <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    chg <- function(a, b, digits) {
      p <- if (a != 0) sprintf(" (%s %%)", fmt_num(100 * (b - a) / a, 1L, l, big = FALSE)) else ""
      paste0(fmt_num(b - a, digits, l), p)
    }
    df <- data.frame(
      a = c(L("tn_row_value"), L("tn_row_saw_vol")),
      b = c(fmt_num(r$orig$value, 0L, l), fmt1(r$orig$saw_vol)),
      c = c(fmt_num(r$tuned$value, 0L, l), fmt1(r$tuned$saw_vol)),
      d = c(chg(r$orig$value, r$tuned$value, 0L), chg(r$orig$saw_vol, r$tuned$saw_vol, 1L)),
      stringsAsFactors = FALSE)
    names(df) <- c(" ", L("tn_col_before"), L("tn_col_after"), L("tn_col_change"))
    apter_reactable(df, sortable = FALSE, pagination = FALSE, lang = l)
  })
  # One line per tuned assortment (degree of fit by iteration), the best iteration marked.
  # Colours are the app's chart colours (the first is the primary green); with more
  # assortments than colours the line type changes.
  output$tn_plot <- renderPlot(width = 700, height = 320, res = 96, {
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    ks <- r$used; lab <- vapply(ks, function(k) disp_key(r, k, l), "")
    d <- do.call(rbind, lapply(seq_along(ks), function(i)
      data.frame(it = r$trace$r, fg = r$by[[ks[i]]]$fg_iter, a = lab[i], stringsAsFactors = FALSE)))
    d$a <- factor(d$a, levels = lab)
    pal <- c(APTER_CHART$series1, APTER_CHART$series2, APTER_CHART$other, APTER_CHART$neutral)
    lt <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    col <- setNames(rep_len(pal, length(ks)), lab)
    lty <- setNames(lt[((seq_along(ks) - 1L) %/% length(pal)) %% length(lt) + 1L], lab)
    best <- d[d$it == r$best_iter, , drop = FALSE]
    ggplot2::ggplot(d, ggplot2::aes(x = it, y = fg, colour = a, linetype = a)) +
      ggplot2::geom_line(linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(data = best, size = 2.4, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = col) + ggplot2::scale_linetype_manual(values = lty) +
      ggplot2::scale_x_continuous(breaks = function(x) { b <- pretty(x); b[b == round(b)] }) +
      ggplot2::labs(x = L("tn_iteration"), y = L("tn_col_fg"), title = L("tn_fig_title"), colour = NULL, linetype = NULL) +
      theme_apter() + ggplot2::theme(legend.position = "bottom")
  })
  # The matrix of the chosen assortment in the chosen view: shares in whole percent per
  # file diameter class (rows) and length class (columns); the outcome views have the
  # degree of fit per class as an extra column. "–": classes without logs, unchanged prices.
  output$tn_matrix <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    k <- or_default(input$tn_matrix_product, r$ids[1L]); req(k %in% r$ids)
    v <- or_default(input$tn_matrix_view, "target")
    mv <- tuning_matrix_view(r$by[[k]], v)
    cell <- ifelse(mv$dash, "–", ifelse(is.na(mv$m), "", fmt_num(mv$m, 0L, l, na = "", big = FALSE)))
    nl <- ncol(mv$m); lc <- paste0("L", seq_len(nl))
    df <- data.frame(cls = fmt_num(mv$classes, 0L, l, big = FALSE), stringsAsFactors = FALSE)
    for (j in seq_len(nl)) df[[lc[j]]] <- cell[, j]
    cols <- list(cls = reactable::colDef(name = L("tn_col_class_mm"), width = 150))
    for (j in seq_len(nl)) cols[[lc[j]]] <- reactable::colDef(name = fmt_num(mv$lengths_cm[j], 0L, l, big = FALSE), width = 64, align = "right")
    if (!is.null(mv$fg)) {
      df$fg <- ifelse(is.na(mv$fg), "", fmt1(mv$fg))
      cols$fg <- reactable::colDef(name = L("tn_col_fg"), width = 150, align = "right")
    }
    apter_reactable(df, columns = cols, sortable = FALSE, pagination = FALSE, lang = l,
                    column_groups = list(reactable::colGroup(L("tn_col_length_cm"), lc)))
  })
  output$tn_matrix_note <- renderUI({
    if (identical(or_default(input$tn_matrix_view, "target"), "change")) p(class = "note-muted", L("tn_change_caption"))
  })

  # Test mode only (shinytest2): the tuned grids, so audit 17 can compare them with scenario B.
  exportTestValues(tn_test = {
    r <- res()
    if (is.null(r) || isTRUE(r$too_few)) NULL
    else list(used = r$used, by = lapply(setNames(r$used, r$used), function(k) {
      g <- r$by[[k]]$grid
      list(lengths = g$lengths, diam_classes = g$diam_classes, cm_map = as.list(g$cm_map),
           best_price = unname(r$by[[k]]$Pb))
    }))
  })

  # ---- use the tuned prices in the other scenario --------------------------------------------
  observeEvent(input$tn_use, {
    r <- res(); req(r, !isTRUE(r$too_few))
    showModal(modalDialog(
      title = L("tn_replace_q", other(r$side)), easyClose = TRUE, fade = FALSE,
      footer = tagList(modalButton(L("btn_cancel")),
                       actionButton("tn_use_ok", L("tn_ok"), class = "btn-primary"))))
  })
  observeEvent(input$tn_use_ok, {
    r <- res(); req(r, !isTRUE(r$too_few)); removeModal()
    new <- r$pm
    for (k in r$used) new <- apply_grid_prices(list(pm = new), r$by[[k]]$grid, r$by[[k]]$Pb)$pm
    sc$replace_scenario(other(r$side), r$side, r$defs, new)
  })
}

#' The tuning on a prepared job (runs in this process): the original-price run of
#' all stems, the tuning of the assortments with enough logs together on one
#' sample, the run with the best prices. The targets are the assortments' own.
#' @param ids ProductKeys of the assortments to tune.
#' @return list(too_few = TRUE) or list(ids, used (tuned), skipped (too few
#'   logs), by (per assortment: name, grid, W, P1, Pb (best prices), eligible,
#'   S_orig / S_tuned (shares), fg_orig / fg_tuned (per class), fg_o / fg_t
#'   (overall), vol_o / vol_t (price volume), fg_iter), orig / tuned (value at
#'   original prices, sawlog volume), trace, best_iter, n, n_logs).
tuning_run <- function(job, products, defs, ids, max_iter, grade_opts, on_iter = NULL) {
  ids <- as.character(ids)
  hp <- setNames(lapply(ids, function(k) tuning_hpr_info(defs, products$pm, k)), ids)
  grids <- setNames(lapply(ids, function(k) tuning_grid(products, k, hp[[k]])), ids)
  Ws <- lapply(grids, tuning_target, type = "real")
  basis <- vapply(grids, function(g) if (g$basis_default == "volume") "volume" else "number", "")
  P1 <- lapply(grids, function(g) g$price)
  measure <- function(job_x, Pl) {
    res <- finish_job(job_x, buck_job(job_x))
    sl <- res$selected_logs_all
    tal <- lapply(setNames(ids, ids), function(k) tally_grid(res, k, grids[[k]]))
    sums <- function(Pm) sum(vapply(ids, function(k) { a <- !is.na(P1[[k]]); sum(tal[[k]]$v[a] * Pm[[k]][a]) }, 0))
    list(tal = tal, fg = lapply(setNames(ids, ids), function(k) fg_grid(tal[[k]], Ws[[k]], basis[[k]])),
         value = sum(res$tree_summary$total_value_nok) - sums(Pl) + sums(P1),
         saw_vol = sum(sl$price_volume_m3[as.character(sl$ProductType) == "sawlog"]))
  }
  o <- measure(job, P1)
  n_logs <- vapply(ids, function(k) sum(o$tal[[k]]$n), 0)
  ok <- vapply(ids, function(k) n_logs[[k]] >= TUNE_MIN_LOGS &&
                 any(colSums(o$tal[[k]]$n) >= TUNE_MIN_LOGS & colSums(!is.na(Ws[[k]])) > 0), logical(1))
  if (!any(ok)) return(list(too_few = TRUE))
  used <- ids[ok]
  n <- max(vapply(used, function(k) tuning_sample_size(length(job$pvs), n_logs[[k]]), 0L))
  tn <- tune_prices(job, products, used, "real", n = n, seed = 1, max_iter = max_iter,
                    hpr = if (length(used) > 1L) hp[used] else hp[[used]], early_stop = FALSE, on_iter = on_iter)
  Pb <- P1
  # The best prices in whole NOK (R4): they are what the final all-stems evaluation uses, what "Use
  # tuned prices" puts into the other scenario and what the .pin export writes (its prices are integers).
  for (k in used) Pb[[k]] <- round(tn$by_product[[k]]$best_price)
  cur <- products
  for (k in used) cur <- apply_grid_prices(cur, grids[[k]], Pb[[k]])
  t2 <- measure(swap_job_prices(job, cur), Pb)
  by <- lapply(setNames(ids, ids), function(k) {
    g <- grids[[k]]
    list(name = products$pd$ProductName[match(k, products$pd$ProductKey)], grid = g, W = Ws[[k]], P1 = P1[[k]], Pb = Pb[[k]],
         eligible = if (k %in% used) tn$by_product[[k]]$eligible else setNames(rep(FALSE, ncol(P1[[k]])), colnames(P1[[k]])),
         n_orig = colSums(o$tal[[k]]$n), n_tuned = colSums(t2$tal[[k]]$n),
         S_orig = o$fg[[k]]$S, S_tuned = t2$fg[[k]]$S, fg_orig = o$fg[[k]]$by_class, fg_tuned = t2$fg[[k]]$by_class,
         fg_o = o$fg[[k]]$overall, fg_t = t2$fg[[k]]$overall, vol_o = sum(o$tal[[k]]$v), vol_t = sum(t2$tal[[k]]$v),
         fg_iter = if (k %in% used) tn$by_product[[k]]$fg else NULL)
  })
  list(ids = ids, used = used, skipped = ids[!ok], by = by,
       orig = list(value = o$value, saw_vol = o$saw_vol), tuned = list(value = t2$value, saw_vol = t2$saw_vol),
       trace = tn$trace, best_iter = tn$best_iter, n = n, N = length(job$pvs), n_logs = n_logs)
}
