# app/tuning.R
#
# Purpose : The "Price tuning" tab (roadmap 7, session 3, 2026-10-30; restructure
#           R3, 2026-10-30). Tunes the prices of the sawlog assortments of
#           Scenario A or B that have a target distribution (set in the target
#           view of Assortments and prices, or read from an .hpr/.pin file),
#           several at once, on the .hpr stems, and offers the tuned prices as
#           the other scenario (R/helpers_price_tuning.R).
#
# Method  : the stems are the ones the Simulator reads (hs_stems(), hs_map());
#           the job is prepared on a worker like a run (profiles, scenario's
#           products, prices and grade options); the tuning loop then runs in
#           this process (tune_prices() is synchronous and uses the workers for
#           every bucking), so the session is busy while it runs (PLAN.md,
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
tuning_server <- function(input, output, session, lang, sc, hs_stems, hs_map, stems_ready) {
  L <- function(id, ...) tr(id, ..., lang = lang())
  other <- function(side) if (side == "A") "B" else "A"
  defs_of <- function(side) if (side == "A") sc$defs_a() else sc$defs_b()
  pm_of   <- function(side) pm_minimal(if (side == "A") sc$pm_a() else sc$pm_b())
  is_hpr  <- reactive(identical(input$tree_source, "hpr") && isTRUE(hs_stems()$ok))
  side    <- reactive(or_default(input$tn_side, "A"))

  res  <- reactiveVal(NULL)       # the last tuning, or list(too_few = TRUE)
  busy <- reactiveVal(FALSE)

  # The assortments of the scenario that can be tuned and have a target.
  prods <- reactive({
    req(is_hpr())
    p <- tuning_products(defs_of(side()), pm_of(side()))
    p[p$has_file_target, , drop = FALSE]
  })

  output$tn_main <- renderUI({
    if (!isTRUE(stems_ready())) return(div(class = "note-muted", L("need_stems")))
    if (!is_hpr()) return(div(class = "note-muted", L("tn_need_hpr")))
    div(class = "tally-controls",
      radioButtons("tn_side", L("tn_scenario"), c("A", "B"), selected = isolate(side()), inline = TRUE),
      uiOutput("tn_products_ui"))
  })

  output$tn_products_ui <- renderUI({
    p <- prods(); l <- lang()
    if (nrow(p) == 0L) return(div(class = "note-muted", L("tn_none")))
    lbl <- sprintf("%s – %s", tr_value(tools::toTitleCase(p$species), l), tr_value(p$name, l))
    cur <- intersect(isolate(input$tn_products), p$product_id)
    tagList(
      checkboxGroupInput("tn_products", L("tn_products"), setNames(p$product_id, lbl),
                         selected = if (length(cur)) cur else p$product_id),
      actionButton("tn_run", L("tn_run"), class = "btn-primary"))
  })

  # Results stay until the tuned scenario (its assortments and prices), the
  # selection or the stems change; the other scenario may change freely.
  observe({
    r <- res(); req(r)
    same <- identical(input$tn_side, r$side) &&
      identical(sort(or_default(input$tn_products, character(0))), sort(r$requested)) &&
      identical(defs_of(r$side), r$defs) && identical(pm_of(r$side), r$pm) &&
      identical(hs_stems()$read_id, r$read_id) && identical(hs_map(), r$map) &&
      identical(input$tree_source, "hpr")
    if (!same) res(NULL)
  })

  # ---- the run ---------------------------------------------------------------------------
  observeEvent(input$tn_run, {
    req(is_hpr(), !busy(), length(input$tn_products) > 0L)
    s <- side(); d <- defs_of(s); pm <- pm_of(s)
    ids <- intersect(prods()$product_id, input$tn_products)
    req(length(ids) > 0L)
    gopts <- sc$grade_opts(s)
    pr <- products_from_defs(d, pm)
    products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
    path <- write_price_file(pm_prices(pm))
    max_iter <- as.integer(getOption("apter.tune_max_iter", 30L))
    l <- isolate(lang())
    map <- hs_map(); read_id <- hs_stems()$read_id; requested <- input$tn_products
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = tr_value("Preparing stem profiles", l), value = 0)
    busy(TRUE); res(NULL)
    done <- function() { prog$close(); busy(FALSE); unlink(path) }

    p <- .backend_promise("prepare_hpr_stems_job",
                          list(hs_stems(), map, path, ROOT, pr$pd, pr$lc, list(), NULL, gopts))
    p <- promises::then(p, function(job) {
      out <- tuning_run(job, products, d, ids, max_iter, gopts,
                        on_iter = function(r, m, fg)
                          prog$set(message = tr("tn_iter_progress", r, m, lang = l), value = r / m))
      out$side <- s; out$defs <- d; out$pm <- pm; out$requested <- requested
      out$read_id <- read_id; out$map <- map
      res(out)
    })
    p <- promises::catch(p, function(e)
      showNotification(tr("err_backend", tr_msg(conditionMessage(e), l), lang = l), type = "error",
                       duration = NULL, session = session))
    promises::finally(p, done)
    NULL
  })

  # ---- results -----------------------------------------------------------------------------
  fmt1 <- function(x) fmt_num(x, 1L, lang())
  asm_name <- function(r, k) tr_value(r$by[[k]]$name, lang())
  output$tn_results <- renderUI({
    r <- res(); if (is.null(r)) return(NULL)
    if (isTRUE(r$too_few)) return(div(class = "pm-msg-error", L("tn_too_few")))
    views <- c(target = L("tn_view_target"), orig = L("tn_view_orig"), tuned = L("tn_view_tuned"),
               change = L("tn_price_change"))
    div(class = "tn-block",
      div(class = "tn-top",
        reactable::reactableOutput("tn_tbl"),
        plotOutput("tn_plot", width = "420px", height = "220px")),
      if (length(r$skipped))
        div(class = "pm-msg-warn", L("tn_skipped", paste(vapply(r$skipped, function(k) asm_name(r, k), ""), collapse = ", "))),
      div(class = "tn-hdr", L("tn_matrices")),
      div(class = "tally-controls",
        selectInput("tn_matrix_product", L("lbl_assortment"), setNames(r$ids, vapply(r$ids, function(k) asm_name(r, k), "")),
                    selected = isolate(input$tn_matrix_product), width = "320px"),
        radioButtons("tn_matrix_view", NULL, setNames(names(views), views), inline = TRUE,
                     selected = or_default(isolate(input$tn_matrix_view), "target"))),
      div(class = "tn-axis", L("tn_col_class_mm")),
      reactable::reactableOutput("tn_matrix"),
      div(class = "tally-downloads",
          actionButton("tn_use", L("tn_use", other(r$side)), class = "btn-secondary btn-sm")))
  })
  output$tn_tbl <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    pct <- if (r$orig$value != 0) sprintf(" (%s %%)", fmt_num(100 * (r$tuned$value - r$orig$value) / r$orig$value, 1L, l)) else ""
    nm <- function(k) asm_name(r, k)
    rows <- c(lapply(r$ids, function(k) list(a = sprintf("%s: %s", L("tn_row_fg"), nm(k)),
                       o = fmt1(r$by[[k]]$fg_o), t = fmt1(r$by[[k]]$fg_t), d = fmt1(r$by[[k]]$fg_t - r$by[[k]]$fg_o))),
              list(list(a = L("tn_row_value"), o = fmt_num(r$orig$value, 0L, l), t = fmt_num(r$tuned$value, 0L, l),
                        d = paste0(fmt_num(r$tuned$value - r$orig$value, 0L, l), pct)),
                   list(a = L("tn_row_saw_vol"), o = fmt1(r$orig$saw_vol), t = fmt1(r$tuned$saw_vol),
                        d = fmt1(r$tuned$saw_vol - r$orig$saw_vol))),
              lapply(r$ids, function(k) list(a = sprintf("%s: %s", L("tn_row_prod_vol"), nm(k)),
                       o = fmt1(r$by[[k]]$vol_o), t = fmt1(r$by[[k]]$vol_t), d = fmt1(r$by[[k]]$vol_t - r$by[[k]]$vol_o))))
    tbl <- data.frame(a = vapply(rows, `[[`, "", "a"), b = vapply(rows, `[[`, "", "o"),
                      c = vapply(rows, `[[`, "", "t"), d = vapply(rows, `[[`, "", "d"), stringsAsFactors = FALSE)
    names(tbl) <- c(" ", L("tn_col_orig"), L("tn_col_tuned"), L("tn_col_change"))
    apter_reactable(tbl, sortable = FALSE, pagination = FALSE, lang = l)
  })
  # One thin line per assortment (FG by iteration), the best iteration marked.
  output$tn_plot <- renderPlot(width = 420, height = 220, res = 96, {
    r <- res(); req(r, !isTRUE(r$too_few)); tr_ <- r$trace
    op <- par(mar = c(3.4, 3.8, 0.6, 0.6), mgp = c(2.2, 0.7, 0), cex = 0.8); on.exit(par(op))
    ks <- r$used
    ys <- lapply(ks, function(k) r$by[[k]]$fg_iter)
    plot(NA, xlim = range(tr_$r), ylim = range(unlist(ys), na.rm = TRUE), xlab = L("tn_iteration"), ylab = L("tn_row_fg"),
         xaxt = "n", bty = "l", las = 1)
    at <- pretty(tr_$r); axis(1, at = at[at == round(at) & at >= 1 & at <= max(tr_$r)])
    for (i in seq_along(ks)) {
      lines(tr_$r, ys[[i]], lty = i, lwd = 1)
      points(r$best_iter, ys[[i]][r$best_iter], pch = 19, cex = 0.9)
    }
    if (length(ks) > 1L)
      legend("bottomright", bty = "n", lty = seq_along(ks), lwd = 1, cex = 0.7,
             legend = vapply(ks, function(k) asm_name(r, k), ""))
  })
  # The matrix of the chosen assortment in the chosen view: shares in whole
  # percent per file diameter class (rows) and length (columns); the outcome
  # views have FG per class as an extra column.
  output$tn_matrix <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); l <- lang()
    k <- or_default(input$tn_matrix_product, r$ids[1L]); req(k %in% r$ids)
    v <- or_default(input$tn_matrix_view, "target")
    mv <- tuning_matrix_view(r$by[[k]], v)
    cell <- ifelse(is.na(mv$m), "", fmt_num(mv$m, 0L, l, na = "", big = FALSE))
    df <- as.data.frame(matrix(cell, nrow(mv$m), ncol(mv$m)), stringsAsFactors = FALSE)
    names(df) <- fmt_num(mv$lengths_cm, 0L, l, big = FALSE)
    df <- cbind(setNames(data.frame(fmt_num(mv$classes, 0L, l, big = FALSE), stringsAsFactors = FALSE), " "), df)
    if (!is.null(mv$fg)) df[[L("tn_col_fg")]] <- ifelse(is.na(mv$fg), "", fmt1(mv$fg))
    apter_reactable(df, sortable = FALSE, pagination = FALSE, lang = l)
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
  for (k in used) Pb[[k]] <- tn$by_product[[k]]$best_price
  cur <- products
  for (k in used) cur <- apply_grid_prices(cur, grids[[k]], Pb[[k]])
  t2 <- measure(swap_job_prices(job, cur), Pb)
  by <- lapply(setNames(ids, ids), function(k) {
    g <- grids[[k]]
    list(name = products$pd$ProductName[match(k, products$pd$ProductKey)], grid = g, W = Ws[[k]], P1 = P1[[k]], Pb = Pb[[k]],
         eligible = if (k %in% used) tn$by_product[[k]]$eligible else setNames(rep(FALSE, ncol(P1[[k]])), colnames(P1[[k]])),
         S_orig = o$fg[[k]]$S, S_tuned = t2$fg[[k]]$S, fg_orig = o$fg[[k]]$by_class, fg_tuned = t2$fg[[k]]$by_class,
         fg_o = o$fg[[k]]$overall, fg_t = t2$fg[[k]]$overall, vol_o = sum(o$tal[[k]]$v), vol_t = sum(t2$tal[[k]]$v),
         fg_iter = if (k %in% used) tn$by_product[[k]]$fg else NULL)
  })
  list(ids = ids, used = used, skipped = ids[!ok], by = by,
       orig = list(value = o$value, saw_vol = o$saw_vol), tuned = list(value = t2$value, saw_vol = t2$saw_vol),
       trace = tn$trace, best_iter = tn$best_iter, n = n, n_logs = n_logs)
}
