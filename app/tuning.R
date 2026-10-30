# app/tuning.R
#
# Purpose : The "Price tuning" tab (roadmap 7, session 3, 2026-10-30). Tunes the
#           prices of one sawlog product of Scenario A or B on the .hpr stems
#           towards a target length distribution (R/helpers_price_tuning.R) and
#           offers the tuned prices as the other scenario.
#
# Method  : the stems are the ones the Simulator reads (hs_stems(), hs_map());
#           the job is prepared on a worker like a run (profiles, scenario's
#           products, prices and grade options); the tuning loop then runs in
#           this process (tune_prices() is synchronous and uses the workers for
#           every bucking), so the session is busy while it runs (PLAN.md,
#           deployment). Seed 1, max_iter = getOption("apter.tune_max_iter", 30),
#           early_stop = FALSE, basis automatic.

TUNE_MIN_LOGS <- 20L

tuning_page <- nav_panel(
  t_("tab_tuning"), value = "tuning",
  card(card_body(uiOutput("tn_main"),
                 uiOutput("tn_results"))))

#' @param sc The scenario_server() result (defs_a, pm_a, defs_b, pm_b, grade_opts,
#'   replace_scenario).
tuning_server <- function(input, output, session, lang, sc, hs_stems, hs_map) {
  L <- function(id, ...) tr(id, ..., lang = lang())
  other <- function(side) if (side == "A") "B" else "A"
  defs_of <- function(side) if (side == "A") sc$defs_a() else sc$defs_b()
  pm_of   <- function(side) pm_minimal(if (side == "A") sc$pm_a() else sc$pm_b())
  is_hpr  <- reactive(identical(input$tree_source, "hpr") && isTRUE(hs_stems()$ok))
  side    <- reactive(or_default(input$tn_side, "A"))

  res  <- reactiveVal(NULL)       # the last tuning, or list(too_few = TRUE)
  busy <- reactiveVal(FALSE)
  observeEvent(list(input$tn_side, input$tn_product, input$tn_target, input$tree_source), res(NULL),
               ignoreInit = TRUE)

  prods <- reactive({
    req(is_hpr())
    tuning_products(defs_of(side()), pm_of(side()))
  })

  output$tn_main <- renderUI({
    if (!is_hpr()) return(div(class = "note-muted", L("tn_need_hpr")))
    div(class = "tally-controls",
      radioButtons("tn_side", L("tn_scenario"), c("A", "B"), selected = isolate(side()), inline = TRUE),
      uiOutput("tn_product_ui"),
      uiOutput("tn_target_ui"),
      actionButton("tn_run", L("tn_run"), class = "btn-primary"))
  })

  output$tn_product_ui <- renderUI({
    p <- prods(); l <- lang()
    lbl <- sprintf("%s – %s", tr_value(tools::toTitleCase(p$species), l), tr_value(p$name, l))
    cur <- isolate(input$tn_product)
    selectInput("tn_product", L("tn_product"), setNames(p$product_id, lbl),
                selected = if (isTRUE(cur %in% p$product_id)) cur, width = "320px")
  })

  has_file <- reactive({
    p <- prods(); k <- input$tn_product
    isTRUE(p$has_file_target[match(k, p$product_id)])
  })
  output$tn_target_ui <- renderUI({
    f <- has_file(); cur <- isolate(input$tn_target)
    ch <- c(if (f) c(setNames("real", L("tn_target_file"))), setNames("neutral", L("tn_target_even")),
            setNames("long", L("tn_target_long")))
    radioButtons("tn_target", L("tn_target"), ch, inline = TRUE,
                 selected = if (isTRUE(cur %in% ch)) cur else ch[[1L]])
  })

  # ---- the run ---------------------------------------------------------------------------
  observeEvent(input$tn_run, {
    req(is_hpr(), !busy(), input$tn_product, input$tn_target)
    s <- side(); key <- input$tn_product; type <- input$tn_target
    if (type == "real" && !has_file()) type <- "neutral"
    d <- defs_of(s); pm <- pm_of(s)
    gopts <- sc$grade_opts(s)
    pr <- products_from_defs(d, pm)
    products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
    path <- write_price_file(pm_prices(pm))
    max_iter <- as.integer(getOption("apter.tune_max_iter", 30L))
    l <- isolate(lang())
    prog <- shiny::Progress$new(session, min = 0, max = 1)
    prog$set(message = tr_value("Preparing stem profiles", l), value = 0)
    busy(TRUE); res(NULL)
    done <- function() { prog$close(); busy(FALSE); unlink(path) }

    p <- .backend_promise("prepare_hpr_stems_job",
                          list(hs_stems(), hs_map(), path, ROOT, pr$pd, pr$lc, list(), NULL, gopts))
    p <- promises::then(p, function(job) {
      out <- tuning_run(job, products, d, key, type, max_iter, gopts,
                        on_iter = function(r, m, fg)
                          prog$set(message = tr("tn_iter_progress", r, m, lang = l), value = r / m))
      out$side <- s; out$key <- key; out$defs <- d; out$pm <- pm; out$type <- type
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
  output$tn_results <- renderUI({
    r <- res(); if (is.null(r)) return(NULL)
    if (isTRUE(r$too_few)) return(div(class = "pm-msg-error", L("tn_too_few")))
    div(class = "tn-block",
      reactable::reactableOutput("tn_tbl"),
      plotOutput("tn_plot", height = "260px"),
      div(class = "tn-hdr", L("tn_price_change")),
      div(class = "tn-axis", L("tn_col_class_mm")),
      reactable::reactableOutput("tn_price_tbl"),
      div(class = "tally-downloads",
          actionButton("tn_use", L("tn_use", other(r$side)), class = "btn-secondary btn-sm")))
  })
  output$tn_tbl <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); o <- r$orig; t <- r$tuned
    pct <- if (o$value != 0) sprintf(" (%s %%)", fmt_num(100 * (t$value - o$value) / o$value, 1L, lang())) else ""
    tbl <- data.frame(
      a = c(L("tn_row_fg"), L("tn_row_value"), L("tn_row_prod_vol"), L("tn_row_saw_vol")),
      b = c(fmt1(o$fg), fmt_num(o$value, 0L, lang()), fmt1(o$prod_vol), fmt1(o$saw_vol)),
      c = c(fmt1(t$fg), fmt_num(t$value, 0L, lang()), fmt1(t$prod_vol), fmt1(t$saw_vol)),
      d = c(fmt1(t$fg - o$fg), paste0(fmt_num(t$value - o$value, 0L, lang()), pct),
            fmt1(t$prod_vol - o$prod_vol), fmt1(t$saw_vol - o$saw_vol)), stringsAsFactors = FALSE)
    names(tbl) <- c(" ", L("tn_col_orig"), L("tn_col_tuned"), L("tn_col_change"))
    apter_reactable(tbl, sortable = FALSE, pagination = FALSE, lang = lang())
  })
  output$tn_price_tbl <- reactable::renderReactable({
    r <- res(); req(r, !isTRUE(r$too_few)); pc <- r$change
    ptab <- as.data.frame(ifelse(is.na(pc), "", fmt_num(round(pc), 0L, lang(), na = "")), stringsAsFactors = FALSE)
    names(ptab) <- colnames(pc)
    ptab <- cbind(setNames(data.frame(fmt_num(r$lengths_cm, 0L, lang(), big = FALSE), stringsAsFactors = FALSE),
                           L("col_length_cm")), ptab)
    names(ptab)[-1L] <- colnames(pc)
    apter_reactable(ptab, sortable = FALSE, pagination = FALSE, lang = lang())
  })
  output$tn_plot <- renderPlot({
    r <- res(); req(r, !isTRUE(r$too_few)); tr_ <- r$trace
    op <- par(mar = c(4, 4.2, 1, 1)); on.exit(par(op))
    plot(tr_$r, tr_$fg, type = "b", pch = 1, xlab = L("tn_iteration"), ylab = L("tn_row_fg"),
         xaxt = "n", bty = "l", las = 1)
    at <- pretty(tr_$r); axis(1, at = at[at == round(at) & at >= 1 & at <= max(tr_$r)])
    points(r$best_iter, tr_$fg[r$best_iter], pch = 19, cex = 1.4)
  })

  # Test mode only (shinytest2): the tuned grid, so audit 17 can compare it with scenario B.
  exportTestValues(tn_test = {
    r <- res()
    if (is.null(r) || isTRUE(r$too_few)) NULL
    else list(lengths = r$grid$lengths, diam_classes = r$grid$diam_classes, cm_map = as.list(r$grid$cm_map),
              best_price = unname(r$best_price))
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
    new <- apply_grid_prices(list(pm = r$pm), r$grid, r$best_price)$pm
    sc$replace_scenario(other(r$side), r$side, r$defs, new)
  })
}

#' The tuning on a prepared job (runs in this process): the original-price run of
#' all stems, the tuning on a sample, the run with the best prices.
#' @return list(too_few = TRUE) or list(orig, tuned (fg, value, prod_vol, saw_vol),
#'   trace, best_iter, best_price, grid, change (lengths cm x classes mm, NA = not
#'   steered), lengths_cm).
tuning_run <- function(job, products, defs, key, type, max_iter, grade_opts, on_iter = NULL) {
  hpr <- tuning_hpr_info(defs, products$pm, key)
  grid <- tuning_grid(products, key, hpr)
  W <- tuning_target(grid, type)
  basis <- if (type == "real" && grid$basis_default == "volume") "volume" else "number"
  P1 <- grid$price; a <- !is.na(P1)
  measure <- function(job_x, P) {
    res <- finish_job(job_x, buck_job(job_x))
    tal <- tally_grid(res, key, grid)
    sl <- res$selected_logs_all
    list(tal = tal, fg = fg_grid(tal, W, basis)$overall,
         value = sum(res$tree_summary$total_value_nok) - sum(tal$v[a] * P[a]) + sum(tal$v[a] * P1[a]),
         prod_vol = sum(tal$v), saw_vol = sum(sl$price_volume_m3[as.character(sl$ProductType) == "sawlog"]))
  }
  o <- measure(job, P1)
  n_logs <- sum(o$tal$n)
  if (n_logs < TUNE_MIN_LOGS || !any(colSums(o$tal$n) >= TUNE_MIN_LOGS & colSums(!is.na(W)) > 0))
    return(list(too_few = TRUE))
  n <- tuning_sample_size(length(job$pvs), n_logs)
  tn <- tune_prices(job, products, key, type, n = n, seed = 1, max_iter = max_iter, hpr = hpr,
                    early_stop = FALSE, on_iter = on_iter)
  Pb <- tn$best_price
  job2 <- swap_job_prices(job, apply_grid_prices(products, grid, Pb))
  t2 <- measure(job2, Pb)
  steer <- !is.na(W) & a & matrix(tn$eligible, nrow(P1), ncol(P1), byrow = TRUE)
  change <- Pb - P1; change[!steer] <- NA_real_
  rownames(change) <- NULL; colnames(change) <- colnames(P1)
  list(orig = o[c("fg", "value", "prod_vol", "saw_vol")], tuned = t2[c("fg", "value", "prod_vol", "saw_vol")],
       trace = tn$trace, best_iter = tn$best_iter, best_price = Pb, grid = grid,
       change = change, lengths_cm = grid$lengths * 10, n = n, n_logs = n_logs)
}
