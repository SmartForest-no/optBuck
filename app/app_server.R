# app/app_server.R
#
# Purpose : Shiny server function (`server`). Sourced by app.R after setup.R
#           and the app helpers (helpers_ui.R, helpers_validation.R,
#           helpers_upload.R, helpers_jobs.R). Styling via CSS classes
#           (www/apter.css) and APTER_CHART colours (theme.R); no inline colours.
#           Not named server.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# State   : Scenarios A and B (assortments and prices) live in
#           scenario_server() (server_scenarios.R, Phase 4d); this file holds
#           the Simulator: tree sources, runs and results.
#           Internal names: "default" and "scenario" in the backend mean A and B.

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Sidebar: NFI selection ----------------------------------------------

  # Bonitet choices: values available for the selected HKL group and county.
  output$abonitet_ui <- renderUI({
    hkl_raw    <- unlist(HKL_MAP[input$hkl_group])
    fyl        <- selected_fylnr(input$fylnr_sel)
    sub <- filter_stratum_plots(plots_prod, hkl_raw, fyl = fyl)
    abon_avail <- as.character(sort(unique(as.integer(sub$abonitet))))
    if (length(abon_avail) == 0L)
      abon_avail <- as.character(sort(unique(as.integer(plots_prod$abonitet))))
    lbl <- BONITET_LABELS[abon_avail]
    lbl[is.na(lbl)] <- abon_avail[is.na(lbl)]
    choices_named <- setNames(abon_avail, lbl)
    pref <- intersect(c("14", "11", "17"), abon_avail)
    sel  <- if (length(pref) > 0L) pref else abon_avail[1L]
    checkboxGroupInput("abonitet", "Site class (bonitet)",
                       choiceNames  = unname(choices_named),
                       choiceValues = abon_avail,
                       selected     = sel)
  })

  # Only real errors: an empty selection or no matching plots.
  output$stratum_error <- renderUI({
    hkl_raw <- unlist(HKL_MAP[input$hkl_group])
    abon    <- as.integer(input$abonitet)
    if (length(hkl_raw) == 0L || length(abon) == 0L)
      return(div(class = "note-error", "Select at least one maturity class and one site class."))
    if (count_eligible_plots(plots_prod, hkl_raw, abon, selected_fylnr(input$fylnr_sel)) == 0L)
      div(class = "note-error", "No plots match this selection.")
  })

  # ---- Uploaded tree list (Phase 5; R/helpers_tree_list.R) -----------------
  tl_prepared <- reactiveVal(NULL)   # prepare_tree_list() result + $file

  observeEvent(input$tl_upload, {
    f <- input$tl_upload; req(f)
    rd <- read_tree_list(f$datapath, f$name)
    p <- if (length(rd$errors) > 0L)
      list(ok = FALSE, errors = rd$errors, warnings = character(0))
    else withProgress(message = "Checking the tree list and imputing heights...", value = 0.5,
                      prepare_tree_list(rd$data))
    p$file <- f$name
    tl_prepared(p)
  })

  output$tl_validation <- renderUI({
    p <- tl_prepared()
    if (is.null(p)) return(NULL)
    h  <- p$heights
    ex <- if (!is.null(p$trees)) p$trees[!p$trees$in_range, , drop = FALSE]
    tagList(
      div(class = if (isTRUE(p$ok)) "pm-msg-ok" else "pm-msg-error",
          if (isTRUE(p$ok))
            sprintf("%s: %d trees, %d within taperNOR's range.", p$file, nrow(p$trees),
                    sum(p$trees$in_range))
          else paste(c(sprintf("%s cannot be used:", p$file), p$errors), collapse = "\n")),
      if (length(p$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", p$warnings, collapse = "\n")),
      if (!is.null(ex) && nrow(ex) > 0L)
        tags$details(class = "tl-excluded",
          tags$summary(sprintf("%d tree(s) not bucked", nrow(ex))),
          tags$ul(lapply(seq_len(nrow(ex)), function(i)
            tags$li(sprintf("%s: %s", ex$treid[i], ex$excluded_reason[i]))))),
      if (!is.null(h) && nrow(h) > 0L)
        div(class = "tl-heights",
            div(class = "section-hdr", "Heights"),
            tags$ul(lapply(seq_len(nrow(h)), function(i) tags$li(sprintf(
              "%s: %d trees, %d measured; %s%s", h$species[i], h$n_trees[i], h$n_measured[i],
              if (h$method[i] == "fieldPlot") sprintf("fieldPlot (%s)", h$model[i]) else h$method[i],
              if (!is.na(h$ratio[i])) sprintf(", ratio %.2f", h$ratio[i]) else "")))))
    )
  })

  output$dl_tree_template <- downloadHandler(
    filename = function() "apter_tree_list_template.csv",
    content  = function(file) tree_list_template(file))


  # Selections above LARGE_RUN_TREES trees: tree count and a rough run time.
  output$large_run_note <- renderUI({
    req(identical(input$tree_source, "nfi"))
    hkl_raw <- unlist(HKL_MAP[input$hkl_group]); abon <- as.integer(input$abonitet)
    req(length(hkl_raw) > 0L, length(abon) > 0L)
    n <- count_stratum_trees(trees_meta, input$species_group, hkl_raw, abon,
                             selected_fylnr(input$fylnr_sel))
    if (n <= LARGE_RUN_TREES) return(NULL)
    secs <- estimate_run_seconds(n, APP_WORKERS, compare = sc$differ())
    div(class = "note-warn", sprintf("%s trees: about %s.", format(n, big.mark = ","),
        if (secs < 90) sprintf("%.0f s", secs) else sprintf("%.0f min", secs / 60)))
  })


  # ---- Assortments and prices (server_scenarios.R) ---------------------------
  sc <- scenario_server(input, output, session)


  # ---- Runs as background tasks (helpers_jobs.R) ---------------------------
  # The backend runs on the workers (R/helpers_parallel.R) as an
  # ExtendedTask: the app stays responsive, the run status shows progress
  # and Run is disabled while a task is running. The browser shows the bar
  # as soon as Run is clicked (www/apter.js); the server replaces it with
  # staged progress: loading (NFI data and taper profiles, or taperNOR for
  # tree lists), bucking, summarising.
  run_progress <- reactiveVal(NULL)
  progress_cb  <- function(done, total, what)
    run_progress(list(done = done, total = total, what = what))

  run_task <- ExtendedTask$new(function(spec, extra) {
    p <- promises::then(run_job_async(spec, progress_cb), function(res) {
      for (n in names(extra)) res[[n]] <- extra[[n]]
      res
    })
    p <- promises::catch(p, function(e) {
      showNotification(paste0("Backend error: ", conditionMessage(e)), type = "error",
                       duration = NULL, session = session)
      NULL
    })
    promises::finally(p, function() unlink(spec$files))
  })
  task_busy <- reactive(run_task$status() == "running")
  observe(updateActionButton(session, "run_btn", disabled = task_busy()))

  # Run: Scenario A, and B on the same trees when B differs from A (one
  # candidate pass for both; the backend's `scenario`).
  run_rejected <- reactiveVal(FALSE)
  run_src      <- reactiveVal("nfi")
  observeEvent(input$run_btn, {
    reject <- function(msg) {
      session$sendCustomMessage("apter_progress_clear", TRUE)
      showNotification(msg, type = "error", duration = 10); run_rejected(TRUE)
    }
    src <- if (identical(input$tree_source, "list")) "list" else "nfi"
    tl  <- if (src == "list") tl_prepared() else NULL
    if (src == "list" && !isTRUE(tl$ok))
      return(reject("Upload a tree list that passes the checks first."))
    if (src == "nfi" && (length(input$hkl_group) == 0L || length(input$abonitet) == 0L))
      return(reject("Select at least one maturity class and one site class."))

    d <- sc$defs_a(); a <- pm_minimal(sc$pm_a())
    va <- check_price_set(d, a)
    if (!va$ok) return(reject(paste(c("Scenario A cannot be used:", va$errors), collapse = "\n")))
    compare <- sc$differ(); b <- NULL
    if (compare) {
      b <- list(defs = sc$defs_b(), pm = pm_minimal(sc$pm_b()))
      vb <- check_price_set(b$defs, b$pm)
      if (!vb$ok) return(reject(paste(c("Scenario B cannot be used:", vb$errors), collapse = "\n")))
    }
    run_rejected(FALSE)

    # A with the default assortments and prices runs exactly as before
    # Phase 4c (the default price file and products).
    files <- character(0); path <- "default_2021"; pd <- NULL; lc <- NULL
    if (!(identical(d, APP_DEFS) && identical(a, APP_PM_DEFAULT))) {
      path <- write_price_file(a); files <- path
      p <- products_from_defs(d, a); pd <- p$pd; lc <- p$lc
    }
    scenario <- NULL
    if (compare) {
      path_b <- write_price_file(b$pm); files <- c(files, path_b)
      pb <- products_from_defs(b$defs, b$pm)
      scenario <- list(price_matrix_path = path_b, products = pb$pd, length_classes = pb$lc)
    }
    spec <- list(src = src, params = if (src == "nfi") build_run_params(input) else list(),
                 tl = tl, list_params = list(file = tl$file), path = path, pd = pd, lc = lc,
                 scenario = scenario, files = files)
    run_src(src)
    run_progress(list(done = 0L, total = NA_integer_,
                      what = if (src == "nfi") "Loading NFI data and taper profiles"
                             else "Taper profiles"))
    run_task$invoke(spec, list(price_matrix = a, price_matrix_b = if (compare) b$pm,
                               state = sc$pm_state()))
  })

  result <- reactive({
    if (run_rejected()) return(NULL)
    run_task$result()
  })
  has_result <- reactive(input$run_btn > 0L && !task_busy() && !is.null(result()))
  compared   <- reactive(has_result() && !is.null(result()$scenario))

  # B's run (the backend's $scenario), with its prices.
  result_b <- reactive({
    req(has_result()); r <- result(); req(!is.null(r$scenario))
    s <- r$scenario; s$price_matrix <- r$price_matrix_b
    s
  })
  # Shown when the last finished run compared A and B. While a run is busy
  # the result is pending and this observer waits, so the tab stays as it
  # is (hiding the open tab would leave its outputs stale).
  observe({
    show <- input$run_btn > 0L && !is.null(result()) && !is.null(result()$scenario)
    if (show) return(nav_show("result_tabs", "comparison"))
    if (identical(isolate(input$result_tabs), "comparison")) nav_select("result_tabs", "summary")
    nav_hide("result_tabs", "comparison")
  })

  # One bar over the whole run: loading (unknown length: animated), then
  # the stages' shares of the bar.
  STAGES <- list(nfi  = list("Bucking trees" = c(0.10, 0.95), "Summarising results" = c(0.95, 1)),
                 list = list("Taper profiles" = c(0, 0.40), "Bucking trees" = c(0.40, 0.95),
                             "Summarising results" = c(0.95, 1)))
  output$run_status <- renderUI({
    if (!task_busy()) return(NULL)
    p <- run_progress()
    if (is.null(p)) p <- list(done = 0L, total = NA_integer_, what = "Starting run")
    rng <- STAGES[[run_src()]][[p$what]]
    known <- !is.null(rng) && !is.na(p$total) && p$total > 0L
    frac <- if (known) rng[1] + (rng[2] - rng[1]) * p$done / p$total else NA
    div(class = "status-running",
        if (known) sprintf("%s: %s / %s", p$what, format(p$done, big.mark = ","),
                           format(p$total, big.mark = ",")) else paste0(p$what, "..."),
        div(class = "run-progress",
            if (known) div(class = "run-progress-bar", style = sprintf("width: %.0f%%", 100 * frac))
            else div(class = "run-progress-bar is-indeterminate")))
  })

  output$stale_notice <- renderUI({
    req(has_result())
    if (!identical(sc$pm_state(), result()$state))
      div(class = "stale-notice", "Assortments or prices changed since this run.")
  })

  # Heights from reference curves (tree lists): shown above all results.
  output$height_banner <- renderUI({
    if (!has_result() || !identical(result()$tree_source, "tree_list")) return(NULL)
    w <- result()$height_warnings
    w <- w[grepl("FinnHoyde", w, fixed = TRUE)]
    if (length(w) == 0L) return(NULL)
    div(class = "height-notice", tags$strong("Heights: "),
        tagList(lapply(w, function(x) div(x))))
  })


  # ---- readable labels ----------------------------------------------------------
  # "spruce_sawlog" -> "Spruce sawlog"; assortment classes ("sawlog") -> "Sawlog".
  asm_label  <- function(k) {
    k <- as.character(k)
    sp <- sub("_.*$", "", k); rest <- gsub("_", " ", sub("^[^_]+_", "", k))
    ifelse(grepl("_", k), paste(tools::toTitleCase(sp), rest), k)
  }
  type_label <- function(t) {
    t <- gsub("_", " ", as.character(t))
    paste0(toupper(substring(t, 1, 1)), substring(t, 2))
  }
  scope_label <- function(s) ifelse(s == "Total", "Total", tools::toTitleCase(as.character(s)))
  # Per-m3 assortments: classes in the Total scope, products per species.
  per_m3_asm_label <- function(scope, a) ifelse(scope == "Total", type_label(a), asm_label(a))

  LOG_LABELS <- c(
    plot_id_github = "Plot", treid = "Tree", species_group = "Species",
    ProductKey = "Assortment", StartPos = "StartPos", StopPos = "StopPos",
    LogLength = "Length (cm)", LengthClass_dm = "Length class (dm)",
    TopDiameter_ob = "Top Ø over bark (mm)", TopDiameter_ub = "Top Ø under bark (mm)",
    PriceDiameterClass_cm_uncapped = "Diameter class (cm)",
    PriceDiameterClass_cm_used = "Price class used (cm)",
    PriceNOKperM3 = "Price (NOK/m³)", price_volume_m3 = "Price volume (m³)",
    commercial_volume_m3 = "Commercial volume u.b. (m³)", value_nok = "Value (NOK)")
  readable_logs <- function(sl) {
    cols <- intersect(names(LOG_LABELS), names(sl))
    out <- sl[, cols, drop = FALSE]
    if ("ProductKey" %in% cols) out$ProductKey <- asm_label(out$ProductKey)
    if ("species_group" %in% cols) out$species_group <- tools::toTitleCase(out$species_group)
    for (n in intersect(c("price_volume_m3", "commercial_volume_m3"), cols)) out[[n]] <- round(out[[n]], 4)
    for (n in intersect(c("value_nok", "PriceNOKperM3"), cols)) out[[n]] <- round(out[[n]], 2)
    names(out) <- unname(LOG_LABELS[cols])
    out
  }


  # ---- Summary ----------------------------------------------------------------
  fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  output$summary_cards <- renderUI({
    if (input$run_btn == 0L) return(div(class = "note-muted", "No run yet."))
    req(has_result()); r <- result()
    tot <- function(r) r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
    vol   <- function(r) format(round(tot(r)$commercial_volume_m3, 2), nsmall = 2, big.mark = ",")
    value <- function(r) fmt_int(sum(r$selected_logs_all$value_nok, na.rm = TRUE))
    nok   <- function(r) { x <- tot(r)$nok_per_m3; if (is.na(x)) "–" else format(round(x, 1), nsmall = 1) }
    pvol  <- function(r) format(round(sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE), 2),
                                nsmall = 2, big.mark = ",")
    if (!compared())
      return(ro_field_row(
        ro_field("Run time", sprintf("%.1f s", r$elapsed_seconds)),
        ro_field("Plots", fmt_int(r$n_plots_sampled)),
        ro_field("Trees", fmt_int(r$n_trees_total)),
        ro_field("Trees with logs", fmt_int(r$n_trees_with_logs)),
        ro_field("Logs", fmt_int(nrow(r$selected_logs_all))),
        ro_field("Commercial volume (m³ u.b.)", vol(r)),
        ro_field("Total value (NOK)", value(r)),
        ro_field("NOK per m³", nok(r)),
        ro_field("Price volume (m³)", pvol(r))))
    # A comparison: the run once, then Scenario A (left) and B (right).
    scen <- function(title, x) div(class = "summary-scen",
      div(class = "section-hdr", title),
      ro_field_row(
        ro_field("Total value (NOK)", value(x)),
        ro_field("Commercial volume (m³ u.b.)", vol(x)),
        ro_field("NOK per m³", nok(x)),
        ro_field("Price volume (m³)", pvol(x))))
    tagList(
      ro_field_row(
        ro_field("Run time", sprintf("%.1f s", r$elapsed_seconds)),
        ro_field("Plots", fmt_int(r$n_plots_sampled)),
        ro_field("Trees", fmt_int(r$n_trees_total))),
      layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                     scen("Scenario A", r), scen("Scenario B", result_b())))
  })


  # ---- Per m3 -----------------------------------------------------------------
  output$tbl_per_m3 <- renderReactable(apter_reactable(local({
    req(has_result()); s <- result()$per_m3_summary
    data.frame(Scope = scope_label(s$scope), Trees = s$n_trees,
               `Value (NOK)` = round(s$value_nok, 2),
               `Commercial volume u.b. (m³)` = round(s$commercial_volume_m3, 4),
               `NOK per m³` = round(s$nok_per_m3, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$tbl_per_m3_assortment <- renderReactable(apter_reactable(local({
    req(has_result()); a <- result()$per_m3_by_assortment
    data.frame(Scope = scope_label(a$scope), Assortment = per_m3_asm_label(a$scope, a$assortment),
               `Value (NOK)` = round(a$value_nok, 2),
               `Commercial volume u.b. (m³)` = round(a$commercial_volume_m3, 4),
               `NOK per m³ (contribution)` = round(a$nok_per_m3_contribution, 1),
               `Volume share (%)` = round(100 * a$volume_share, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  }), page_size = 20L))

  total_by_assortment <- function() {
    a <- result()$per_m3_by_assortment
    a[a$scope == "Total", , drop = FALSE]
  }
  output$bar_per_m3_contribution <- renderPlot(res = 96, {
    req(has_result()); a <- total_by_assortment(); req(nrow(a) > 0L)
    apter_barplot(setNames(a$nok_per_m3_contribution, type_label(a$assortment)),
                  col = APTER_CHART$series1,
                  main = "NOK per m³ by assortment (contribution)",
                  ylab = "NOK / m³",
                  fmt = function(v) format(round(v, 1), nsmall = 1, trim = TRUE))
  })

  output$bar_volume_share <- renderPlot(res = 96, {
    req(has_result()); a <- total_by_assortment(); req(nrow(a) > 0L)
    apter_barplot(setNames(100 * a$volume_share, type_label(a$assortment)),
                  col = APTER_CHART$series1,
                  main = "Share of commercial volume by assortment",
                  ylab = "%",
                  fmt = function(v) sprintf("%.1f %%", v))
  })

  # Trees: a server-side paged table (all plots of a stratum can be tens of
  # thousands of trees); the selected tree's logs are shown below it.
  trees_df <- reactive({
    req(has_result()); ts <- result()$tree_summary
    data.frame(Plot = ts$plot_id_github, Tree = ts$treid,
               Species = tools::toTitleCase(ts$species_group), `DBH (cm)` = ts$dbh_cm,
               `Height (m)` = round(ts$height_m, 1), `Height source` = ts$height_source,
               Logs = ts$n_logs_selected,
               `Value (NOK)` = round(ts$total_value_nok, 2),
               `Commercial volume u.b. (m³)` = round(ts$commercial_volume_m3, 4),
               `NOK per m³` = round(ts$nok_per_m3, 1),
               Status = ts$optimizer_status,
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  tree_sel <- paged_table_server(input, output, session, "tbl_trees", trees_df, page_size = 15L,
                                 selection = TRUE, filename = "optBuck_trees.csv")
  output$tree_logs <- renderUI({
    i <- tree_sel()
    if (is.null(i)) return(div(class = "note-muted", "Select a tree to see its logs."))
    tr <- trees_df()[i, ]
    sl <- result()$selected_logs_all
    logs <- sl[sl$plot_id_github %in% tr$Plot & sl$treid == tr$Tree, , drop = FALSE]   # plot may be NA
    tagList(div(class = "section-hdr", sprintf("Logs of tree %s%s", tr$Tree,
                                               if (!is.na(tr$Plot)) sprintf(" (plot %s)", tr$Plot) else "")),
            if (nrow(logs) == 0L) div(class = "note-muted", "No selected logs for this tree.")
            else apter_reactable(readable_logs(logs)[, -(1:3)], page_size = 20L))
  })


  # ---- Products -----------------------------------------------------------------
  products_ab <- reactive({
    req(has_result()); p1 <- result()$product_summary
    if (!compared()) return(NULL)
    p2 <- result_b()$product_summary
    keys <- union(p1$ProductKey, p2$ProductKey)
    g <- function(p, col) { v <- p[[col]][match(keys, p$ProductKey)]; v[is.na(v)] <- 0; v }
    list(keys = keys,
         logs_a = g(p1, "n_logs"), logs_b = g(p2, "n_logs"),
         vol_a = g(p1, "total_volume_m3"), vol_b = g(p2, "total_volume_m3"),
         val_a = g(p1, "total_value_nok"), val_b = g(p2, "total_value_nok"))
  })

  output$tbl_products <- renderReactable({
    req(has_result())
    v <- products_ab()
    if (is.null(v)) {
      ps <- result()$product_summary
      return(apter_reactable(data.frame(
        Assortment = asm_label(ps$ProductKey), Logs = ps$n_logs,
        `Price volume (m³)` = round(ps$total_volume_m3, 3),
        `Value (NOK)` = round(ps$total_value_nok, 0),
        `Mean price (NOK/m³)` = round(ps$mean_value_nok_per_m3, 1),
        check.names = FALSE, stringsAsFactors = FALSE), page_size = 20L))
    }
    apter_reactable(data.frame(
      Assortment = asm_label(v$keys),
      `Logs A` = v$logs_a, `Logs B` = v$logs_b,
      `Price volume A (m³)` = round(v$vol_a, 3), `Price volume B (m³)` = round(v$vol_b, 3),
      `Value A (NOK)` = round(v$val_a, 0), `Value B (NOK)` = round(v$val_b, 0),
      check.names = FALSE, stringsAsFactors = FALSE), page_size = 20L)
  })

  output$bar_value_by_product <- renderPlot(res = 96, {
    req(has_result()); v <- products_ab()
    if (!is.null(v))
      return(apter_barplot_ab(v$val_a, v$val_b, asm_label(v$keys),
                              main = "Value (NOK) by assortment", ylab = "NOK", fmt = fmt_int))
    ps <- result()$product_summary; req(nrow(ps) > 0L)
    apter_barplot(setNames(ps$total_value_nok, asm_label(ps$ProductKey)), col = APTER_CHART$series1,
                  main = "Value (NOK) by assortment", ylab = "NOK", fmt = fmt_int)
  })

  output$bar_volume_by_product <- renderPlot(res = 96, {
    req(has_result()); v <- products_ab()
    f2 <- function(x) format(round(x, 1), nsmall = 1, trim = TRUE)
    if (!is.null(v))
      return(apter_barplot_ab(v$vol_a, v$vol_b, asm_label(v$keys),
                              main = "Price volume (m³) by assortment", ylab = "m³", fmt = f2))
    ps <- result()$product_summary; req(nrow(ps) > 0L)
    apter_barplot(setNames(ps$total_volume_m3, asm_label(ps$ProductKey)), col = APTER_CHART$series1,
                  main = "Price volume (m³) by assortment", ylab = "m³", fmt = f2)
  })


  # ---- Selected logs ------------------------------------------------------------
  logs_df <- reactive({
    req(has_result()); readable_logs(result()$selected_logs_all)
  })
  paged_table_server(input, output, session, "tbl_selected_logs", logs_df, page_size = 25L,
                     filename = "optBuck_selected_logs.csv")


  # ---- Scenario comparison (A vs B) ------------------------------------------
  # Per-tree pattern comparison (R/helpers_comparison.R).
  cmp_trees <- reactive({
    r1 <- result(); r2 <- result_b()
    compare_cutting_patterns(r1, r2, r1$price_matrix, r2$price_matrix)
  })

  output$tbl_cmp_overview <- renderReactable(apter_reactable(local({
    r1 <- result(); r2 <- result_b()
    mk <- function(r, nm) {
      tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
      data.frame(Scenario = nm, Plots = r$n_plots_sampled, Trees = r$n_trees_total,
                 `Trees with logs` = r$n_trees_with_logs,
                 `Value (NOK)` = round(sum(r$selected_logs_all$value_nok, na.rm = TRUE), 0),
                 `Price volume (m³)` = round(sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE), 3),
                 `Commercial volume u.b. (m³)` = round(tot$commercial_volume_m3, 3),
                 `NOK per m³` = round(tot$nok_per_m3, 1),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    rbind(mk(r1, "Scenario A"), mk(r2, "Scenario B"))
  })))

  output$tbl_cmp_delta <- renderReactable(apter_reactable(local({
    r1 <- result(); r2 <- result_b()
    v1 <- sum(r1$selected_logs_all$value_nok, na.rm = TRUE)
    v2 <- sum(r2$selected_logs_all$value_nok, na.rm = TRUE)
    m1 <- sum(r1$selected_logs_all$price_volume_m3, na.rm = TRUE)
    m2 <- sum(r2$selected_logs_all$price_volume_m3, na.rm = TRUE)
    t1 <- r1$per_m3_summary[r1$per_m3_summary$scope == "Total", ]
    t2 <- r2$per_m3_summary[r2$per_m3_summary$scope == "Total", ]
    pct <- function(a, b) if (abs(b) > 1e-9) round((a - b) / abs(b) * 100, 2) else 0
    data.frame(
      `Value (NOK)`                 = round(v2 - v1, 0),
      `Value (%)`                   = pct(v2, v1),
      `Price volume (m³)`           = round(m2 - m1, 3),
      `Price volume (%)`            = pct(m2, m1),
      `Commercial volume u.b. (m³)` = round(t2$commercial_volume_m3 - t1$commercial_volume_m3, 3),
      `NOK per m³`                  = round(t2$nok_per_m3 - t1$nok_per_m3, 1),
      `NOK per m³ (%)`              = pct(t2$nok_per_m3, t1$nok_per_m3),
      `Trees changed`               = sum(cmp_trees()$changed),
      check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$tbl_cmp_per_m3 <- renderReactable(apter_reactable(local({
    x <- compare_per_m3(result(), result_b())
    data.frame(Scope = scope_label(x$scope), Assortment = per_m3_asm_label(x$scope, x$assortment),
               `NOK per m³ A` = round(x$nok_per_m3_default, 1),
               `NOK per m³ B` = round(x$nok_per_m3_scenario, 1),
               `Change (NOK per m³)` = round(x$delta_nok_per_m3, 1),
               `Volume share A (%)` = round(100 * x$volume_share_default, 1),
               `Volume share B (%)` = round(100 * x$volume_share_scenario, 1),
               `Change (percentage points)` = round(100 * x$delta_volume_share, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  }), page_size = 20L))

  output$cmp_patterns <- renderUI({
    x <- cmp_trees()
    div(class = "stat-strip",
        tags$strong(sprintf("%s of %s trees changed their cutting pattern",
                            fmt_int(sum(x$changed)), fmt_int(nrow(x)))),
        if (sum(x$tie_only) > 0L)
          sprintf(" (%d more differ only by equal-value ties)", sum(x$tie_only)))
  })

  cmp_changed_df <- reactive({
    x <- cmp_trees(); x <- x[x$changed, , drop = FALSE]
    x <- x[order(-abs(x$value_scenario - x$value_default)), , drop = FALSE]
    fin <- function(v) ifelse(is.finite(v), v, NA_real_)   # a log not priced in the other scenario
    data.frame(
      Plot = x$plot_id_github, Tree = x$treid, Species = tools::toTitleCase(x$species_group),
      `Logs A` = x$n_logs_default, `Logs B` = x$n_logs_scenario,
      `Value A (NOK)` = round(x$value_default, 2),
      `Value B (NOK)` = round(x$value_scenario, 2),
      `Gain at B prices (NOK)` = round(fin(x$gain_at_scenario), 2),
      `Gain at A prices (NOK)` = round(fin(x$gain_at_default), 2),
      `Largest volume change (m³)` = round(x$max_volume_diff_m3, 4),
      check.names = FALSE, stringsAsFactors = FALSE)
  })
  paged_table_server(input, output, session, "tbl_cmp_trees", cmp_changed_df, page_size = 15L,
                     filename = "optBuck_changed_trees.csv")

  # Tree picker: server-side choices (all trees of a stratum).
  observe({
    req(compared()); x <- cmp_trees()
    x <- x[order(!x$changed, -abs(x$value_scenario - x$value_default)), , drop = FALSE]
    lbl <- sprintf("%s / %s (%s%s)", x$plot_id_github, x$treid, x$species_group,
                   ifelse(x$changed, sprintf(", %+.1f NOK", x$value_scenario - x$value_default),
                          ", unchanged"))
    updateSelectizeInput(session, "cmp_tree", server = TRUE,
                         choices = setNames(paste(x$plot_id_github, x$treid, sep = "|"), lbl),
                         selected = paste(x$plot_id_github, x$treid, sep = "|")[1L])
  })

  output$stem_compare <- renderPlot(res = 96, {
    req(compared(), !is.null(input$cmp_tree), nzchar(input$cmp_tree))
    k  <- input$cmp_tree
    tk <- function(df) paste(df$plot_id_github, df$treid, sep = "|")
    l1 <- result()$selected_logs_all;   l1 <- l1[tk(l1) == k, , drop = FALSE]
    l2 <- result_b()$selected_logs_all; l2 <- l2[tk(l2) == k, , drop = FALSE]
    ts <- result()$tree_summary; tr <- ts[tk(ts) == k, , drop = FALSE]
    req(nrow(tr) == 1L)
    apter_stem_compare(l1, l2, tr$height_m,
                       sprintf("Tree %s (%s, dbh %.1f cm, height %.1f m)",
                               tr$treid, tr$species_group, tr$dbh_cm, tr$height_m))
  })

  output$tbl_cmp_products <- renderReactable({
    v <- products_ab(); req(v)
    pct <- function(a, b) ifelse(abs(b) > 1e-9, round((a - b) / abs(b) * 100, 2), 0)
    # Share of each run's total commercial volume under bark, per product.
    share <- function(r, keys) {
      sl  <- r$selected_logs_all
      tot <- sum(sl$commercial_volume_m3, na.rm = TRUE)
      vapply(keys, function(k)
        if (tot > 0) sum(sl$commercial_volume_m3[sl$ProductKey == k]) / tot else 0,
        numeric(1), USE.NAMES = FALSE)
    }
    apter_reactable(data.frame(
      Assortment = asm_label(v$keys),
      `Value A (NOK)` = round(v$val_a, 0), `Value B (NOK)` = round(v$val_b, 0),
      `Value change (NOK)` = round(v$val_b - v$val_a, 0),
      `Value change (%)` = pct(v$val_b, v$val_a),
      `Price volume A (m³)` = round(v$vol_a, 3), `Price volume B (m³)` = round(v$vol_b, 3),
      `Volume change (m³)` = round(v$vol_b - v$vol_a, 3),
      `Volume change (%)` = pct(v$vol_b, v$vol_a),
      `Volume share A (%)` = round(100 * share(result(), v$keys), 1),
      `Volume share B (%)` = round(100 * share(result_b(), v$keys), 1),
      check.names = FALSE, stringsAsFactors = FALSE), page_size = 20L)
  })

  output$bar_cmp_value <- renderPlot(res = 96, {
    v <- products_ab(); req(v)
    apter_barplot_ab(v$val_a, v$val_b, asm_label(v$keys),
                     main = "Value by assortment", ylab = "NOK", fmt = fmt_int)
  })

  output$bar_cmp_delta <- renderPlot(res = 96, {
    v <- products_ab(); req(v)
    # Sign is carried by bar direction from the zero baseline and the signed
    # label, so one neutral colour is used (no colour-only meaning).
    apter_barplot(setNames(v$val_b - v$val_a, asm_label(v$keys)), col = APTER_CHART$neutral,
                  main = "Value change (B − A) by assortment", ylab = "NOK",
                  fmt = function(x) ifelse(x > 0, paste0("+", fmt_int(x)), fmt_int(x)))
  })

}  # end server
