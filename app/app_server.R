# app/app_server.R
#
# Purpose : Shiny server function (`server`). Sourced by app.R after setup.R
#           and the app helpers (helpers_ui.R, helpers_validation.R,
#           helpers_upload.R). Styling via CSS classes (www/apter.css) and
#           APTER_CHART colours (theme.R); no inline colours.
#           Not named server.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Dynamic bonitet selector — filtered to available values for selected HKL group and county
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

  # Selection availability preview
  output$stratum_preview <- renderUI({
    hkl_raw <- unlist(HKL_MAP[input$hkl_group])
    abon    <- as.integer(input$abonitet)
    fyl     <- selected_fylnr(input$fylnr_sel)
    if (length(hkl_raw) == 0L || length(abon) == 0L)
      return(div(class = "note-error",
                 "Select at least one maturity class and one bonitet."))
    n_elig <- count_eligible_plots(plots_prod, hkl_raw, abon, fyl)
    div(
      sprintf("%d eligible plot(s) available.", n_elig), br(),
      if (n_elig == 0L)
        span(class = "note-error", "No matching plots \u2014 adjust selection.")
      else if (n_elig < as.integer(input$n_plots))
        span(class = "note-warn",
             sprintf("Fewer than %s plots exist; all will be used.", input$n_plots))
      else
        span(class = "note-ok", "Selection OK.")
    )
  })

  # Run status line
  output$run_status <- renderUI({
    if (input$run_btn == 0L)
      return(div(class = "status-idle",
                 "Select parameters and click Run optimizer."))
    r <- result()
    if (is.null(r))
      return(div(class = "status-error",
                 "Run failed. See error message above."))
    div(class = "status-done",
        sprintf("Run complete in %.1f s  |  %d plot(s)  |  %d trees  |  %d selected logs  |  ID: %s",
                r$elapsed_seconds, r$n_plots_sampled, r$n_trees_total,
                nrow(r$selected_logs_all), r$run_id))
  })

  # Run card: parameters as read-only fields, stages as a step list
  output$run_params <- renderUI({
    req(input$run_btn > 0L)
    r <- result(); req(!is.null(r))
    p <- r$params
    fyl <- if (is.null(p$fylnr)) "All counties"
           else paste(names(FYLNR_CHOICES)[match(p$fylnr, FYLNR_CHOICES)],
                      collapse = ", ")
    ro_field_row(
      ro_field("Species group", p$species_group),
      ro_field("Maturity class (hogstkl)", paste(p$hogstkl, collapse = ", ")),
      ro_field("Site class (bonitet)", paste(p$abonitet, collapse = ", ")),
      ro_field("County", fyl),
      ro_field("Plots sampled", sprintf("%d of %d requested",
                                        r$n_plots_sampled, as.integer(p$n_plots))),
      ro_field("Seed", p$seed),
      ro_field("Run ID", r$run_id, wide = TRUE)
    )
  })

  output$run_steps <- renderUI({
    r <- if (input$run_btn > 0L) result() else NULL
    if (is.null(r)) return(step_list(run_step_table()))
    step_list(run_step_table(r$diagnostics$stage_timings, r$started_at))
  })

  output$home_last_run <- renderUI({
    r <- if (input$run_btn > 0L) result() else NULL
    if (is.null(r))
      return(div(class = "note-muted",
                 "No run yet. Open the simulator, choose trees and click Run optimizer."))
    total_val <- sum(r$selected_logs_all$value_nok, na.rm = TRUE)
    tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
    div(class = "status-tiles",
        status_tile(r$run_id, "Done",
                    c(sprintf("%s \u00b7 %d plot(s) \u00b7 %d trees",
                              r$params$species_group, r$n_plots_sampled,
                              r$n_trees_total),
                      sprintf("%s NOK per m\u00b3 \u00b7 total value %s NOK \u00b7 %d logs",
                              format(round(tot$nok_per_m3, 1), nsmall = 1),
                              format(round(total_val), big.mark = ",",
                                     scientific = FALSE),
                              nrow(r$selected_logs_all)))))
  })

  observeEvent(input$go_simulator, nav_select("main_nav", "Simulator"))

  # Backend call with multi-stage progress indicator
  result <- eventReactive(input$run_btn, {
    req(length(input$hkl_group) > 0L, length(input$abonitet) > 0L)

    # Map user-facing inputs (HKL group, county) -> backend params
    params     <- build_run_params(input)
    started_at <- Sys.time()   # for the run step list
    # Products and prices in effect at this Run, for both runs (result_s2
    # uses the scenario's); later edits wait for the next Run.
    inp_def <- run_inputs("default")
    inp_scn <- run_inputs("scenario")
    state   <- pm_state()

    withProgress(message = "Preparing inputs...", value = 0, {
      incProgress(0.10, detail = "Loading price matrix and products")
      r <- tryCatch({
        incProgress(0.15, detail = "Sampling plots and loading taper profiles")
        Sys.sleep(0)  # yield to allow UI refresh
        incProgress(0.20, detail = "Running bucking optimizer...")
        res <- if (!inp_def$custom) {
          run_sampled_plot_bucking(
            params            = params,
            price_matrix_path = "default_2021",
            root              = ROOT)
        } else {
          # Custom assortments used in the default run (Phase 4b).
          path <- write_scenario_pm(inp_def$pm)
          on.exit(tryCatch(file.remove(path), error = function(e) NULL), add = TRUE)
          run_sampled_plot_bucking(params = params, price_matrix_path = path,
                                   root = ROOT, products = inp_def$pd,
                                   length_classes = inp_def$lc)
        }
        incProgress(0.45, detail = "Summarising results")
        Sys.sleep(0)
        incProgress(0.10, detail = "Rendering outputs")
        res$started_at      <- started_at
        res$price_matrix    <- if (inp_def$custom) inp_def$pm else APP_PM_DEFAULT
        res$scenario_inputs <- inp_scn
        res$state           <- state
        res
      }, error = function(e) {
        showNotification(
          paste0("Backend error: ", conditionMessage(e)),
          type = "error", duration = NULL)
        NULL
      })
      r
    })
  })


  # ---- Comparison: S2 reactive, renders ------------------------------------

  # Scenario run: same trees, the scenario products and prices in effect
  # when Run was clicked (result()$scenario_inputs). Later edits take effect
  # at the next Run; until then a notice says the prices changed
  # (scenario_stale()).
  result_s2 <- reactive({
    req(isTRUE(input$compare_on), !is.null(result()))
    params <- isolate(build_run_params(input))
    inp    <- result()$scenario_inputs
    path   <- write_scenario_pm(inp$pm)
    on.exit(tryCatch(file.remove(path), error = function(e) NULL), add = TRUE)
    withProgress(message = "Running scenario prices...", value = 0.5, {
      tryCatch({
        r <- run_sampled_plot_bucking(params = params, price_matrix_path = path,
                                      root = ROOT, products = inp$pd,
                                      length_classes = inp$lc)
        r$price_matrix    <- inp$pm
        r$n_cells_changed <- pm_n_diff(result()$price_matrix, inp$pm)
        r
      }, error = function(e) {
        showNotification(paste0("Scenario error: ", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      })
    })
  })

  # TRUE when prices or custom assortments were edited after the last run.
  scenario_stale <- reactive({
    r <- if (input$run_btn > 0L) result() else NULL
    !is.null(r) && !identical(pm_state(), r$state)
  })
  stale_notice <- function() {
    if (!isTRUE(input$compare_on) || !scenario_stale()) return(NULL)
    div(class = "stale-notice",
        "Scenario prices changed since last run; run again to update.")
  }
  output$scenario_stale_run <- renderUI(stale_notice())
  output$scenario_stale_cmp <- renderUI(stale_notice())

  # Per-tree pattern comparison (R/helpers_comparison.R).
  cmp_trees <- reactive({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    compare_cutting_patterns(result(), result_s2(),
                             result()$price_matrix, result_s2()$price_matrix)
  })

  output$cmp_status <- renderUI({
    if (!isTRUE(input$compare_on))
      return(div(class = "stat-strip",
                 "Tick 'Compare with scenario prices' in the sidebar and click Run optimizer."))
    r1 <- result(); r2 <- result_s2()
    if (is.null(r1)) return(div(class = "status-error", "Scenario 1 failed."))
    if (is.null(r2)) return(div(class = "status-error", "Scenario 2 failed."))
    pm <- identical(r1$sampled_plot_ids, r2$sampled_plot_ids)
    div(class = if (pm) "status-done" else "status-error",
        if (pm) "Comparison complete. Same plots sampled in both scenarios."
        else "WARNING: sampled plots differ between scenarios.")
  })

  output$tbl_cmp_overview <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    s2_lbl <- sprintf("scenario (%d cells changed)", r2$n_cells_changed %||% NA_integer_)
    mk <- function(r, nm, src) {
      v <- sum(r$selected_logs_all$value_nok, na.rm = TRUE)
      m <- sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE)
      tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
      data.frame(Scenario = nm, Price_source = src,
                 Plots = r$n_plots_sampled, Trees = r$n_trees_total,
                 Trees_w_logs = r$n_trees_with_logs,
                 Value_NOK = round(v, 2), Price_volume_m3 = round(m, 4),
                 Commercial_m3_ub = round(tot$commercial_volume_m3, 4),
                 NOK_per_m3 = round(tot$nok_per_m3, 1),
                 Runtime_s = round(r$elapsed_seconds, 1),
                 stringsAsFactors = FALSE)
    }
    rbind(mk(r1, "Default", "default_2021"), mk(r2, "Scenario", s2_lbl))
  })))

  output$tbl_cmp_delta <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    req(identical(r1$sampled_plot_ids, r2$sampled_plot_ids))
    v1 <- sum(r1$selected_logs_all$value_nok, na.rm = TRUE)
    v2 <- sum(r2$selected_logs_all$value_nok, na.rm = TRUE)
    m1 <- sum(r1$selected_logs_all$price_volume_m3, na.rm = TRUE)
    m2 <- sum(r2$selected_logs_all$price_volume_m3, na.rm = TRUE)
    t1 <- r1$per_m3_summary[r1$per_m3_summary$scope == "Total", ]
    t2 <- r2$per_m3_summary[r2$per_m3_summary$scope == "Total", ]
    h1 <- t1$nok_per_m3; h2 <- t2$nok_per_m3
    c1 <- t1$commercial_volume_m3; c2 <- t2$commercial_volume_m3
    pct <- function(a, b) if (abs(b) > 1e-9) round((a - b) / abs(b) * 100, 2) else 0
    data.frame(
      Delta_value_NOK      = round(v2 - v1, 2),
      Delta_value_pct      = pct(v2, v1),
      Delta_volume_m3      = round(m2 - m1, 4),
      Delta_volume_pct     = pct(m2, m1),
      Delta_commercial_m3  = round(c2 - c1, 4),
      Delta_NOK_per_m3     = round(h2 - h1, 1),
      Delta_NOK_per_m3_pct = pct(h2, h1),
      Trees_changed        = sum(cmp_trees()$changed),
      stringsAsFactors = FALSE
    )
  })))

  output$tbl_cmp_per_m3 <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    x <- compare_per_m3(result(), result_s2())
    data.frame(Scope = x$scope, Assortment = x$assortment,
               NOK_per_m3_default  = round(x$nok_per_m3_default, 1),
               NOK_per_m3_scenario = round(x$nok_per_m3_scenario, 1),
               Delta_NOK_per_m3    = round(x$delta_nok_per_m3, 1),
               Share_pct_default   = round(100 * x$volume_share_default, 1),
               Share_pct_scenario  = round(100 * x$volume_share_scenario, 1),
               Delta_share_pp      = round(100 * x$delta_volume_share, 1),
               stringsAsFactors = FALSE)
  }), page_size = 12L))

  output$cmp_patterns <- renderUI({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    x <- cmp_trees()
    tagList(
      div(class = "stat-strip",
          tags$strong(sprintf("%d of %d trees changed their cutting pattern",
                              sum(x$changed), nrow(x))),
          " (the change alters value or volume)."),
      if (sum(x$tie_only) > 0L)
        div(class = "note-muted",
            sprintf("%d tree(s) differ only by equal-value ties; not counted as changes.",
                    sum(x$tie_only)))
    )
  })

  output$tbl_cmp_trees <- renderReactable({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    x <- cmp_trees(); x <- x[x$changed, , drop = FALSE]
    if (nrow(x) == 0L)
      return(apter_reactable(data.frame(Message = "No tree changed its cutting pattern.")))
    x <- x[order(-abs(x$value_scenario - x$value_default)), , drop = FALSE]
    apter_reactable(data.frame(
      Plot = x$plot_id_github, Tree = x$treid, Species = x$species_group,
      Logs_default = x$n_logs_default, Logs_scenario = x$n_logs_scenario,
      Value_default_NOK = round(x$value_default, 2),
      Value_scenario_NOK = round(x$value_scenario, 2),
      Gain_at_scenario_prices = round(x$gain_at_scenario, 2),
      Gain_at_default_prices  = round(x$gain_at_default, 2),
      Max_volume_diff_m3 = round(x$max_volume_diff_m3, 4),
      stringsAsFactors = FALSE))
  })

  output$cmp_tree_ui <- renderUI({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    x <- cmp_trees()
    x <- x[order(!x$changed, -abs(x$value_scenario - x$value_default)), , drop = FALSE]
    lbl <- sprintf("%s / %s (%s%s)", x$plot_id_github, x$treid, x$species_group,
                   ifelse(x$changed, sprintf(", %+.1f NOK", x$value_scenario - x$value_default),
                          ", unchanged"))
    selectInput("cmp_tree", "Tree (changed trees first, largest value change first)",
                choices = setNames(paste(x$plot_id_github, x$treid, sep = "|"), lbl),
                width = "100%")
  })

  output$stem_compare <- renderPlot(res = 96, {
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()),
        !is.null(input$cmp_tree))
    k  <- input$cmp_tree
    tk <- function(df) paste(df$plot_id_github, df$treid, sep = "|")
    l1 <- result()$selected_logs_all;   l1 <- l1[tk(l1) == k, , drop = FALSE]
    l2 <- result_s2()$selected_logs_all; l2 <- l2[tk(l2) == k, , drop = FALSE]
    ts <- result()$tree_summary; tr <- ts[tk(ts) == k, , drop = FALSE]
    req(nrow(tr) == 1L)
    apter_stem_compare(l1, l2, tr$height_m,
                       sprintf("Tree %s (%s, dbh %.1f cm, height %.1f m)",
                               tr$treid, tr$species_group, tr$dbh_cm, tr$height_m))
  })

  output$tbl_cmp_products <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    ps1 <- r1$product_summary[, c("ProductKey", "ProductType",
                                   "total_value_nok", "total_volume_m3")]
    ps2 <- r2$product_summary[, c("ProductKey", "total_value_nok", "total_volume_m3")]
    names(ps2)[2:3] <- c("S2_val", "S2_vol")
    m <- merge(ps1, ps2, by = "ProductKey", all = TRUE)
    m[is.na(m)] <- 0
    pct <- function(a, b) ifelse(abs(b) > 1e-9, round((a - b) / abs(b) * 100, 2), 0)
    # Share of the scenario's total commercial volume under bark, per product.
    share <- function(r, keys) {
      sl  <- r$selected_logs_all
      tot <- sum(sl$commercial_volume_m3, na.rm = TRUE)
      vapply(keys, function(k)
        if (tot > 0) sum(sl$commercial_volume_m3[sl$ProductKey == k]) / tot else 0,
        numeric(1), USE.NAMES = FALSE)
    }
    data.frame(
      ProductKey   = m$ProductKey,
      ProductType  = m$ProductType,
      S1_value     = round(m$total_value_nok, 2),
      S2_value     = round(m$S2_val, 2),
      Delta_value  = round(m$S2_val - m$total_value_nok, 2),
      Delta_val_pct = pct(m$S2_val, m$total_value_nok),
      S1_volume    = round(m$total_volume_m3, 4),
      S2_volume    = round(m$S2_vol, 4),
      Delta_volume = round(m$S2_vol - m$total_volume_m3, 4),
      Delta_vol_pct = pct(m$S2_vol, m$total_volume_m3),
      S1_vol_share_pct = round(100 * share(r1, m$ProductKey), 1),
      S2_vol_share_pct = round(100 * share(r2, m$ProductKey), 1),
      stringsAsFactors = FALSE
    )
  })))

  output$bar_cmp_value <- renderPlot(res = 96, {
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    ps1 <- result()$product_summary
    ps2 <- result_s2()$product_summary
    keys <- union(ps1$ProductKey, ps2$ProductKey)
    v1 <- ps1$total_value_nok[match(keys, ps1$ProductKey)]; v1[is.na(v1)] <- 0
    v2 <- ps2$total_value_nok[match(keys, ps2$ProductKey)]; v2[is.na(v2)] <- 0
    ch  <- APTER_CHART
    old <- apter_par(); on.exit(par(old))
    m   <- rbind(S1 = v1, S2 = v2); colnames(m) <- gsub("_", "\n", keys)
    bp  <- barplot(m, beside = TRUE, col = NA, border = NA, axes = FALSE,
                   ylim = c(0, max(m, 0) * 1.15), las = 1, cex.names = 0.8,
                   padj = 0.6, space = c(0, 0.9),
                   main = "Value by product: default vs scenario", ylab = "NOK")
    abline(h = axTicks(2), col = ch$grid, lwd = 0.7)
    barplot(m, beside = TRUE, col = c(ch$series1, ch$series2),
            border = ch$surface, lwd = 2, axes = FALSE, add = TRUE,
            space = c(0, 0.9), names.arg = rep("", ncol(m)))
    axis(2, at = axTicks(2), col = NA, col.ticks = NA,
         labels = format(axTicks(2), big.mark = ",", scientific = FALSE, trim = TRUE))
    abline(h = 0, col = ch$baseline)
    legend("topright", c("Default", "Scenario"), fill = c(ch$series1, ch$series2),
           border = NA, bty = "n", text.col = ch$ink, cex = 0.85)
  })

  output$bar_cmp_delta <- renderPlot(res = 96, {
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    ps1 <- result()$product_summary
    ps2 <- result_s2()$product_summary
    keys <- union(ps1$ProductKey, ps2$ProductKey)
    v1 <- ps1$total_value_nok[match(keys, ps1$ProductKey)]; v1[is.na(v1)] <- 0
    v2 <- ps2$total_value_nok[match(keys, ps2$ProductKey)]; v2[is.na(v2)] <- 0
    delta <- setNames(v2 - v1, keys)
    # Sign is carried by bar direction from the zero baseline and the signed
    # label, so one neutral colour is used (no colour-only meaning).
    apter_barplot(delta, col = APTER_CHART$neutral,
                  main = "Value change (scenario \u2212 default) by product", ylab = "NOK",
                  fmt = function(v) {
                    s <- format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
                    ifelse(v > 0, paste0("+", s), s)
                  })
  })

  output$tbl_cmp_diag <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    pm <- identical(r1$sampled_plot_ids, r2$sampled_plot_ids)
    data.frame(
      Item  = c("Same sampled plots", "S1 price FAILs", "S2 price FAILs",
                "S1 unpriced groups", "S2 unpriced groups",
                "S1 runtime s", "S2 runtime s"),
      Value = c(as.character(pm),
                sum(r1$price_validation_checks$result == "FAIL"),
                sum(r2$price_validation_checks$result == "FAIL"),
                nrow(r1$unpriced_summary), nrow(r2$unpriced_summary),
                round(r1$elapsed_seconds, 1), round(r2$elapsed_seconds, 1)),
      stringsAsFactors = FALSE
    )
  })))

  # ---- Tab 1: Summary -------------------------------------------------------
  output$summary_cards <- renderUI({
    r <- result(); req(!is.null(r))
    total_val <- sum(r$selected_logs_all$value_nok,       na.rm = TRUE)
    total_vol <- sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE)
    pct_logs  <- if (r$n_trees_total > 0)
      round(100 * r$n_trees_with_logs / r$n_trees_total) else 0L
    tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]

    ro_field_row(
      ro_field("Plots sampled",     r$n_plots_sampled),
      ro_field("Trees (filtered)",  r$n_trees_total),
      ro_field("Trees with logs",   sprintf("%d (%d%%)", r$n_trees_with_logs, pct_logs)),
      ro_field("Selected logs",     nrow(r$selected_logs_all)),
      ro_field("NOK per m\u00b3 (commercial, under bark)",
               if (is.na(tot$nok_per_m3)) "\u2013" else
                 format(round(tot$nok_per_m3, 1), nsmall = 1)),
      ro_field("Total value (NOK)", format(round(total_val, 0), big.mark = ",",
                                           scientific = FALSE)),
      ro_field("Commercial volume u.b. (m\u00b3)", round(tot$commercial_volume_m3, 2)),
      ro_field("Price volume (m\u00b3)", round(total_vol, 2)),
      ro_field("Runtime",           sprintf("%.1f s", r$elapsed_seconds))
    )
  })

  output$summary_detail <- renderUI({
    r <- result(); req(!is.null(r))
    n_zero <- nrow(r$zero_log_trees)
    n_unpr <- nrow(r$unpriced_summary)
    tagList(
      div(class = "stat-strip",
          sprintf("Plots matching the selection: %d  |  Zero-log trees: %d  |  Unpriced-candidate groups: %d",
                  r$n_eligible_plots, n_zero, n_unpr)),
      div(class = "stat-strip", RESULTS_LABEL)
    )
  })

  # ---- Tab 2: Products ------------------------------------------------------
  output$tbl_products <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    r$product_summary
  }), digits = 3))

  output$bar_value_by_product <- renderPlot(res = 96, {
    r <- result(); req(!is.null(r))
    ps <- r$product_summary
    req(nrow(ps) > 0L)
    vals <- setNames(ps$total_value_nok, ps$ProductKey)
    apter_barplot(vals, col = APTER_CHART$series1,
                  main = "Total value (NOK) by product", ylab = "NOK",
                  fmt = function(v) format(round(v), big.mark = ",",
                                           scientific = FALSE, trim = TRUE))
  })

  output$bar_volume_by_product <- renderPlot(res = 96, {
    r <- result(); req(!is.null(r))
    ps <- r$product_summary
    req(nrow(ps) > 0L)
    vols <- setNames(ps$total_volume_m3, ps$ProductKey)
    apter_barplot(vols, col = APTER_CHART$series1,
                  main = "Total volume (m\u00b3) by product", ylab = "m\u00b3",
                  fmt = function(v) format(round(v, 2), nsmall = 2, trim = TRUE))
  })

  # ---- Per m3 tab --------------------------------------------------------------
  output$tbl_per_m3 <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    s <- r$per_m3_summary
    data.frame(Scope = s$scope, Trees = s$n_trees,
               Value_NOK = round(s$value_nok, 2),
               Commercial_volume_ub_m3 = round(s$commercial_volume_m3, 4),
               NOK_per_m3 = round(s$nok_per_m3, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$tbl_per_m3_assortment <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    a <- r$per_m3_by_assortment
    data.frame(Scope = a$scope, Assortment = a$assortment,
               Value_NOK = round(a$value_nok, 2),
               Commercial_volume_ub_m3 = round(a$commercial_volume_m3, 4),
               NOK_per_m3_contribution = round(a$nok_per_m3_contribution, 1),
               Volume_share_pct = round(100 * a$volume_share, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$bar_per_m3_contribution <- renderPlot(res = 96, {
    r <- result(); req(!is.null(r))
    a <- r$per_m3_by_assortment
    a <- a[a$scope == "Total", , drop = FALSE]
    req(nrow(a) > 0L)
    apter_barplot(setNames(a$nok_per_m3_contribution, a$assortment),
                  col = APTER_CHART$series1,
                  main = "NOK per m\u00b3 by assortment (contribution)",
                  ylab = "NOK / m\u00b3",
                  fmt = function(v) format(round(v, 1), nsmall = 1, trim = TRUE))
  })

  output$bar_volume_share <- renderPlot(res = 96, {
    r <- result(); req(!is.null(r))
    a <- r$per_m3_by_assortment
    a <- a[a$scope == "Total", , drop = FALSE]
    req(nrow(a) > 0L)
    apter_barplot(setNames(100 * a$volume_share, a$assortment),
                  col = APTER_CHART$series1,
                  main = "Share of commercial volume by assortment",
                  ylab = "%",
                  fmt = function(v) sprintf("%.1f %%", v))
  })

  # Tree detail on demand: one row per tree, its logs in the row details.
  output$tbl_trees <- renderReactable({
    r <- result(); req(!is.null(r))
    ts <- r$tree_summary
    tr <- data.frame(Plot = ts$plot_id_github, Tree = ts$treid,
                     Species = ts$species_group, DBH_cm = ts$dbh_cm,
                     Height_m = ts$height_m, Logs = ts$n_logs_selected,
                     Value_NOK = round(ts$total_value_nok, 2),
                     Commercial_volume_ub_m3 = round(ts$commercial_volume_m3, 4),
                     NOK_per_m3 = round(ts$nok_per_m3, 1),
                     Status = ts$optimizer_status,
                     check.names = FALSE, stringsAsFactors = FALSE)
    sl <- r$selected_logs_all
    log_cols <- intersect(c("ProductKey", "StartPos", "StopPos", "LogLength",
                            "TopDiameter_ob", "TopDiameter_ub",
                            "PriceDiameterClass_cm_used", "price_volume_m3",
                            "commercial_volume_m3", "PriceNOKperM3", "value_nok"),
                          names(sl))
    apter_reactable(tr, page_size = 10L, details = function(i) {
      logs <- sl[sl$plot_id_github == tr$Plot[i] & sl$treid == tr$Tree[i],
                 log_cols, drop = FALSE]
      if (nrow(logs) == 0L)
        return(div(class = "note-muted", "No selected logs for this tree."))
      div(class = "tree-logs", apter_reactable(logs, digits = 3, page_size = 20L))
    })
  })

  # ---- Tab 4: Diagnostics ---------------------------------------------------
  output$tbl_stage_timings <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    st <- r$diagnostics$stage_timings
    req(is.data.frame(st))
    # Exclude "total" row; sort by elapsed descending.
    st2 <- st[st$stage != "total", ]
    st2 <- st2[order(st2$elapsed_seconds, decreasing = TRUE), ]
    pct <- round(100 * st2$elapsed_seconds /
                   r$diagnostics$elapsed_seconds)
    data.frame(Stage      = st2$stage,
               Seconds    = round(st2$elapsed_seconds, 2),
               `Pct total`= paste0(pct, "%"),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$tbl_diag <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    diag <- r$diagnostics
    # Drop stage_timings (shown separately) and elapsed_seconds (in status bar).
    skip <- c("stage_timings", "elapsed_seconds")
    diag <- diag[!names(diag) %in% skip]
    vals <- vapply(diag, function(x) paste(x, collapse = "; "), character(1))
    data.frame(Parameter = names(vals), Value = unname(vals),
               stringsAsFactors = FALSE)
  })))

  output$tbl_price_val <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    r$price_validation_checks
  })))

  output$tbl_zero_logs <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    zl <- r$zero_log_trees
    if (nrow(zl) == 0L)
      data.frame(Message = "No zero-log trees in this sample.",
                 stringsAsFactors = FALSE)
    else zl
  })))

  # ---- Tab 5: Selected logs -------------------------------------------------
  output$selected_logs_header <- renderUI({
    r <- result(); req(!is.null(r))
    n_total <- nrow(r$selected_logs_all)
    div(class = "stat-strip",
        if (n_total == 0L) "No selected logs."
        else sprintf("Total selected logs: %d", n_total))
  })

  output$tbl_selected_logs <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    sl <- r$selected_logs_all
    if (nrow(sl) == 0L)
      return(data.frame(Message = "No selected logs.", stringsAsFactors = FALSE))
    sl
  }), digits = 3))

  # ---- Prices and assortments tab ------------------------------------------
  output$tbl_ref_products <- renderReactable(apter_reactable(local({
    APP_PRODUCTS
  })))

  output$tbl_ref_lc <- renderReactable(apter_reactable(local({
    lc <- APP_LC
    # flag rows where the length class is NOT priced in default matrix
    sup_dm <- get_supported_sawlog_lengths(APP_PRICE_LONG)$LengthClass_dm
    lc$Note <- ifelse(
      grepl("sawlog", lc$ProductKey, ignore.case = TRUE) &
        !is.na(lc$LogLength_dm) & !(lc$LogLength_dm %in% sup_dm),
      "not priced in default 2021 matrix", "")
    lc
  })))

  # ---- Price-matrix editor (R/helpers_price_matrix.R) ---------------------------
  # Two price sets: default_pm() (the 2021 default plus default-side prices
  # of custom assortments; base prices read-only) and scenario_pm(). Each
  # holds prices for the products of its own run only.
  default_pm  <- reactiveVal(APP_PM_DEFAULT)
  scenario_pm <- reactiveVal(APP_PM_DEFAULT)
  custom_defs <- reactiveVal(empty_assortment_defs())   # Phase 4b assortments
  custom_keys <- reactive(assortment_keys(custom_defs()))
  pm_tick     <- reactiveVal(0L)     # re-render the grids
  pm_msg      <- reactiveVal(NULL)   # last upload / reset / edit message
  asm_msg     <- reactiveVal(NULL)   # last assortment add / edit / upload message

  # Everything that changes a run; a run stores it to detect stale results.
  pm_state <- reactive(list(custom_defs(), default_pm(), scenario_pm()))

  # Products, length classes and prices of one run (base + custom by UseIn).
  run_inputs <- function(run) {
    p  <- products_for_run(APP_PRODUCTS, APP_LC, custom_defs(), run)
    pm <- pm_minimal(if (run == "default") default_pm() else scenario_pm())
    list(pd = p$pd, lc = p$lc, pm = pm[pm$ProductKey %in% p$pd$ProductKey, , drop = FALSE],
         custom = nrow(p$pd) > nrow(APP_PRODUCTS))
  }
  # Drop prices of products not in the run (e.g. after a flat reset).
  keep_run_keys <- function(pm, run)
    pm[pm$ProductKey %in% products_for_run(APP_PRODUCTS, APP_LC, custom_defs(), run)$pd$ProductKey,
       , drop = FALSE]
  pm_has <- function(pm, id) if (identical(id, PM_FLAT_ID)) any(is.na(pm$LengthClass_dm))
                             else id %in% pm$ProductKey[!is.na(pm$LengthClass_dm)]

  # Matrix choices follow the custom assortments (initial choices: setup.R).
  pm_ids <- reactive({
    ids <- unique(c(pm_matrix_ids(default_pm()), pm_matrix_ids(scenario_pm())))
    c(setdiff(ids, PM_FLAT_ID), PM_FLAT_ID)
  })
  observeEvent(pm_ids(), {
    ids <- pm_ids(); sel <- isolate(input$pm_id)
    updateSelectInput(session, "pm_id",
                      choices = setNames(ids, vapply(ids, pm_matrix_label, character(1))),
                      selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
  }, ignoreInit = TRUE)

  pm_widget <- function(grid, id, read_only, renderer, def = NULL, interp = NULL) {
    w <- rhandsontable(grid, rowHeaders = rownames(grid), readOnly = read_only,
                       height = 560, stretchH = "all", matrix_id = id,
                       def = def, interp = interp)
    w <- hot_cols(w, renderer = renderer, columnSorting = FALSE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
    if (!read_only) w <- hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
    w
  }

  # Default grid: base prices read-only; custom assortments' rows editable.
  output$pm_default <- renderRHandsontable({
    pm_tick(); id <- input$pm_id; req(id)
    dpm <- isolate(default_pm()); req(pm_has(dpm, id))
    g  <- pm_to_grid(dpm, id)
    interp <- if (identical(id, PM_FLAT_ID))
      matrix(FALSE, nrow(g), ncol(g)) else
      outer(rownames(g), colnames(g), function(d, l)
        paste(id, l, d) %in% APP_PM_INTERP_KEYS)
    custom <- if (identical(id, PM_FLAT_ID)) rownames(g) %in% custom_keys()
              else rep(id %in% custom_keys(), nrow(g))
    w <- pm_widget(g, id, !any(custom), PM_INTERP_RENDERER, interp = unname(interp))
    if (any(custom)) for (i in which(!custom)) w <- hot_row(w, i, readOnly = TRUE)
    w
  })
  output$pm_default_note <- renderUI({
    id <- input$pm_id; req(id)
    if (!pm_has(default_pm(), id))
      div(class = "note-muted", "Not used in the default run.")
    else if (id %in% custom_keys() || (identical(id, PM_FLAT_ID) &&
                                       any(custom_keys() %in% default_pm()$ProductKey)))
      div(class = "note-muted", "Custom assortment prices are editable here.")
  })

  output$pm_scenario <- renderRHandsontable({
    pm_tick(); id <- input$pm_id; req(id)
    spm <- isolate(scenario_pm()); req(pm_has(spm, id))
    pm_widget(pm_to_grid(spm, id), id, FALSE, PM_DIFF_RENDERER,
              def = unname(pm_default_aligned(default_pm(), spm, id)))
  })
  output$pm_scenario_note <- renderUI({
    id <- input$pm_id; req(id)
    if (!pm_has(scenario_pm(), id)) div(class = "note-muted", "Not used in the scenario run.")
  })

  # Edits in a grid. The matrix id travels with the widget, so a stale grid
  # from the previously shown matrix is never written elsewhere. In the
  # default grid only custom assortments' prices change.
  apply_grid_edit <- function(x, pmv, only_keys = NULL) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- x$params$matrix_id %||% isolate(input$pm_id)
    g  <- hot_to_r(x)
    ref <- pm_to_grid(isolate(pmv()), id)
    if (!identical(dim(g), dim(ref))) return()
    rownames(g) <- rownames(ref); colnames(g) <- colnames(ref)
    if (any(as.matrix(g) < 0, na.rm = TRUE)) {
      pm_msg(list(type = "error", text = "Negative prices are not allowed; the edit was undone."))
      pm_tick(pm_tick() + 1L)
      return()
    }
    if (!is.null(only_keys) && identical(id, PM_FLAT_ID)) {
      fixed <- !rownames(g) %in% only_keys
      g[fixed, ] <- ref[fixed, ]
    } else if (!is.null(only_keys) && !id %in% only_keys) return()
    pmv(pm_from_grid(g, id, isolate(pmv())))
  }
  observeEvent(input$pm_scenario, apply_grid_edit(input$pm_scenario, scenario_pm))
  observeEvent(input$pm_default,
               apply_grid_edit(input$pm_default, default_pm, only_keys = isolate(custom_keys())))

  observeEvent(input$pm_reset, {
    id <- input$pm_id
    if (!pm_has(default_pm(), id) || !pm_has(scenario_pm(), id)) {
      pm_msg(list(type = "error", text = sprintf(
        "%s is priced in one run only; there is no default to reset to.", pm_matrix_label(id))))
      return()
    }
    scenario_pm(keep_run_keys(pm_reset(scenario_pm(), default_pm(), id), "scenario"))
    pm_tick(pm_tick() + 1L)
    pm_msg(list(type = "ok", text = sprintf("%s reset to the default.", pm_matrix_label(id))))
  })

  observeEvent(input$pm_upload, {
    f <- input$pm_upload; req(f)
    p <- products_for_run(APP_PRODUCTS, APP_LC, custom_defs(), "scenario")
    res <- apply_price_upload(f$datapath, f$name, scenario_pm(), default_pm(), p$pd, p$lc)
    if (isTRUE(res$ok)) {
      scenario_pm(res$pm)
      pm_tick(pm_tick() + 1L)
      pm_msg(list(type = "ok", warnings = res$warnings, text = sprintf(
        "Loaded %s (%s layout): replaced %s.", f$name, res$layout,
        paste(vapply(res$ids, pm_matrix_label, character(1)), collapse = ", "))))
    } else {
      pm_msg(list(type = "error", text = paste(
        c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
    }
  })

  output$pm_status <- renderUI({
    req(input$pm_id)
    n_all <- pm_n_diff(default_pm(), scenario_pm())
    n_now <- if (pm_has(default_pm(), input$pm_id) && pm_has(scenario_pm(), input$pm_id))
      sum(pm_diff_mask(default_pm(), scenario_pm(), input$pm_id)) else 0L
    div(class = "pm-status",
        tags$strong(sprintf("%d cell(s) differ from the default", n_all)),
        sprintf(" (%d in this matrix).", n_now))
  })

  msg_ui <- function(m) {
    if (is.null(m)) return(NULL)
    tagList(
      div(class = if (identical(m$type, "error")) "pm-msg-error" else "pm-msg-ok", m$text),
      if (length(m$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", m$warnings, collapse = "\n")))
  }
  output$pm_upload_msg <- renderUI(msg_ui(pm_msg()))

  # Templates hold the scenario run's products, filled with default prices
  # where there are any.
  template_pm <- reactive({
    keys <- products_for_run(APP_PRODUCTS, APP_LC, custom_defs(), "scenario")$pd$ProductKey
    d <- pm_minimal(default_pm()); s <- pm_minimal(scenario_pm())
    d <- d[d$ProductKey %in% keys, , drop = FALSE]
    pm_minimal(rbind(d, s[s$ProductKey %in% setdiff(keys, d$ProductKey), , drop = FALSE]))
  })
  output$dl_template_wide <- downloadHandler(
    filename = function() "apter_price_template_wide.xlsx",
    content  = function(file) pm_template_wide(template_pm(), file))
  output$dl_template_long <- downloadHandler(
    filename = function() "apter_price_template_long.csv",
    content  = function(file) pm_template_long(template_pm(), file))
  output$dl_scenario <- downloadHandler(
    filename = function() "apter_scenario_prices.csv",
    content  = function(file) pm_export(scenario_pm(), file))

  output$scenario_sidebar <- renderUI({
    n <- pm_n_diff(default_pm(), scenario_pm())
    k <- nrow(custom_defs())
    div(class = "note-muted",
        sprintf("Scenario: %d price cell(s) differ from the default.", n),
        if (k > 0L) tagList(br(), sprintf("%d custom assortment(s).", k)), br(),
        actionLink("go_prices", "Edit scenario prices"))
  })
  observeEvent(input$go_prices, nav_select("main_nav", "Prices and assortments"))


  # ---- Custom assortments (Phase 4b) ------------------------------------------
  USE_IN_LABELS <- c(both = "Default and scenario", default = "Default only",
                     scenario = "Scenario only")

  output$tbl_assortments <- renderReactable(apter_reactable(local({
    d <- custom_defs()
    if (nrow(d) == 0L)
      return(data.frame(Message = "No custom assortments. Add one, or upload an assortments file.",
                        stringsAsFactors = FALSE))
    rng <- function(lo, hi) sprintf("%s–%s", ifelse(is.na(lo), 0, lo),
                                    ifelse(is.finite(hi), hi, "∞"))
    data.frame(Name = d$Name, Class = d$Class, Species = d$Species,
               `Top (cm)` = rng(d$MinTopDiameter_cm, d$MaxTopDiameter_cm),
               `Butt (cm)` = rng(d$MinButtDiameter_cm, d$MaxButtDiameter_cm),
               `Lengths (cm)` = d$AllowedLengths_cm, Priority = d$Priority,
               `Under bark` = d$DiameterUnderBark,
               `Dt below cut (cm)` = d$DiameterTopPosition_cm,
               `Volume method` = d$VolumeDiameterCategory,
               Rounding = d$VolumeDiameterAdjustment, Pricing = d$PriceType,
               `Used in` = unname(USE_IN_LABELS[d$UseIn]),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  observeEvent(custom_defs(), {
    n <- custom_defs()$Name
    updateSelectInput(session, "asm_pick", choices = n,
                      selected = if (isTRUE(isolate(input$asm_pick) %in% n)) isolate(input$asm_pick) else n[1L])
  })

  asm_editing  <- reactiveVal(NULL)   # Name of the assortment being edited; NULL = new
  asm_form_err <- reactiveVal(NULL)
  output$asm_form_msg <- renderUI({
    e <- asm_form_err(); if (is.null(e)) return(NULL)
    div(class = "pm-msg-error", paste(e, collapse = "\n"))
  })

  asm_modal <- function(d = NULL) {
    v <- function(n, default) {
      x <- if (is.null(d)) NULL else d[[n]]
      if (is.null(x) || length(x) == 0L || is.na(x)) default else x
    }
    fin <- function(n) { x <- v(n, NA); if (is.finite(x)) x else NA }
    modalDialog(
      title = if (is.null(d)) "Add assortment" else paste("Edit", d$Name),
      size = "l", easyClose = FALSE,
      layout_columns(col_widths = c(6, 6),
        textInput("asm_name", "Name", v("Name", "")),
        textInput("asm_class", "Assortment class (groups species in the Total results)",
                  v("Class", "")),
        selectInput("asm_species", "Species",
                    c("Spruce and pine" = "both", "Spruce" = "spruce", "Pine" = "pine"),
                    v("Species", "both")),
        selectInput("asm_use_in", "Used in", setNames(names(USE_IN_LABELS), USE_IN_LABELS),
                    v("UseIn", "scenario")),
        numericInput("asm_min_top", "Min top diameter (cm)", v("MinTopDiameter_cm", 0), min = 0),
        numericInput("asm_max_top", "Max top diameter (cm; empty = none)", fin("MaxTopDiameter_cm"), min = 0),
        numericInput("asm_min_butt", "Min butt diameter (cm)", v("MinButtDiameter_cm", 0), min = 0),
        numericInput("asm_max_butt", "Max butt diameter (cm; empty = none)", fin("MaxButtDiameter_cm"), min = 0),
        textInput("asm_lengths", "Allowed lengths (cm, e.g. 400;430;460)", v("AllowedLengths_cm", "")),
        numericInput("asm_priority", "Priority in ties (1 = highest; sawlog 1, pulpwood 2)",
                     v("Priority", 1)),
        selectInput("asm_method", "Price-volume method", VOLUME_METHODS,
                    v("VolumeDiameterCategory", "Top cylinder")),
        selectInput("asm_adjust", "Top diameter for volume",
                    c("As measured" = "None", "Rounded down to whole cm" = VOLUME_ROUND_DOWN_CM),
                    v("VolumeDiameterAdjustment", "None")),
        numericInput("asm_dtp", "Top diameter measured (cm below the cut; Norwegian rule: 10)",
                     v("DiameterTopPosition_cm", 0), min = 0, step = 10),
        checkboxInput("asm_ub", "Diameters under bark", v("DiameterUnderBark", TRUE)),
        radioButtons("asm_price_type", "Pricing",
                     c("Grid (length × diameter class)" = "matrix", "Flat price" = "flat"),
                     v("PriceType", "matrix")),
        numericInput("asm_price", "Starting price for new cells (NOK/m³)", 500, min = 0),
        numericInput("asm_max_class",
                     "Largest diameter class (cm; grid without a max top diameter)", NA, min = 1)
      ),
      div(class = "note-muted",
          if (is.null(d)) "A new assortment starts at the starting price in every cell; edit the prices in the price matrices."
          else paste("Saving keeps the default and scenario prices of every cell that still exists",
                     "(same species, diameter class and length; a flat price stays if pricing stays flat).",
                     "New cells get the starting price; cells no longer in the grid are dropped."),
          " The largest diameter class of a grid is its price cap: larger logs are priced at it."),
      uiOutput("asm_form_msg"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("asm_save", "Save", class = "btn-primary")))
  }

  observeEvent(input$asm_add, {
    asm_editing(NULL); asm_form_err(NULL); showModal(asm_modal())
  })
  observeEvent(input$asm_edit, {
    d <- custom_defs(); i <- match(input$asm_pick, d$Name); req(!is.na(i))
    asm_editing(d$Name[i]); asm_form_err(NULL); showModal(asm_modal(d[i, , drop = FALSE]))
  })

  # Replace the prices of `drop` keys; add `new` rows where the run uses them.
  replace_prices <- function(pm, drop, new, use_in, run) {
    m <- pm_minimal(pm); m <- m[!m$ProductKey %in% drop, , drop = FALSE]
    if (!is.null(new) && use_in %in% c("both", run)) m <- pm_minimal(rbind(m, new))
    m
  }

  observeEvent(input$asm_save, {
    num <- function(x) if (is.null(x) || length(x) == 0L || is.na(x)) NA_real_ else as.numeric(x)
    or  <- function(x, y) if (is.na(x)) y else x
    na_if_blank <- function(x) { x <- trimws(x %||% ""); if (nzchar(x)) x else NA }
    name <- trimws(input$asm_name %||% "")
    def <- .type_assortment_defs(data.frame(
      Name = name, Class = or(na_if_blank(input$asm_class), assortment_slug(name)),
      Species = input$asm_species,
      MinTopDiameter_cm = or(num(input$asm_min_top), 0),
      MaxTopDiameter_cm = or(num(input$asm_max_top), Inf),
      MinButtDiameter_cm = or(num(input$asm_min_butt), 0),
      MaxButtDiameter_cm = or(num(input$asm_max_butt), Inf),
      AllowedLengths_cm = input$asm_lengths, Priority = num(input$asm_priority),
      DiameterUnderBark = isTRUE(input$asm_ub),
      DiameterTopPosition_cm = or(num(input$asm_dtp), 0),
      VolumeDiameterCategory = input$asm_method,
      VolumeDiameterAdjustment = input$asm_adjust, PriceType = input$asm_price_type,
      UseIn = input$asm_use_in, stringsAsFactors = FALSE))
    old  <- custom_defs()
    prev <- if (is.null(asm_editing())) old[0, , drop = FALSE]
            else old[old$Name == asm_editing(), , drop = FALSE]
    defs <- rbind(old[!old$Name %in% prev$Name, , drop = FALSE], def)
    v <- validate_assortment_defs(defs, APP_PRODUCTS, APP_LC)
    price <- num(input$asm_price)
    errs <- c(v$errors, if (is.na(price) || price < 0) "Give a starting price of 0 NOK/m³ or more.")
    new <- if (length(errs) == 0L)
      tryCatch(pm_new_assortment_prices(def, price, num(input$asm_max_class)),
               error = function(e) conditionMessage(e))
    if (is.character(new)) errs <- c(errs, new)
    if (length(errs) > 0L) { asm_form_err(errs); return() }

    # An edit keeps the prices of cells that still exist, per run; new cells
    # get the starting price and removed cells are dropped.
    drop <- union(assortment_keys(prev), unique(new$ProductKey))
    keep <- function(pm) if (nrow(prev) == 0L) new
                         else pm_keep_assortment_prices(new, pm, prev, def)
    new_d <- keep(default_pm()); new_s <- keep(scenario_pm())
    default_pm(replace_prices(default_pm(), drop, new_d, def$UseIn, "default"))
    scenario_pm(replace_prices(scenario_pm(), drop, new_s, def$UseIn, "scenario"))
    custom_defs(defs)
    pm_tick(pm_tick() + 1L)
    removeModal()
    runs <- intersect(c("default", "scenario"),
                      if (def$UseIn == "both") c("default", "scenario") else def$UseIn)
    kept <- vapply(runs, function(r) attr(if (r == "default") new_d else new_s, "n_kept") %||% 0L,
                   integer(1))
    asm_msg(list(type = "ok", warnings = v$warnings, text = if (nrow(prev) == 0L)
      sprintf("Added %s (%s). Its prices are in the matrix list below.",
              name, USE_IN_LABELS[[def$UseIn]])
      else sprintf(
        "Saved %s (%s): of %d price cell(s), %s; new cells at the starting price.",
        name, USE_IN_LABELS[[def$UseIn]], nrow(new),
        paste(sprintf("%d kept in the %s", kept, runs), collapse = ", "))))
  })

  observeEvent(input$asm_remove, {
    d <- custom_defs(); i <- match(input$asm_pick, d$Name); req(!is.na(i))
    keys <- assortment_keys(d[i, , drop = FALSE])
    default_pm(replace_prices(default_pm(), keys, NULL, "both", "default"))
    scenario_pm(replace_prices(scenario_pm(), keys, NULL, "both", "scenario"))
    custom_defs(d[-i, , drop = FALSE])
    pm_tick(pm_tick() + 1L)
    asm_msg(list(type = "ok", text = sprintf("Removed %s.", d$Name[i])))
  })

  output$dl_assortments <- downloadHandler(
    filename = function() "apter_assortments.xlsx",
    content  = function(file) assortments_export(custom_defs(), default_pm(), scenario_pm(), file))

  observeEvent(input$asm_upload, {
    f <- input$asm_upload; req(f)
    res <- assortments_import(f$datapath, APP_PRODUCTS, APP_LC, default_pm(), scenario_pm())
    if (isTRUE(res$ok)) {
      custom_defs(res$defs); default_pm(res$default_pm); scenario_pm(res$scenario_pm)
      pm_tick(pm_tick() + 1L)
      asm_msg(list(type = "ok", warnings = res$warnings, text = sprintf(
        "Loaded %s: %d custom assortment(s) with their default and scenario prices.",
        f$name, nrow(res$defs))))
    } else {
      asm_msg(list(type = "error", text = paste(
        c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
    }
  })
  output$asm_msg <- renderUI(msg_ui(asm_msg()))

}  # end server
