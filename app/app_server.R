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

  # Stratum availability preview
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
        span(class = "note-ok", "Stratum OK.")
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
    div(class = "status-tiles",
        status_tile(r$run_id, "Done",
                    c(sprintf("%s \u00b7 %d plot(s) \u00b7 %d trees",
                              r$params$species_group, r$n_plots_sampled,
                              r$n_trees_total),
                      sprintf("Total value %s NOK \u00b7 %d selected logs",
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

    withProgress(message = "Preparing inputs...", value = 0, {
      incProgress(0.10, detail = "Loading price matrix and products")
      r <- tryCatch({
        incProgress(0.15, detail = "Sampling plots and loading taper profiles")
        Sys.sleep(0)  # yield to allow UI refresh
        incProgress(0.20, detail = "Running bucking optimizer...")
        res <- run_sampled_plot_bucking(
          params            = params,
          price_matrix_path = "default_2021",
          root              = ROOT
        )
        incProgress(0.45, detail = "Summarising results")
        Sys.sleep(0)
        incProgress(0.10, detail = "Rendering outputs")
        res$started_at <- started_at
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

  result_s2 <- reactive({
    req(isTRUE(input$compare_on), !is.null(result()))
    params  <- isolate(build_run_params(input))
    s2_mode <- isolate(input$s2_mode)
    s2_pm <- resolve_s2_price_path(
      mode    = s2_mode,
      upload  = isolate(input$s2_file),
      mults   = c(saw  = as.numeric(isolate(input$mult_saw)),
                  pulp = as.numeric(isolate(input$mult_pulp)),
                  wst  = as.numeric(isolate(input$mult_wst))),
      base_pm = APP_PRICE_LONG
    )
    # The multiplier matrix is a tempfile; remove it once Scenario 2 has run.
    if (identical(s2_mode, "multiplier"))
      on.exit(tryCatch(file.remove(s2_pm), error = function(e) NULL), add = TRUE)
    withProgress(message = "Running Scenario 2...", value = 0.5, {
      tryCatch(
        run_sampled_plot_bucking(params = params, price_matrix_path = s2_pm,
                                 root = ROOT),
        error = function(e) {
          showNotification(paste0("Scenario 2 error: ", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        }
      )
    })
  })

  output$cmp_status <- renderUI({
    if (!isTRUE(input$compare_on))
      return(div(class = "stat-strip",
                 "Enable 'Run scenario comparison' in the sidebar and click Run optimizer."))
    r1 <- result(); r2 <- result_s2()
    if (is.null(r1)) return(div(class = "status-error", "Scenario 1 failed."))
    if (is.null(r2)) return(div(class = "status-error", "Scenario 2 failed."))
    pm <- identical(sort(as.character(r1$plot_summary$plot_id_github)),
                    sort(as.character(r2$plot_summary$plot_id_github)))
    div(class = if (pm) "status-done" else "status-error",
        if (pm) "Comparison complete. Same plots sampled in both scenarios."
        else "WARNING: sampled plots differ between scenarios.")
  })

  output$tbl_cmp_overview <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    s2_lbl <- switch(isolate(input$s2_mode),
      "same" = "default_2021 (same)", "multiplier" = "multipliers applied",
      "upload" = "uploaded matrix", "default_2021")
    mk <- function(r, nm, src) {
      v <- sum(r$selected_logs_all$value_nok, na.rm = TRUE)
      m <- sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE)
      h <- mean(r$plot_summary$value_per_ha_nok, na.rm = TRUE)
      data.frame(Scenario = nm, Price_source = src,
                 Plots = r$n_plots_sampled, Trees = r$n_trees_total,
                 Trees_w_logs = r$n_trees_with_logs,
                 Value_NOK = round(v, 2), Volume_m3 = round(m, 4),
                 Mean_val_ha = round(h, 0), Runtime_s = round(r$elapsed_seconds, 1),
                 stringsAsFactors = FALSE)
    }
    rbind(mk(r1, "Scenario 1", "default_2021"), mk(r2, "Scenario 2", s2_lbl))
  })))

  output$tbl_cmp_delta <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    req(identical(sort(as.character(r1$plot_summary$plot_id_github)),
                  sort(as.character(r2$plot_summary$plot_id_github))))
    v1 <- sum(r1$selected_logs_all$value_nok, na.rm = TRUE)
    v2 <- sum(r2$selected_logs_all$value_nok, na.rm = TRUE)
    m1 <- sum(r1$selected_logs_all$price_volume_m3, na.rm = TRUE)
    m2 <- sum(r2$selected_logs_all$price_volume_m3, na.rm = TRUE)
    h1 <- mean(r1$plot_summary$value_per_ha_nok, na.rm = TRUE)
    h2 <- mean(r2$plot_summary$value_per_ha_nok, na.rm = TRUE)
    pct <- function(a, b) if (abs(b) > 1e-9) round((a - b) / abs(b) * 100, 2) else 0
    data.frame(
      Delta_value_NOK      = round(v2 - v1, 2),
      Delta_value_pct      = pct(v2, v1),
      Delta_volume_m3      = round(m2 - m1, 4),
      Delta_volume_pct     = pct(m2, m1),
      Delta_mean_val_ha    = round(h2 - h1, 0),
      Delta_mean_val_ha_pct = pct(h2, h1),
      stringsAsFactors = FALSE
    )
  })))

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
                   main = "Value by product: S1 vs S2", ylab = "NOK")
    abline(h = axTicks(2), col = ch$grid, lwd = 0.7)
    barplot(m, beside = TRUE, col = c(ch$series1, ch$series2),
            border = ch$surface, lwd = 2, axes = FALSE, add = TRUE,
            space = c(0, 0.9), names.arg = rep("", ncol(m)))
    axis(2, at = axTicks(2), col = NA, col.ticks = NA,
         labels = format(axTicks(2), big.mark = ",", scientific = FALSE, trim = TRUE))
    abline(h = 0, col = ch$baseline)
    legend("topright", c("S1", "S2"), fill = c(ch$series1, ch$series2),
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
                  main = "Delta value (S2 - S1) by product", ylab = "NOK",
                  fmt = function(v) {
                    s <- format(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
                    ifelse(v > 0, paste0("+", s), s)
                  })
  })

  output$tbl_cmp_diag <- renderReactable(apter_reactable(local({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    r1 <- result(); r2 <- result_s2()
    pm <- identical(sort(as.character(r1$plot_summary$plot_id_github)),
                    sort(as.character(r2$plot_summary$plot_id_github)))
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

    ro_field_row(
      ro_field("Plots sampled",     r$n_plots_sampled),
      ro_field("Trees (filtered)",  r$n_trees_total),
      ro_field("Trees with logs",   sprintf("%d (%d%%)", r$n_trees_with_logs, pct_logs)),
      ro_field("Selected logs",     nrow(r$selected_logs_all)),
      ro_field("Total value (NOK)", format(round(total_val, 0), big.mark = ",",
                                           scientific = FALSE)),
      ro_field("Total volume (m\u00b3)", round(total_vol, 2)),
      ro_field("Runtime",           sprintf("%.1f s", r$elapsed_seconds))
    )
  })

  output$summary_detail <- renderUI({
    r <- result(); req(!is.null(r))
    n_zero <- nrow(r$zero_log_trees)
    n_unpr <- nrow(r$unpriced_summary)
    tagList(
      div(class = "stat-strip",
          sprintf("Eligible plots in stratum: %d  |  Zero-log trees: %d  |  Unpriced-candidate groups: %d",
                  r$n_eligible_plots, n_zero, n_unpr)),
      div(class = "stat-strip", ESTIMATE_LABEL)
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

  # ---- Tab 3: Plots ---------------------------------------------------------
  output$plot_stats_text <- renderUI({
    r <- result(); req(!is.null(r))
    ps   <- r$plot_summary
    vals <- ps$value_per_ha_nok[!is.na(ps$value_per_ha_nok)]
    if (length(vals) == 0L) return(NULL)
    div(class = "stat-strip",
        sprintf(
          "Value per ha (NOK):  min = %s  |  mean = %s  |  median = %s  |  max = %s",
          format(round(min(vals)),    big.mark = ","),
          format(round(mean(vals)),   big.mark = ","),
          format(round(median(vals)), big.mark = ","),
          format(round(max(vals)),    big.mark = ",")))
  })

  output$tbl_plots <- renderReactable(apter_reactable(local({
    r <- result(); req(!is.null(r))
    r$plot_summary
  }), digits = 2))

  output$hist_value_ha <- renderPlot(res = 96, {
    r <- result(); req(!is.null(r))
    ps   <- r$plot_summary
    vals <- ps$value_per_ha_nok[!is.na(ps$value_per_ha_nok)]
    req(length(vals) > 0L)
    ch  <- APTER_CHART
    old <- apter_par(mar = c(4.5, 5, 3, 1)); on.exit(par(old))
    h <- hist(vals, breaks = max(3L, min(15L, length(vals))), plot = FALSE)
    plot(h, col = NA, border = NA, axes = FALSE,
         main = "Value per ha (NOK) \u2014 sampled plots",
         xlab = "NOK / ha", ylab = "Number of plots")
    abline(h = axTicks(2), col = ch$grid, lwd = 0.7)
    plot(h, col = ch$series1, border = ch$surface, add = TRUE)
    axis(1, col = NA, col.ticks = NA); axis(2, col = NA, col.ticks = NA)
    abline(h = 0, col = ch$baseline)
    abline(v = mean(vals), col = ch$ink, lwd = 1.5, lty = 2)
    legend("topright",
           legend = sprintf("mean = %s", format(round(mean(vals)), big.mark = ",")),
           col = ch$ink, lty = 2, lwd = 1.5, bty = "n", cex = 0.85,
           text.col = ch$ink)
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

  output$price_matrix_header <- renderUI({
    div(class = "stat-strip",
        sprintf("Total rows: %d.", nrow(APP_PRICE_LONG)))
  })

  output$tbl_ref_price <- renderReactable(apter_reactable(local({
    pm <- APP_PRICE_LONG
    keep <- intersect(c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm",
                        "PriceNOKperM3", "PriceScenario", "PriceType",
                        "PriceSource", "Notes"),
                      names(pm))
    pm[, keep, drop = FALSE]
  }), digits = 2))

}  # end server
