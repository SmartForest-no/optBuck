# app/app_server.R
#
# Purpose : Shiny server function (`server`). Sourced by app.R after setup.R
#           and the app helpers (helpers_validation.R, helpers_upload.R).
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
      return(div(style = "color:#cc0000;",
                 "Select at least one maturity class and one bonitet."))
    n_elig <- count_eligible_plots(plots_prod, hkl_raw, abon, fyl)
    div(
      sprintf("%d eligible plot(s) available.", n_elig), br(),
      if (n_elig == 0L)
        span(style = "color:#cc0000;", "No matching plots \u2014 adjust selection.")
      else if (n_elig < as.integer(input$n_plots))
        span(style = "color:#e07000;",
             sprintf("Fewer than %s plots exist; all will be used.", input$n_plots))
      else
        span(style = "color:#28a745;", "Stratum OK.")
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

  # Backend call with multi-stage progress indicator
  result <- eventReactive(input$run_btn, {
    req(length(input$hkl_group) > 0L, length(input$abonitet) > 0L)

    # Map user-facing inputs (HKL group, county) -> backend params
    params <- build_run_params(input)

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

  output$tbl_cmp_overview <- renderTable({
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
  }, striped = TRUE, hover = TRUE)

  output$tbl_cmp_delta <- renderTable({
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
  }, striped = TRUE, hover = TRUE)

  output$tbl_cmp_products <- renderTable({
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
  }, striped = TRUE, hover = TRUE)

  output$bar_cmp_value <- renderPlot({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    ps1 <- result()$product_summary
    ps2 <- result_s2()$product_summary
    keys <- union(ps1$ProductKey, ps2$ProductKey)
    v1 <- ps1$total_value_nok[match(keys, ps1$ProductKey)]; v1[is.na(v1)] <- 0
    v2 <- ps2$total_value_nok[match(keys, ps2$ProductKey)]; v2[is.na(v2)] <- 0
    par(mar = c(5, 5, 3, 1))
    barplot(rbind(S1 = v1, S2 = v2), beside = TRUE,
            col = c("#2c7be5", "#e07000"), border = NA,
            main = "Value by product: S1 vs S2", ylab = "NOK", las = 2, cex.names = 0.8)
    legend("topright", c("S1", "S2"), fill = c("#2c7be5", "#e07000"), bty = "n")
  })

  output$bar_cmp_delta <- renderPlot({
    req(isTRUE(input$compare_on), !is.null(result()), !is.null(result_s2()))
    ps1 <- result()$product_summary
    ps2 <- result_s2()$product_summary
    keys <- union(ps1$ProductKey, ps2$ProductKey)
    v1 <- ps1$total_value_nok[match(keys, ps1$ProductKey)]; v1[is.na(v1)] <- 0
    v2 <- ps2$total_value_nok[match(keys, ps2$ProductKey)]; v2[is.na(v2)] <- 0
    delta <- setNames(v2 - v1, keys)
    par(mar = c(5, 5, 3, 1))
    barplot(delta, col = ifelse(delta >= 0, "#28a745", "#dc3545"), border = NA,
            main = "Delta value (S2 - S1) by product", ylab = "NOK",
            las = 2, cex.names = 0.8)
    abline(h = 0, col = "#555", lty = 2)
  })

  output$tbl_cmp_diag <- renderTable({
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
  }, striped = TRUE, hover = TRUE)

  # ---- Tab 1: Summary -------------------------------------------------------
  output$summary_cards <- renderUI({
    r <- result(); req(!is.null(r))
    total_val <- sum(r$selected_logs_all$value_nok,       na.rm = TRUE)
    total_vol <- sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE)
    pct_logs  <- if (r$n_trees_total > 0)
      round(100 * r$n_trees_with_logs / r$n_trees_total) else 0L

    card <- function(num, lbl, cls = "") {
      div(class = paste("vcard", cls),
          div(class = "vnum", num),
          div(class = "vlbl", lbl))
    }
    tagList(
      div(class = "card-row",
          card(r$n_plots_sampled,          "Plots sampled"),
          card(r$n_trees_total,            "Trees (filtered)"),
          card(sprintf("%d (%d%%)", r$n_trees_with_logs, pct_logs),
               "Trees with logs", "green"),
          card(nrow(r$selected_logs_all),  "Selected logs",   "green"),
          card(format(round(total_val, 0), big.mark = ",", scientific = FALSE),
               "Total value (NOK)"),
          card(round(total_vol, 2),        "Total volume (m\u00b3)"),
          card(sprintf("%.1f s", r$elapsed_seconds), "Runtime", "orange")
      )
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
      div(class = "stat-strip", style = "color:#555;", ESTIMATE_LABEL)
    )
  })

  # ---- Tab 2: Products ------------------------------------------------------
  output$tbl_products <- renderTable({
    r <- result(); req(!is.null(r))
    r$product_summary
  }, striped = TRUE, hover = TRUE, digits = 3)

  output$bar_value_by_product <- renderPlot({
    r <- result(); req(!is.null(r))
    ps <- r$product_summary
    req(nrow(ps) > 0L)
    vals <- setNames(ps$total_value_nok, ps$ProductKey)
    cols <- c("#4a9a6e", "#2c7be5", "#e07000", "#8b5cf6")[seq_along(vals)]
    par(mar = c(4, 5, 3, 1))
    bp <- barplot(vals, col = cols, border = NA,
                  main = "Total value (NOK) by product",
                  ylab = "NOK", las = 2, cex.names = 0.8)
    text(bp, vals / 2,
         labels = format(round(vals), big.mark = ",", scientific = FALSE),
         col = "white", cex = 0.8, font = 2)
  })

  output$bar_volume_by_product <- renderPlot({
    r <- result(); req(!is.null(r))
    ps <- r$product_summary
    req(nrow(ps) > 0L)
    vols <- setNames(ps$total_volume_m3, ps$ProductKey)
    cols <- c("#4a9a6e", "#2c7be5", "#e07000", "#8b5cf6")[seq_along(vols)]
    par(mar = c(4, 5, 3, 1))
    bp <- barplot(vols, col = cols, border = NA,
                  main = "Total volume (m\u00b3) by product",
                  ylab = "m\u00b3", las = 2, cex.names = 0.8)
    text(bp, vols / 2, labels = round(vols, 2),
         col = "white", cex = 0.8, font = 2)
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

  output$tbl_plots <- renderTable({
    r <- result(); req(!is.null(r))
    r$plot_summary
  }, striped = TRUE, hover = TRUE, digits = 2)

  output$hist_value_ha <- renderPlot({
    r <- result(); req(!is.null(r))
    ps   <- r$plot_summary
    vals <- ps$value_per_ha_nok[!is.na(ps$value_per_ha_nok)]
    req(length(vals) > 0L)
    old_mar <- par(mar = c(4.5, 4.5, 3, 1))
    on.exit(par(old_mar))
    hist(vals, col = "#4a9a6e", border = "white",
         breaks = max(3L, min(15L, length(vals))),
         main = "Value per ha (NOK) ??? sampled plots",
         xlab = "NOK / ha", ylab = "Number of plots")
    abline(v = mean(vals), col = "#e05a2b", lwd = 2, lty = 2)
    legend("topright",
           legend = sprintf("mean = %s", format(round(mean(vals)), big.mark = ",")),
           col = "#e05a2b", lty = 2, lwd = 2, bty = "n", cex = 0.85)
  })

  # ---- Tab 4: Diagnostics ---------------------------------------------------
  output$tbl_stage_timings <- renderTable({
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
  }, striped = TRUE, hover = TRUE)

  output$tbl_diag <- renderTable({
    r <- result(); req(!is.null(r))
    diag <- r$diagnostics
    # Drop stage_timings (shown separately) and elapsed_seconds (in status bar).
    skip <- c("stage_timings", "elapsed_seconds")
    diag <- diag[!names(diag) %in% skip]
    vals <- vapply(diag, function(x) paste(x, collapse = "; "), character(1))
    data.frame(Parameter = names(vals), Value = unname(vals),
               stringsAsFactors = FALSE)
  }, striped = TRUE, hover = TRUE)

  output$tbl_price_val <- renderTable({
    r <- result(); req(!is.null(r))
    r$price_validation_checks
  }, striped = TRUE, hover = TRUE)

  output$tbl_zero_logs <- renderTable({
    r <- result(); req(!is.null(r))
    zl <- r$zero_log_trees
    if (nrow(zl) == 0L)
      data.frame(Message = "No zero-log trees in this sample.",
                 stringsAsFactors = FALSE)
    else zl
  }, striped = TRUE, hover = TRUE)

  # ---- Tab 5: Selected logs -------------------------------------------------
  output$selected_logs_header <- renderUI({
    r <- result(); req(!is.null(r))
    n_total <- nrow(r$selected_logs_all)
    div(class = "stat-strip",
        if (n_total == 0L) "No selected logs."
        else sprintf("Total selected logs: %d  |  Showing first 100.",
                     n_total))
  })

  output$tbl_selected_logs <- renderTable({
    r <- result(); req(!is.null(r))
    sl <- r$selected_logs_all
    if (nrow(sl) == 0L)
      return(data.frame(Message = "No selected logs.", stringsAsFactors = FALSE))
    head(sl, 100L)
  }, striped = TRUE, hover = TRUE, digits = 3)

  # ---- Prices and assortments tab ------------------------------------------
  output$tbl_ref_products <- renderTable({
    APP_PRODUCTS
  }, striped = TRUE, hover = TRUE)

  output$tbl_ref_lc <- renderTable({
    lc <- APP_LC
    # flag rows where the length class is NOT priced in default matrix
    sup_dm <- get_supported_sawlog_lengths(APP_PRICE_LONG)$LengthClass_dm
    lc$Note <- ifelse(
      grepl("sawlog", lc$ProductKey, ignore.case = TRUE) &
        !is.na(lc$LogLength_dm) & !(lc$LogLength_dm %in% sup_dm),
      "not priced in default 2021 matrix", "")
    lc
  }, striped = TRUE, hover = TRUE)

  output$price_matrix_header <- renderUI({
    n <- nrow(APP_PRICE_LONG)
    shown <- min(200L, n)
    div(class = "stat-strip",
        sprintf("Total rows: %d.%s", n,
                if (n > 200L)
                  sprintf("  Showing first %d.", shown)
                else ""))
  })

  output$tbl_ref_price <- renderTable({
    pm <- APP_PRICE_LONG
    keep <- intersect(c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm",
                        "PriceNOKperM3", "PriceScenario", "PriceType",
                        "PriceSource", "Notes"),
                      names(pm))
    head(pm[, keep, drop = FALSE], 200L)
  }, striped = TRUE, hover = TRUE, digits = 2)

}  # end server
