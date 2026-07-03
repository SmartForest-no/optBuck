# ==============================================================================
# app/app.R
#
# Purpose : Shiny prototype for nfiBuck sampled-plot bucking estimator.
#           v1 scope: spruce/pine, default 2021 price matrix, sampled plots.
#
# Run     : shiny::runApp("app/app.R") from project root, OR
#           shiny::runApp("S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck/app")
#
# Data    : data_processed_github/ (sanitized, no original plot IDs)
# Backend : scripts/app_backend/01_run_sampled_plot_bucking.R
# ==============================================================================

library(shiny)
suppressPackageStartupMessages(library(arrow))

# ---- project root -----------------------------------------------------------
ROOT <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"

# ---- source backend (also sources all 7 helpers) ----------------------------
source(file.path(ROOT, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"))

# ---- load plot metadata for stratum availability preview --------------------
plots_all <- as.data.frame(arrow::read_parquet(
  file.path(ROOT, "data_processed_github", "nfi_plots_github.parquet")))

# ---- load product / price reference data (displayed read-only) -------------
APP_PRODUCTS   <- load_products(ROOT)
APP_LC         <- load_length_classes(ROOT)
APP_PRICE_LONG <- load_price_matrix_long(ROOT)

ALL_HOGSTKL  <- as.character(sort(unique(plots_all$hogstkl)))
ALL_ABONITET <- as.character(sort(unique(plots_all$abonitet)))

ESTIMATE_LABEL <- "Sampled-plot estimate. Not ha-expanded. No NFI factors applied."

# ---- shared CSS -------------------------------------------------------------
APP_CSS <- "
  .estimate-label {
    background: #f8f9fa;
    border-left: 4px solid #6c757d;
    padding: 8px 14px;
    margin-bottom: 12px;
    font-size: 0.88em;
    color: #555;
  }
  .status-idle  { color: #888; font-style: italic; padding: 4px 0; }
  .status-done  { color: #28a745; font-weight: bold; padding: 4px 0; }
  .status-error { color: #dc3545; font-weight: bold; padding: 4px 0;
                  white-space: pre-wrap; }
  .card-row { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 16px; }
  .vcard {
    background: #fff;
    border: 1px solid #dee2e6;
    border-radius: 6px;
    padding: 12px 18px;
    min-width: 130px;
    flex: 1;
    text-align: center;
  }
  .vcard .vnum  { font-size: 1.8em; font-weight: bold; color: #2c7be5; }
  .vcard .vlbl  { font-size: 0.78em; color: #666; margin-top: 2px; }
  .vcard.green  .vnum { color: #28a745; }
  .vcard.orange .vnum { color: #e07000; }
  .section-hdr {
    font-size: 0.9em; font-weight: 600; color: #444;
    border-bottom: 1px solid #dee2e6;
    margin: 14px 0 6px; padding-bottom: 3px;
  }
  .stat-strip {
    background: #f8f9fa;
    border-radius: 4px;
    padding: 7px 12px;
    font-size: 0.88em;
    color: #555;
    margin-bottom: 10px;
  }
"

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(

  tags$head(tags$style(HTML(APP_CSS))),

  titlePanel("nfiBuck \u2014 Sampled-plot bucking estimator (v1)"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput("species_group", "Species group",
                  choices = c("spruce", "pine", "both"), selected = "spruce"),

      checkboxGroupInput("hkl_group", "Maturity class",
                         choiceNames  = names(HKL_CHOICES),
                         choiceValues = unname(HKL_CHOICES),
                         selected     = "4"),

      uiOutput("abonitet_ui"),

      selectInput("n_plots", "Plots to sample",
                  choices = c("5", "10", "25", "50"), selected = "10"),

      numericInput("seed", "Random seed", value = 42L, min = 1L, step = 1L),

      hr(),
      checkboxInput("compare_on", "Run scenario comparison", value = FALSE),
      conditionalPanel("input.compare_on == true",
        selectInput("s2_mode", "Scenario 2 prices",
                    choices = c("Same as Scenario 1" = "same",
                                "Apply multipliers"   = "multiplier",
                                "Upload matrix (CSV/RDS)" = "upload"),
                    selected = "same"),
        conditionalPanel("input.s2_mode == 'multiplier'",
          numericInput("mult_saw",  "Sawlog multiplier",   1.00, 0, 5, step = 0.05),
          numericInput("mult_pulp", "Pulpwood multiplier", 1.00, 0, 5, step = 0.05),
          numericInput("mult_wst",  "Waste multiplier",    1.00, 0, 5, step = 0.05)
        ),
        conditionalPanel("input.s2_mode == 'upload'",
          fileInput("s2_file", "Upload price matrix", accept = c(".csv", ".rds"))
        )
      ),

      hr(),

      actionButton("run_btn", "Run optimizer",
                   class = "btn-primary", width = "100%"),

      hr(),

      div(style = "font-size: 0.88em; color: #555;",
          uiOutput("stratum_preview"),
          br(),
          div(style = "color:#888; font-size:0.9em;",
              "HKL 1\u20132 (young/establishment) are excluded from this prototype."))
    ),

    mainPanel(
      width = 9,

      div(class = "estimate-label", ESTIMATE_LABEL),
      uiOutput("run_status"),
      br(),

      tabsetPanel(id = "result_tabs",

        tabPanel("Summary",
          br(),
          uiOutput("summary_cards"),
          br(),
          uiOutput("summary_detail")
        ),

        tabPanel("Products",
          br(),
          div(class = "section-hdr", "Product summary"),
          tableOutput("tbl_products"),
          br(),
          fluidRow(
            column(6, plotOutput("bar_value_by_product",  height = "240px")),
            column(6, plotOutput("bar_volume_by_product", height = "240px"))
          )
        ),

        tabPanel("Plots",
          br(),
          div(class = "section-hdr", "Plot-level results"),
          uiOutput("plot_stats_text"),
          tableOutput("tbl_plots"),
          br(),
          plotOutput("hist_value_ha", height = "260px")
        ),

        tabPanel("Diagnostics",
          br(),
          div(class = "section-hdr", "Stage timings (slowest first)"),
          tableOutput("tbl_stage_timings"),
          br(),
          div(class = "section-hdr", "Run diagnostics"),
          tableOutput("tbl_diag"),
          br(),
          div(class = "section-hdr", "Price validation"),
          tableOutput("tbl_price_val"),
          br(),
          div(class = "section-hdr", "Zero-log trees"),
          tableOutput("tbl_zero_logs")
        ),

        tabPanel("Selected logs",
          br(),
          uiOutput("selected_logs_header"),
          tableOutput("tbl_selected_logs")
        ),
        tabPanel("Prices and assortments",
          br(),
          div(class = "section-hdr", "Product definitions"),
          div(class = "stat-strip",
              "These are the product specifications used by the optimizer.",
              " DiameterUnderBark = FALSE: all diameters are over-bark",
              " (taperNOR, with_bark = TRUE)."),
          tableOutput("tbl_ref_products"),
          br(),
          div(class = "section-hdr", "Length and diameter classes"),
          div(class = "stat-strip",
              "Note: sawlog length 31 dm is defined here but absent from the",
              " default 2021 price matrix (matrix starts at 34 dm).",
              " 31 dm sawlog candidates are generated but remain unpriced and",
              " cannot be selected by the optimizer."),
          uiOutput("lc_filter_note"),
          tableOutput("tbl_ref_lc"),
          br(),
          div(class = "section-hdr", "Default 2021 price matrix"),
          uiOutput("price_matrix_header"),
          tableOutput("tbl_ref_price"),
          br(),
          div(class = "section-hdr", "Notes and limitations"),
          tags$ul(
            tags$li("The current app uses the default 2021 price matrix."),
            tags$li("Unpriced candidates are excluded from optimization and reported in Diagnostics."),
            tags$li("The known 31 dm sawlog price gap is treated as a data gap, not as zero price."),
            tags$li("Diameter classes above 50 cm are capped to 50 cm for price lookup only; physical diameters are preserved."),
            tags$li("Volumes and diameters use taperNOR over-bark values. No bark correction is applied."),
            tags$li("Estimates are sampled-plot only. No NFI expansion factors or plot weights are applied.")
          )
        ),
        tabPanel("Scenario comparison",
          br(),
          uiOutput("cmp_status"),
          br(),
          div(class = "section-hdr", "High-level comparison"),
          tableOutput("tbl_cmp_overview"),
          br(),
          div(class = "section-hdr", "Difference summary"),
          tableOutput("tbl_cmp_delta"),
          br(),
          div(class = "section-hdr", "Product-level comparison"),
          tableOutput("tbl_cmp_products"),
          br(),
          fluidRow(
            column(6, plotOutput("bar_cmp_value", height = "240px")),
            column(6, plotOutput("bar_cmp_delta", height = "240px"))
          ),
          br(),
          div(class = "section-hdr", "Comparison diagnostics"),
          tableOutput("tbl_cmp_diag")
        )
      )
    )
  )
)
# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Stratum availability preview
  output$stratum_preview <- renderUI({
    hkl  <- as.integer(input$hogstkl)
    abon <- as.integer(input$abonitet)
    if (length(hkl) == 0L || length(abon) == 0L)
      return(div(style = "color:#cc0000;",
                 "Select at least one hogstkl and one abonitet."))
    n_elig <- nrow(plots_all[
      plots_all$hogstkl %in% hkl & plots_all$abonitet %in% abon, ])
    div(
      sprintf("%d eligible plot(s) available.", n_elig), br(),
      if (n_elig == 0L)
        span(style = "color:#cc0000;", "No matching plots ??? adjust selection.")
      else if (n_elig < as.integer(input$n_plots))
        span(style = "color:#e07000;",
             sprintf("Fewer than %s plots exist; all will be used.",
                     input$n_plots))
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

    # Map user-facing HKL group -> raw hogstkl codes
    raw_hogstkl <- unlist(HKL_MAP[input$hkl_group])

    params <- list(
      species_group = input$species_group,
      hogstkl       = raw_hogstkl,
      abonitet      = as.integer(input$abonitet),
      n_plots       = as.integer(input$n_plots),
      seed          = as.integer(input$seed)
    )

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


  # ---- Comparison: helper, S2 reactive, renders ----------------------------

  .make_s2_pm <- function(base_pm, mult_saw, mult_pulp, mult_wst) {
    pm2 <- base_pm
    pk  <- pm2$ProductKey
    pm2$PriceNOKperM3 <- ifelse(
      grepl("sawlog",  pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_saw,
      ifelse(grepl("pulp",  pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_pulp,
      ifelse(grepl("waste", pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_wst,
             pm2$PriceNOKperM3)))
    pm2
  }

  result_s2 <- reactive({
    req(isTRUE(input$compare_on), !is.null(result()))
    params <- list(
      species_group = isolate(input$species_group),
      hogstkl       = unlist(HKL_MAP[isolate(input$hkl_group)]),
      abonitet      = as.integer(isolate(input$abonitet)),
      n_plots       = as.integer(isolate(input$n_plots)),
      seed          = as.integer(isolate(input$seed))
    )
    s2_mode <- isolate(input$s2_mode)
    s2_pm <- switch(s2_mode,
      "same"       = "default_2021",
      "multiplier" = {
        pm2 <- .make_s2_pm(APP_PRICE_LONG,
                            as.numeric(isolate(input$mult_saw)),
                            as.numeric(isolate(input$mult_pulp)),
                            as.numeric(isolate(input$mult_wst)))
        tmp <- tempfile(fileext = ".csv")
        readr::write_csv(pm2, tmp)
        on.exit(tryCatch(file.remove(tmp), error = function(e) NULL), add = TRUE)
        tmp
      },
      "upload" = {
        req(!is.null(isolate(input$s2_file)))
        isolate(input$s2_file)$datapath
      },
      "default_2021"
    )
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
}

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
                        "PriceNOKperM3", "PriceScenario", "PriceType"),
                      names(pm))
    head(pm[, keep, drop = FALSE], 200L)
  }, striped = TRUE, hover = TRUE, digits = 2)

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)
