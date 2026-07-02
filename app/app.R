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

      checkboxGroupInput("hogstkl", "Maturity class (hogstkl)",
                         choices = ALL_HOGSTKL, selected = c("41", "51"),
                         inline  = TRUE),

      checkboxGroupInput("abonitet", "Site class (abonitet)",
                         choices = ALL_ABONITET, selected = c("14", "17"),
                         inline  = TRUE),

      selectInput("n_plots", "Plots to sample",
                  choices = c("5", "10", "25", "50"), selected = "10"),

      numericInput("seed", "Random seed", value = 42L, min = 1L, step = 1L),

      hr(),

      actionButton("run_btn", "Run optimizer",
                   class = "btn-primary", width = "100%"),

      hr(),

      div(style = "font-size: 0.88em; color: #555;",
          uiOutput("stratum_preview"))
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
        span(style = "color:#cc0000;", "No matching plots — adjust selection.")
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

  # Backend call with progress indicator
  result <- eventReactive(input$run_btn, {
    req(length(input$hogstkl) > 0L, length(input$abonitet) > 0L)
    params <- list(
      species_group = input$species_group,
      hogstkl       = as.integer(input$hogstkl),
      abonitet      = as.integer(input$abonitet),
      n_plots       = as.integer(input$n_plots),
      seed          = as.integer(input$seed)
    )
    withProgress(
      message = sprintf("Running optimizer (%s plots, seed %s)...",
                        input$n_plots, input$seed),
      value = 0.3,
      tryCatch(
        run_sampled_plot_bucking(params = params,
                                 price_matrix_path = "default_2021",
                                 root = ROOT),
        error = function(e) {
          showNotification(
            paste0("Backend error: ", conditionMessage(e)),
            type = "error", duration = NULL)
          NULL
        }
      )
    )
  })

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
         main = "Value per ha (NOK) — sampled plots",
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

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)

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

ALL_HOGSTKL  <- as.character(sort(unique(plots_all$hogstkl)))
ALL_ABONITET <- as.character(sort(unique(plots_all$abonitet)))

ESTIMATE_LABEL <- "Sampled-plot estimate. Not ha-expanded. No NFI factors applied."

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(

  tags$head(tags$style(HTML("
    .estimate-label {
      background: #f8f9fa;
      border-left: 4px solid #6c757d;
      padding: 8px 14px;
      margin-bottom: 14px;
      font-size: 0.88em;
      color: #555;
    }
    .status-idle   { color: #888; font-style: italic; }
    .status-done   { color: #28a745; font-weight: bold; }
    .status-error  { color: #dc3545; font-weight: bold; }
  "))),

  titlePanel("nfiBuck \u2014 Sampled-plot bucking estimator (v1)"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      selectInput("species_group", "Species group",
                  choices  = c("spruce", "pine", "both"),
                  selected = "spruce"),

      checkboxGroupInput("hogstkl", "Maturity class (hogstkl)",
                         choices  = ALL_HOGSTKL,
                         selected = c("41", "51"),
                         inline   = TRUE),

      checkboxGroupInput("abonitet", "Site class (abonitet)",
                         choices  = ALL_ABONITET,
                         selected = c("14", "17"),
                         inline   = TRUE),

      selectInput("n_plots", "Plots to sample",
                  choices  = c("5", "10", "25", "50"),
                  selected = "10"),

      numericInput("seed", "Random seed", value = 42L, min = 1L, step = 1L),

      hr(),

      actionButton("run_btn", "Run optimizer",
                   class = "btn-primary", width = "100%"),

      hr(),

      div(style = "font-size:0.88em; color:#555;",
          uiOutput("stratum_preview"))
    ),

    mainPanel(
      width = 9,

      div(class = "estimate-label", ESTIMATE_LABEL),

      uiOutput("run_status"),
      br(),

      tabsetPanel(id = "result_tabs",

        tabPanel("Summary",
          br(),
          tableOutput("tbl_summary")
        ),

        tabPanel("Product summary",
          br(),
          tableOutput("tbl_products")
        ),

        tabPanel("Plot summary",
          br(),
          tableOutput("tbl_plots"),
          br(),
          plotOutput("hist_value_ha", height = "280px")
        ),

        tabPanel("Diagnostics",
          br(),
          h5("Run diagnostics"),
          tableOutput("tbl_diag"),
          br(),
          h5("Price validation checks"),
          tableOutput("tbl_price_val"),
          br(),
          h5("Zero-log trees"),
          tableOutput("tbl_zero_logs")
        ),

        tabPanel("Selected logs (first 100)",
          br(),
          tableOutput("tbl_selected_logs")
        )
      )
    )
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Stratum availability preview (reactive on UI changes, no run needed)
  output$stratum_preview <- renderUI({
    hkl  <- as.integer(input$hogstkl)
    abon <- as.integer(input$abonitet)
    if (length(hkl) == 0L || length(abon) == 0L) {
      return(div(style = "color:#cc0000;",
                 "Select at least one hogstkl and one abonitet."))
    }
    n_elig <- nrow(plots_all[
      plots_all$hogstkl %in% hkl & plots_all$abonitet %in% abon, ])
    div(
      sprintf("%d eligible plot(s) available.", n_elig),
      br(),
      if (n_elig == 0L)
        span(style = "color:#cc0000;", "No matching plots — adjust selection.")
      else if (n_elig < as.integer(input$n_plots))
        span(style = "color:#e07000;",
             sprintf("Fewer than %s plots available; all will be used.",
                     input$n_plots))
      else
        span(style = "color:#28a745;", "Stratum OK.")
    )
  })

  # Run status message
  run_state <- reactiveVal("idle")   # "idle", "done", "error"
  run_msg   <- reactiveVal("")

  output$run_status <- renderUI({
    switch(run_state(),
      idle  = div(class = "status-idle",
                  "Select parameters and click Run optimizer."),
      done  = div(class = "status-done",  run_msg()),
      error = div(class = "status-error", run_msg())
    )
  })

  # Backend call — only fires when Run is clicked
  result <- eventReactive(input$run_btn, {
    req(length(input$hogstkl)  > 0L,
        length(input$abonitet) > 0L)

    run_state("idle")
    run_msg("Running...")

    params <- list(
      species_group   = input$species_group,
      hogstkl         = as.integer(input$hogstkl),
      abonitet        = as.integer(input$abonitet),
      n_plots         = as.integer(input$n_plots),
      seed            = as.integer(input$seed)
    )

    r <- tryCatch(
      run_sampled_plot_bucking(
        params            = params,
        price_matrix_path = "default_2021",
        root              = ROOT
      ),
      error = function(e) {
        run_state("error")
        run_msg(paste("Error:", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(r)) {
      run_state("done")
      run_msg(sprintf(
        "Run complete in %.1f s  |  %d plot(s), %d trees, %d selected logs  |  ID: %s",
        r$elapsed_seconds, r$n_plots_sampled, r$n_trees_total,
        nrow(r$selected_logs_all), r$run_id))
    }

    r
  })

  # ---- Tab 1: Summary -------------------------------------------------------
  output$tbl_summary <- renderTable({
    r <- result()
    req(!is.null(r))
    total_val <- sum(r$selected_logs_all$value_nok,        na.rm = TRUE)
    total_vol <- sum(r$selected_logs_all$price_volume_m3,  na.rm = TRUE)
    data.frame(
      Metric = c("Eligible plots in stratum",
                 "Plots sampled",
                 "Trees total (passing filter)",
                 "Trees with selected logs",
                 "Selected logs",
                 "Total value (NOK)",
                 "Total volume (m\u00b3)",
                 "Runtime (s)"),
      Value  = c(r$n_eligible_plots,
                 r$n_plots_sampled,
                 r$n_trees_total,
                 r$n_trees_with_logs,
                 nrow(r$selected_logs_all),
                 round(total_val, 2),
                 round(total_vol, 4),
                 round(r$elapsed_seconds, 1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, hover = TRUE)

  # ---- Tab 2: Product summary -----------------------------------------------
  output$tbl_products <- renderTable({
    r <- result(); req(!is.null(r))
    r$product_summary
  }, striped = TRUE, hover = TRUE, digits = 4)

  # ---- Tab 3: Plot summary + histogram --------------------------------------
  output$tbl_plots <- renderTable({
    r <- result(); req(!is.null(r))
    r$plot_summary
  }, striped = TRUE, hover = TRUE, digits = 2)

  output$hist_value_ha <- renderPlot({
    r <- result(); req(!is.null(r))
    ps   <- r$plot_summary
    req(nrow(ps) > 0L, "value_per_ha_nok" %in% names(ps))
    vals <- ps$value_per_ha_nok[!is.na(ps$value_per_ha_nok)]
    req(length(vals) > 0L)

    old_mar <- par(mar = c(4.5, 4.5, 3, 1))
    on.exit(par(old_mar))

    hist(vals,
         main   = "Value per ha \u2014 sampled plots",
         xlab   = "Value (NOK / ha)",
         ylab   = "Number of plots",
         col    = "#4a9a6e",
         border = "white",
         breaks = max(3L, min(15L, length(vals))))
    abline(v = mean(vals), col = "#e05a2b", lwd = 2, lty = 2)
    legend("topright", legend = sprintf("mean = %s NOK/ha", format(round(mean(vals)), big.mark = ",")),
           col = "#e05a2b", lty = 2, lwd = 2, bty = "n", cex = 0.9)
  })

  # ---- Tab 4: Diagnostics ---------------------------------------------------
  output$tbl_diag <- renderTable({
    r <- result(); req(!is.null(r))
    diag <- r$diagnostics
    # Collapse list items to strings for display
    vals <- vapply(diag, function(x) paste(x, collapse = "; "), character(1))
    data.frame(Parameter = names(vals), Value = vals,
               stringsAsFactors = FALSE, row.names = NULL)
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

  # ---- Tab 5: Selected logs (first 100) ------------------------------------
  output$tbl_selected_logs <- renderTable({
    r <- result(); req(!is.null(r))
    sl <- r$selected_logs_all
    if (nrow(sl) == 0L)
      return(data.frame(Message = "No selected logs.", stringsAsFactors = FALSE))
    head(sl, 100L)
  }, striped = TRUE, hover = TRUE, digits = 4)
}

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)
