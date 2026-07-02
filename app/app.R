# ==============================================================================
# app/app.R
#
# Purpose : Minimal Shiny prototype for nfiBuck sampled-plot bucking estimator.
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
