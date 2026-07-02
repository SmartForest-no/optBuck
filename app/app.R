# ==============================================================================
# app/app.R
# Purpose : nfiBuck sampled-plot bucking estimator (Shiny prototype v1).
# Run     : shiny::runApp("app/app.R")   OR
#           shiny::runApp("S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck/app")
# Backend : scripts/app_backend/01_run_sampled_plot_bucking.R
# ==============================================================================

library(shiny)
suppressPackageStartupMessages(library(arrow))

ROOT <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"

source(file.path(ROOT, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"))

# ---- plot metadata ----------------------------------------------------------
plots_all <- as.data.frame(arrow::read_parquet(
  file.path(ROOT, "data_processed_github", "nfi_plots_github.parquet")))

# ---- HKL group mapping (user-facing -> raw hogstkl codes) ------------------
HKL_CHOICES <- c(
  "HKL 3 - younger production forest" = "3",
  "HKL 4 - older production forest"   = "4",
  "HKL 5 - mature / old-growth forest" = "5"
)
HKL_MAP <- list("3" = c(31L, 32L), "4" = c(41L, 42L), "5" = c(51L, 52L))

BONITET_LABELS <- c(
  "6"="6 (very low)","8"="8","11"="11","14"="14",
  "17"="17","20"="20","23"="23","26"="26"
)
# Productive-forest plots only (raw HKL 3-5, codes 31-52)
plots_prod <- plots_all[plots_all$hogstkl %in% unlist(HKL_MAP), , drop=FALSE]

ESTIMATE_LABEL <- "Sampled-plot estimate. Not ha-expanded. No NFI factors applied."

# ---- CSS --------------------------------------------------------------------
APP_CSS <- "
  .estimate-label{background:#f8f9fa;border-left:4px solid #6c757d;
    padding:8px 14px;margin-bottom:12px;font-size:.88em;color:#555}
  .status-idle{color:#888;font-style:italic;padding:4px 0}
  .status-done{color:#28a745;font-weight:bold;padding:4px 0}
  .status-error{color:#dc3545;font-weight:bold;padding:4px 0;white-space:pre-wrap}
  .card-row{display:flex;flex-wrap:wrap;gap:10px;margin-bottom:16px}
  .vcard{background:#fff;border:1px solid #dee2e6;border-radius:6px;
    padding:12px 18px;min-width:130px;flex:1;text-align:center}
  .vcard .vnum{font-size:1.8em;font-weight:bold;color:#2c7be5}
  .vcard .vlbl{font-size:.78em;color:#666;margin-top:2px}
  .vcard.green  .vnum{color:#28a745}
  .vcard.orange .vnum{color:#e07000}
  .section-hdr{font-size:.9em;font-weight:600;color:#444;
    border-bottom:1px solid #dee2e6;margin:14px 0 6px;padding-bottom:3px}
  .stat-strip{background:#f8f9fa;border-radius:4px;padding:7px 12px;
    font-size:.88em;color:#555;margin-bottom:10px}
"

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(APP_CSS))),
  titlePanel("nfiBuck \u2014 Sampled-plot bucking estimator (v1)"),

  sidebarLayout(
    sidebarPanel(width = 3,

      selectInput("species_group", "Species group",
                  choices = c("spruce","pine","both"), selected = "spruce"),

      checkboxGroupInput("hkl_group", "Maturity class",
                         choiceNames  = names(HKL_CHOICES),
                         choiceValues = unname(HKL_CHOICES),
                         selected     = "4"),

      uiOutput("abonitet_ui"),

      selectInput("n_plots", "Plots to sample",
                  choices = c("5","10","25","50"), selected = "10"),

      numericInput("seed", "Random seed", value = 42L, min = 1L, step = 1L),
      hr(),
      actionButton("run_btn", "Run optimizer",
                   class = "btn-primary", width = "100%"),
      hr(),
      div(style = "font-size:.88em;color:#555;",
          uiOutput("stratum_preview"), br(),
          div(style = "color:#888;font-size:.9em;",
              "HKL 1-2 (young/establishment) are excluded from this prototype."))
    ),

    mainPanel(width = 9,
      div(class = "estimate-label", ESTIMATE_LABEL),
      uiOutput("run_status"), br(),

      tabsetPanel(id = "result_tabs",

        tabPanel("Summary", br(),
          uiOutput("summary_cards"), br(),
          uiOutput("summary_detail")
        ),

        tabPanel("Products", br(),
          div(class="section-hdr","Product summary"),
          tableOutput("tbl_products"), br(),
          fluidRow(
            column(6, plotOutput("bar_value_by_product",  height="240px")),
            column(6, plotOutput("bar_volume_by_product", height="240px"))
          )
        ),

        tabPanel("Plots", br(),
          div(class="section-hdr","Plot-level results"),
          uiOutput("plot_stats_text"),
          tableOutput("tbl_plots"), br(),
          plotOutput("hist_value_ha", height="260px")
        ),

        tabPanel("Diagnostics", br(),
          div(class="section-hdr","Stage timings (slowest first)"),
          tableOutput("tbl_stage_timings"), br(),
          div(class="section-hdr","Run diagnostics"),
          tableOutput("tbl_diag"), br(),
          div(class="section-hdr","Price validation"),
          tableOutput("tbl_price_val"), br(),
          div(class="section-hdr","Zero-log trees"),
          tableOutput("tbl_zero_logs")
        ),

        tabPanel("Selected logs", br(),
          uiOutput("selected_logs_header"),
          tableOutput("tbl_selected_logs")
        )
      )
    )
  )
)

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Dynamic bonitet selector filtered to selected HKL group
  output$abonitet_ui <- renderUI({
    hkl_raw    <- unlist(HKL_MAP[input$hkl_group])
    abon_avail <- as.character(sort(unique(
      plots_prod$abonitet[plots_prod$hogstkl %in% hkl_raw])))
    lbl <- BONITET_LABELS[abon_avail]
    lbl[is.na(lbl)] <- abon_avail[is.na(lbl)]
    choices_named <- setNames(abon_avail, lbl)
    pref  <- intersect(c("14","11","17"), abon_avail)
    sel   <- if (length(pref) > 0L) pref else abon_avail[1L]
    checkboxGroupInput("abonitet", "Site class (bonitet)",
                       choiceNames  = unname(choices_named),
                       choiceValues = abon_avail,
                       selected     = sel)
  })

  # Stratum availability preview
  output$stratum_preview <- renderUI({
    hkl_raw <- unlist(HKL_MAP[input$hkl_group])
    abon    <- as.integer(input$abonitet)
    if (length(hkl_raw) == 0L || length(abon) == 0L)
      return(div(style="color:#cc0000;",
                 "Select at least one maturity class and one bonitet."))
    n_elig <- nrow(plots_prod[
      plots_prod$hogstkl  %in% hkl_raw &
      plots_prod$abonitet %in% abon, ])
    div(sprintf("%d eligible plot(s) available.", n_elig), br(),
        if (n_elig == 0L)
          span(style="color:#cc0000;", "No matching plots - adjust selection.")
        else if (n_elig < as.integer(input$n_plots))
          span(style="color:#e07000;",
               sprintf("Fewer than %s plots; all will be used.", input$n_plots))
        else
          span(style="color:#28a745;", "Stratum OK.")
    )
  })

  # Run status line
  output$run_status <- renderUI({
    if (input$run_btn == 0L)
      return(div(class="status-idle",
                 "Select parameters and click Run optimizer."))
    r <- result()
    if (is.null(r))
      return(div(class="status-error", "Run failed. See error notification."))
    div(class="status-done",
        sprintf("Run complete in %.1f s  |  %d plot(s)  |  %d trees  |  %d selected logs  |  ID: %s",
                r$elapsed_seconds, r$n_plots_sampled, r$n_trees_total,
                nrow(r$selected_logs_all), r$run_id))
  })

  # Backend call with multi-stage progress
  result <- eventReactive(input$run_btn, {
    req(length(input$hkl_group) > 0L, length(input$abonitet) > 0L)
    # Map user HKL group -> raw hogstkl codes
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
        Sys.sleep(0)
        incProgress(0.20, detail = "Running bucking optimizer...")
        res <- run_sampled_plot_bucking(params = params,
                                        price_matrix_path = "default_2021",
                                        root = ROOT)
        incProgress(0.45, detail = "Summarising results")
        Sys.sleep(0)
        incProgress(0.10, detail = "Rendering outputs")
        res
      }, error = function(e) {
        showNotification(paste0("Backend error: ", conditionMessage(e)),
                         type = "error", duration = NULL)
        NULL
      })
      r
    })
  })

  # ---- Summary tab ----------------------------------------------------------
  output$summary_cards <- renderUI({
    r <- result(); req(!is.null(r))
    total_val <- sum(r$selected_logs_all$value_nok,       na.rm=TRUE)
    total_vol <- sum(r$selected_logs_all$price_volume_m3, na.rm=TRUE)
    pct_logs  <- if (r$n_trees_total > 0)
      round(100 * r$n_trees_with_logs / r$n_trees_total) else 0L
    card <- function(num, lbl, cls="") {
      div(class=paste("vcard",cls),
          div(class="vnum",num), div(class="vlbl",lbl))
    }
    tagList(div(class="card-row",
      card(r$n_plots_sampled,         "Plots sampled"),
      card(r$n_trees_total,           "Trees (filtered)"),
      card(sprintf("%d (%d%%)", r$n_trees_with_logs, pct_logs),
           "Trees with logs","green"),
      card(nrow(r$selected_logs_all), "Selected logs","green"),
      card(format(round(total_val,0), big.mark=",", scientific=FALSE),
           "Total value (NOK)"),
      card(round(total_vol,2),        "Total volume (m\u00b3)"),
      card(sprintf("%.1f s", r$elapsed_seconds), "Runtime","orange")
    ))
  })

  output$summary_detail <- renderUI({
    r <- result(); req(!is.null(r))
    tagList(
      div(class="stat-strip",
          sprintf("Eligible plots: %d  |  Zero-log trees: %d  |  Unpriced-candidate groups: %d",
                  r$n_eligible_plots, nrow(r$zero_log_trees), nrow(r$unpriced_summary))),
      div(class="stat-strip", style="color:#555;", ESTIMATE_LABEL)
    )
  })

  # ---- Products tab ---------------------------------------------------------
  output$tbl_products <- renderTable({
    r <- result(); req(!is.null(r)); r$product_summary
  }, striped=TRUE, hover=TRUE, digits=3)

  output$bar_value_by_product <- renderPlot({
    r <- result(); req(!is.null(r))
    ps <- r$product_summary; req(nrow(ps) > 0L)
    vals <- setNames(ps$total_value_nok, ps$ProductKey)
    cols <- c("#4a9a6e","#2c7be5","#e07000","#8b5cf6")[seq_along(vals)]
    par(mar=c(4,5,3,1))
    bp <- barplot(vals, col=cols, border=NA,
                  main="Total value (NOK) by product",
                  ylab="NOK", las=2, cex.names=0.8)
    text(bp, vals/2,
         labels=format(round(vals), big.mark=",", scientific=FALSE),
         col="white", cex=0.8, font=2)
  })

  output$bar_volume_by_product <- renderPlot({
    r <- result(); req(!is.null(r))
    ps <- r$product_summary; req(nrow(ps) > 0L)
    vols <- setNames(ps$total_volume_m3, ps$ProductKey)
    cols <- c("#4a9a6e","#2c7be5","#e07000","#8b5cf6")[seq_along(vols)]
    par(mar=c(4,5,3,1))
    bp <- barplot(vols, col=cols, border=NA,
                  main="Total volume (m\u00b3) by product",
                  ylab="m\u00b3", las=2, cex.names=0.8)
    text(bp, vols/2, labels=round(vols,2), col="white", cex=0.8, font=2)
  })

  # ---- Plots tab ------------------------------------------------------------
  output$plot_stats_text <- renderUI({
    r <- result(); req(!is.null(r))
    vals <- r$plot_summary$value_per_ha_nok
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) return(NULL)
    div(class="stat-strip",
        sprintf("Value per ha (NOK):  min=%s  mean=%s  median=%s  max=%s",
                format(round(min(vals)),    big.mark=","),
                format(round(mean(vals)),   big.mark=","),
                format(round(median(vals)), big.mark=","),
                format(round(max(vals)),    big.mark=",")))
  })

  output$tbl_plots <- renderTable({
    r <- result(); req(!is.null(r)); r$plot_summary
  }, striped=TRUE, hover=TRUE, digits=2)

  output$hist_value_ha <- renderPlot({
    r <- result(); req(!is.null(r))
    vals <- r$plot_summary$value_per_ha_nok
    vals <- vals[!is.na(vals)]; req(length(vals) > 0L)
    old <- par(mar=c(4.5,4.5,3,1)); on.exit(par(old))
    hist(vals, col="#4a9a6e", border="white",
         breaks=max(3L, min(15L, length(vals))),
         main="Value per ha (NOK) - sampled plots",
         xlab="NOK / ha", ylab="Number of plots")
    abline(v=mean(vals), col="#e05a2b", lwd=2, lty=2)
    legend("topright",
           legend=sprintf("mean = %s", format(round(mean(vals)), big.mark=",")),
           col="#e05a2b", lty=2, lwd=2, bty="n", cex=0.85)
  })

  # ---- Diagnostics tab ------------------------------------------------------
  output$tbl_stage_timings <- renderTable({
    r <- result(); req(!is.null(r))
    st <- r$diagnostics$stage_timings
    req(is.data.frame(st))
    st2 <- st[st$stage != "total", ]
    st2 <- st2[order(st2$elapsed_seconds, decreasing=TRUE), ]
    pct <- round(100 * st2$elapsed_seconds / r$diagnostics$elapsed_seconds)
    data.frame(Stage=st2$stage, Seconds=round(st2$elapsed_seconds,2),
               "Pct total"=paste0(pct,"%"), check.names=FALSE,
               stringsAsFactors=FALSE)
  }, striped=TRUE, hover=TRUE)

  output$tbl_diag <- renderTable({
    r <- result(); req(!is.null(r))
    diag <- r$diagnostics
    skip <- c("stage_timings","elapsed_seconds")
    diag <- diag[!names(diag) %in% skip]
    vals <- vapply(diag, function(x) paste(x, collapse="; "), character(1))
    data.frame(Parameter=names(vals), Value=unname(vals),
               stringsAsFactors=FALSE)
  }, striped=TRUE, hover=TRUE)

  output$tbl_price_val <- renderTable({
    r <- result(); req(!is.null(r)); r$price_validation_checks
  }, striped=TRUE, hover=TRUE)

  output$tbl_zero_logs <- renderTable({
    r <- result(); req(!is.null(r))
    zl <- r$zero_log_trees
    if (nrow(zl)==0L)
      data.frame(Message="No zero-log trees in this sample.",
                 stringsAsFactors=FALSE)
    else zl
  }, striped=TRUE, hover=TRUE)

  # ---- Selected logs tab ----------------------------------------------------
  output$selected_logs_header <- renderUI({
    r <- result(); req(!is.null(r))
    n <- nrow(r$selected_logs_all)
    div(class="stat-strip",
        if (n==0L) "No selected logs."
        else sprintf("Total selected logs: %d  |  Showing first 100.", n))
  })

  output$tbl_selected_logs <- renderTable({
    r <- result(); req(!is.null(r))
    sl <- r$selected_logs_all
    if (nrow(sl)==0L)
      return(data.frame(Message="No selected logs.", stringsAsFactors=FALSE))
    head(sl, 100L)
  }, striped=TRUE, hover=TRUE, digits=3)
}

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)