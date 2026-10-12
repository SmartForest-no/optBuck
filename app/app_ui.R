# app/app_ui.R
#
# Purpose : Shiny UI definition (`ui`). Sourced by app.R after setup.R.
#           Not named ui.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.

# ---- UI ---------------------------------------------------------------------
ui <- fluidPage(

  tags$head(tags$style(HTML(APP_CSS))),

  titlePanel("Apter \u2014 Bucking simulator (v1)"),

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

      selectInput("fylnr_sel", "County",
                  choices  = FYLNR_CHOICES,
                  selected = "__all__"),

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
              "HKL 1\u20132 (young/establishment) are excluded from this prototype."),
          br(),
          div(style = "color:#888; font-size:0.85em;",
              "County filtering uses fylnr from the sanitized NFI-derived data.",
              " Codes use current and historical names where possible."))
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
              "Note: sawlog length 31 dm is priced by interpolation",
              " (2×34dm price − 37dm price), not from the original",
              " PriceMatrixGran/PriceMatrixFuru source spreadsheet.",
              " See the Notes column below for flagged rows",
              " (PriceSource = \"Interpolated\")."),
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
            tags$li("31 dm sawlog prices are interpolated (2×34dm − 37dm), flagged PriceSource = \"Interpolated\" in the price table."),
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
