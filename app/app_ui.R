# app/app_ui.R
#
# Purpose : Shiny UI definition (`ui`). Sourced by app.R after setup.R,
#           theme.R and helpers_ui.R.
#           Not named ui.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# Layout  : bslib page_navbar (Bootstrap 5) with three pages: Home, Simulator
#           and Prices and assortments. Styling is in www/apter.css and
#           theme.R; no inline colours here.

# ---- pages ----------------------------------------------------------------------

home_page <- nav_panel(
  "Home",
  app_header("Bucking simulator for Norway spruce and Scots pine"),
  div(class = "apter-page",
    apter_card("Tree sources",
      div(class = "status-tiles",
        status_tile("NFI sample trees", "Available",
                    c("Spruce and pine from the Norwegian National Forest Inventory",
                      "Selected by species, maturity class, site class and county")),
        status_tile("Tree lists", "Planned",
                    c("Upload your own trees",
                      "Tree ID, species and diameter, with heights where available"),
                    icon_name = "list", planned = TRUE),
        status_tile("Stem profiles", "Planned",
                    c("Upload measured stem profiles",
                      "For example from harvester data"),
                    icon_name = "chart-line", planned = TRUE)
      )
    ),
    apter_card("Latest run", uiOutput("home_last_run")),
    actionButton("go_simulator", "Open simulator", class = "btn-primary")
  )
)

simulator_sidebar <- sidebar(
  width = 320,

  div(class = "sidebar-heading", "Trees"),
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

  div(class = "sidebar-heading", "Scenario"),
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

  actionButton("run_btn", "Run optimizer",
               class = "btn-primary", width = "100%"),

  div(class = "sidebar-notes",
      uiOutput("stratum_preview"),
      div("HKL 1–2 (young/establishment) are excluded from this prototype."),
      div("County filtering uses fylnr from the sanitized NFI-derived data.",
          " Codes use current and historical names where possible."))
)

simulator_page <- nav_panel(
  "Simulator",
  layout_sidebar(
    sidebar = simulator_sidebar,
    fillable = FALSE,

    div(class = "results-label", RESULTS_LABEL),

    # Details left, steps right, as in the reference's batch details page.
    layout_columns(col_widths = c(7, 5),
      apter_card("Run details",
        uiOutput("run_status"),
        uiOutput("run_params")
      ),
      apter_card("Run steps", collapsible = TRUE,
        uiOutput("run_steps")
      )
    ),

    navset_card_underline(id = "result_tabs",

      nav_panel("Summary",
        uiOutput("summary_cards"),
        uiOutput("summary_detail")
      ),

      nav_panel("Per m\u00b3",
        div(class = "section-hdr",
            "NOK per m\u00b3 of commercial volume under bark"),
        div(class = "stat-strip",
            "Commercial volume: the 10 cm sections of all non-waste logs, each",
            " using the under-bark diameter at the top of its section. Bark,",
            " stump, waste and the top above the last merchantable log are",
            " excluded. Each species uses its own commercial volume."),
        reactableOutput("tbl_per_m3"),
        div(class = "section-hdr", "By assortment"),
        div(class = "stat-strip",
            "Contribution = assortment value \u00f7 the scope's commercial volume,",
            " so contributions add up to the scope's NOK/m\u00b3."),
        reactableOutput("tbl_per_m3_assortment"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_per_m3_contribution", height = "260px"),
          plotOutput("bar_volume_share",        height = "260px")
        ),
        apter_card("Trees", collapsible = TRUE, open = FALSE,
          div(class = "stat-strip",
              "One row per tree. Expand a row to see its logs."),
          reactableOutput("tbl_trees")
        )
      ),

      nav_panel("Products",
        div(class = "section-hdr", "Product summary"),
        reactableOutput("tbl_products"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_value_by_product",  height = "260px"),
          plotOutput("bar_volume_by_product", height = "260px")
        )
      ),


      nav_panel("Diagnostics",
        div(class = "section-hdr", "Stage timings (slowest first)"),
        reactableOutput("tbl_stage_timings"),
        div(class = "section-hdr", "Run diagnostics"),
        reactableOutput("tbl_diag"),
        div(class = "section-hdr", "Price validation"),
        reactableOutput("tbl_price_val"),
        div(class = "section-hdr", "Zero-log trees"),
        reactableOutput("tbl_zero_logs")
      ),

      nav_panel("Selected logs",
        uiOutput("selected_logs_header"),
        reactableOutput("tbl_selected_logs")
      ),

      nav_panel("Scenario comparison",
        uiOutput("cmp_status"),
        div(class = "section-hdr", "High-level comparison"),
        reactableOutput("tbl_cmp_overview"),
        div(class = "section-hdr", "Difference summary"),
        reactableOutput("tbl_cmp_delta"),
        div(class = "section-hdr", "Product-level comparison"),
        reactableOutput("tbl_cmp_products"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_cmp_value", height = "260px"),
          plotOutput("bar_cmp_delta", height = "260px")
        ),
        div(class = "section-hdr", "Comparison diagnostics"),
        reactableOutput("tbl_cmp_diag")
      )
    )
  )
)

prices_page <- nav_panel(
  "Prices and assortments",
  div(class = "apter-page",
    apter_card("Product definitions",
      div(class = "stat-strip",
          "These are the product specifications used by the optimizer.",
          " DiameterUnderBark = TRUE: product limits, price-diameter classes",
          " and price volumes use under-bark diameters (taperNOR over-bark",
          " profile minus taperNOR::barkNOR double bark, floored at 0)."),
      reactableOutput("tbl_ref_products")
    ),
    apter_card("Length and diameter classes",
      div(class = "stat-strip",
          "Note: sawlog length 31 dm is priced by interpolation",
          " (2×34dm price − 37dm price), not from the original",
          " PriceMatrixGran/PriceMatrixFuru source spreadsheet.",
          " See the Notes column below for flagged rows",
          " (PriceSource = \"Interpolated\")."),
      reactableOutput("tbl_ref_lc")
    ),
    apter_card("Default 2021 price matrix",
      uiOutput("price_matrix_header"),
      reactableOutput("tbl_ref_price")
    ),
    apter_card("Notes and limitations", collapsible = TRUE,
      tags$ul(
        tags$li("The current app uses the default 2021 price matrix."),
        tags$li("Unpriced candidates are excluded from optimization and reported in Diagnostics."),
        tags$li("31 dm sawlog prices are interpolated (2×34dm − 37dm), flagged PriceSource = \"Interpolated\" in the price table."),
        tags$li("Diameter classes above 50 cm are capped to 50 cm for price lookup only; physical diameters are preserved."),
        tags$li("Stem profiles are taperNOR over bark. Under-bark diameters subtract taperNOR::barkNOR double bark thickness, floored at 0 so the under-bark diameter never exceeds the over-bark diameter (provisional, pending the taperNOR authors)."),
        tags$li("Sawlog price volume: cylinder at the under-bark top diameter, rounded down to whole cm. Pulpwood and waste: solid volume of the stem section under bark."),
        tags$li("Results describe the selected trees only. Values are NOK per m\u00b3 of commercial volume under bark (10 cm sections of non-waste logs, each using the under-bark diameter at its top).")
      )
    )
  )
)


# ---- UI ---------------------------------------------------------------------------
ui <- page_navbar(
  id           = "main_nav",
  title        = navbar_brand(),
  window_title = "Apter — Bucking simulator (v1)",
  theme        = apter_theme(),
  fillable     = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "apter.css"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "apter-icon.svg")
  ),
  footer = app_footer(APP_VERSION),
  home_page,
  simulator_page,
  prices_page
)
