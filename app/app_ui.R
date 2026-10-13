# app/app_ui.R
#
# Purpose : Shiny UI definition (`ui`). Sourced by app.R after setup.R,
#           theme.R and helpers_ui.R.
#           Not named ui.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# Layout  : bslib page_navbar (Bootstrap 5) with two tabs (Phase 4c):
#           Assortments and prices (opens first) and Simulator. Styling is in
#           www/apter.css and theme.R; no inline colours here.

# ---- Assortments and prices --------------------------------------------------------

pm_side <- function(side) {
  s <- tolower(side)
  div(class = "pm-grid",
    div(class = "section-hdr", side),
    if (side == "B") uiOutput("pm_b_state_ui"),
    conditionalPanel("input.pm_id != 'flat_prices'",
      div(class = "pm-axis", "Top diameter (cm) × length (dm), NOK/m³ under bark")),
    uiOutput(paste0("pm_", s, "_note")),
    if (side == "B") conditionalPanel("output.pm_b_shown", rHandsontableOutput("pm_b"))
    else rHandsontableOutput("pm_a"),
    uiOutput(paste0("pm_", s, "_axes")),
    div(class = "pm-actions",
      actionButton(paste0("pm_", s, "_reset"), "Reset to 2021 prices", class = "btn-secondary"),
      downloadButton(paste0("dl_", s, "_wide"), "Download (xlsx)", class = "btn-secondary"),
      downloadButton(paste0("dl_", s, "_long"), "Download (CSV)", class = "btn-secondary")),
    fileInput(paste0("pm_", s, "_upload"), "Upload price matrix (xlsx or CSV, wide or long)",
              accept = c(".csv", ".xlsx", ".rds"), width = "100%")
  )
}

prices_page <- nav_panel(
  "Assortments and prices", value = "prices",
  div(class = "apter-page",
    apter_card("Assortments",
      div(class = "pm-toolbar",
        actionButton("asm_add", "Add assortment", class = "btn-primary"),
        selectInput("asm_pick", "Assortment", choices = character(0)),
        actionButton("asm_edit", "Edit", class = "btn-secondary"),
        actionButton("asm_remove", "Remove", class = "btn-secondary"),
        actionButton("asm_reset", "Reset to defaults", class = "btn-secondary")
      ),
      reactableOutput("tbl_assortments"),
      layout_columns(col_widths = c(6, 6),
        fileInput("asm_upload", "Upload assortments (xlsx downloaded here)",
                  accept = ".xlsx", width = "100%"),
        div(class = "pm-actions",
            downloadButton("dl_assortments", "Download assortments (xlsx)", class = "btn-secondary"))
      ),
      uiOutput("asm_msg")
    ),
    apter_card("Price matrices",
      div(class = "pm-toolbar",
        selectInput("pm_id", "Matrix", choices = character(0)),
        uiOutput("pm_status")
      ),
      layout_columns(col_widths = c(6, 6), pm_side("A"), pm_side("B")),
      uiOutput("pm_upload_msg")
    )
  )
)


# ---- Simulator -----------------------------------------------------------------------

simulator_sidebar <- sidebar(
  width = 320,

  div(class = "sidebar-heading", "Trees"),
  radioButtons("tree_source", "Tree source",
               c("NFI sample" = "nfi", "Uploaded tree list" = "list"), selected = "nfi"),
  conditionalPanel("input.tree_source == 'nfi'",
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
    uiOutput("stratum_error")
  ),
  conditionalPanel("input.tree_source == 'list'",
    fileInput("tl_upload", "Tree list (CSV or xlsx)", accept = c(".csv", ".xlsx"),
              width = "100%"),
    div(class = "note-muted",
        "Columns: tree_id, species (spruce/pine), dbh (cm); optional height (m), plot_id. ",
        downloadLink("dl_tree_template", "Download template (CSV)")),
    uiOutput("tl_validation")
  ),

  actionButton("run_btn", "Run optimizer",
               class = "btn-primary", width = "100%")
)

simulator_page <- nav_panel(
  "Simulator", value = "simulator",
  layout_sidebar(
    sidebar = simulator_sidebar,
    fillable = FALSE,

    uiOutput("run_status"),
    uiOutput("height_banner"),

    navset_card_underline(id = "result_tabs",

      nav_panel("Summary", value = "summary",
        uiOutput("stale_notice"),
        uiOutput("summary_cards")
      ),

      nav_panel("Per m³", value = "per_m3",
        div(class = "section-hdr", "NOK per m³ of commercial volume under bark"),
        reactableOutput("tbl_per_m3"),
        div(class = "section-hdr", "By assortment"),
        reactableOutput("tbl_per_m3_assortment"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_per_m3_contribution", height = "260px"),
          plotOutput("bar_volume_share",        height = "260px")
        ),
        apter_card("Trees", collapsible = TRUE, open = FALSE,
          reactableOutput("tbl_trees")
        )
      ),

      nav_panel("Products", value = "products",
        reactableOutput("tbl_products"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_value_by_product",  height = "260px"),
          plotOutput("bar_volume_by_product", height = "260px")
        )
      ),

      nav_panel("Selected logs", value = "logs",
        reactableOutput("tbl_selected_logs")
      ),

      nav_panel("Price matrix comparison", value = "comparison",
        div(class = "section-hdr", "A and B"),
        reactableOutput("tbl_cmp_overview"),
        div(class = "section-hdr", "Difference (B − A)"),
        reactableOutput("tbl_cmp_delta"),
        div(class = "section-hdr", "NOK per m³ and volume share by assortment"),
        reactableOutput("tbl_cmp_per_m3"),
        div(class = "section-hdr", "Cutting-pattern changes"),
        uiOutput("cmp_patterns"),
        reactableOutput("tbl_cmp_trees"),
        div(class = "section-hdr", "Example stems"),
        uiOutput("cmp_tree_ui"),
        plotOutput("stem_compare", height = "230px"),
        div(class = "section-hdr", "Value by product"),
        layout_columns(col_widths = c(6, 6),
          plotOutput("bar_cmp_value", height = "260px"),
          plotOutput("bar_cmp_delta", height = "260px")
        ),
        reactableOutput("tbl_cmp_products")
      )
    )
  )
)


# ---- UI ---------------------------------------------------------------------------
ui <- page_navbar(
  id           = "main_nav",
  title        = navbar_brand(),
  window_title = "Apter — Bucking simulator",
  theme        = apter_theme(),
  fillable     = FALSE,
  selected     = "prices",
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "apter.css"),
      tags$link(rel = "icon", type = "image/svg+xml", href = "apter-icon.svg")
    ),
    banner_strip()
  ),
  prices_page,
  simulator_page
)
