# app/app_ui.R
#
# Purpose : Shiny UI definition (`ui`). Sourced by app.R after setup.R,
#           theme.R and helpers_ui.R.
#           Not named ui.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# Layout  : bslib page_navbar (Bootstrap 5) with two tabs: Assortments and
#           prices (opens first; Scenario A and Scenario B side by side,
#           Phase 4d) and Simulator. Styling is in www/apter.css and theme.R;
#           no inline colours here. The app shows the name "optBuck".

# ---- Assortments and prices --------------------------------------------------------

# One scenario column: its assortment table, its scenario file, and its
# price matrices. s = "a" or "b". The two scenarios are independent and
# both editable; one copy button per section copies from the other column
# ("Make assortment equal to B/A", "Make matrix equal to B/A"). A whole
# scenario is copied by downloading one side and uploading it to the other.
scenario_column <- function(s) {
  S <- toupper(s); O <- if (S == "A") "B" else "A"
  btn <- function(id, label, cls = "btn-secondary btn-sm") actionButton(id, label, class = cls)
  apter_card(paste("Scenario", S),
    div(class = "section-hdr", "Assortments"),
    div(class = "pm-toolbar",
      selectInput(paste0("asm_pick_", s), NULL, choices = character(0), width = "220px"),
      btn(paste0("asm_add_", s), "Add", "btn-primary btn-sm"),
      btn(paste0("asm_edit_", s), "Edit"),
      btn(paste0("asm_remove_", s), "Remove"),
      btn(paste0("asm_copy_one_", s), sprintf("Make assortment equal to %s", O)),
      btn(paste0("asm_reset_", s), "Reset to defaults")
    ),
    reactableOutput(paste0("tbl_asm_", s)),
    div(class = "scen-file",
      fileInput(paste0("scen_", s, "_upload"), "Upload scenario (assortments and prices, xlsx)",
                accept = ".xlsx", width = "100%"),
      downloadButton(paste0("dl_scen_", s), "Download scenario (xlsx)", class = "btn-secondary btn-sm")),

    div(class = "section-hdr", "Price matrices"),
    div(class = "pm-grid",
      div(class = "pm-toolbar",
        selectInput(paste0("pm_id_", s), "Matrix", choices = character(0), width = "220px"),
        btn(paste0("pm_", s, "_copy_one"), sprintf("Make matrix equal to %s", O))),
      conditionalPanel(sprintf("input.pm_id_%s != 'flat_prices'", s),
        div(class = "pm-axis", "Top diameter (cm) × length (dm), NOK/m³ under bark")),
      rHandsontableOutput(paste0("pm_", s)),
      uiOutput(paste0("pm_", s, "_axes")),
      div(class = "pm-actions",
        btn(paste0("pm_", s, "_reset"), "Reset prices"),
        downloadButton(paste0("dl_", s, "_wide"), "Download (xlsx)", class = "btn-secondary btn-sm"),
        downloadButton(paste0("dl_", s, "_long"), "Download (CSV)", class = "btn-secondary btn-sm")),
      fileInput(paste0("pm_", s, "_upload"), "Upload price matrix (xlsx or CSV)",
                accept = c(".csv", ".xlsx", ".rds"), width = "100%")
    ),
    uiOutput(paste0("msg_", s))
  )
}

prices_page <- nav_panel(
  "Assortments and prices", value = "prices",
  div(class = "apter-page",
    p(class = "page-intro", sprintf(paste(
      "Define two scenarios with assortments and prices. %s finds the optimal bucking",
      "of each stem under both and compares the results."), APP_DISPLAY_NAME)),
    layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                   scenario_column("a"), scenario_column("b"))
  )
)


# ---- Simulator -----------------------------------------------------------------------

simulator_sidebar <- sidebar(
  width = 320,

  div(class = "sidebar-heading", "Trees"),
  radioButtons("tree_source", "Data source",
               c("NFI data" = "nfi", "Uploaded tree list" = "list"), selected = "nfi"),
  conditionalPanel("input.tree_source == 'nfi'",
    selectInput("species_group", "Species group",
                choices = c("Spruce" = "spruce", "Pine" = "pine", "Spruce and pine" = "both"),
                selected = "spruce"),
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
               class = "btn-primary", width = "100%"),
  uiOutput("large_run_note")
)

simulator_page <- nav_panel(
  "Simulator", value = "simulator",
  layout_sidebar(
    sidebar = simulator_sidebar,
    fillable = FALSE,

    div(id = "run_status_box", uiOutput("run_status")),
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
        layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
          plotOutput("bar_per_m3_contribution", height = "260px"),
          plotOutput("bar_volume_share",        height = "260px")
        ),
        apter_card("Trees", collapsible = TRUE, open = FALSE,
          paged_table_ui("tbl_trees"),
          uiOutput("tree_logs")
        )
      ),

      nav_panel("Products", value = "products",
        reactableOutput("tbl_products"),
        layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
          plotOutput("bar_value_by_product",  height = "280px"),
          plotOutput("bar_volume_by_product", height = "280px")
        )
      ),

      nav_panel("Selected logs", value = "logs",
        uiOutput("logs_preview_note"),
        reactableOutput("tbl_selected_logs"),
        downloadButton("dl_logs", "Download all logs (CSV)", class = "btn-secondary btn-sm")
      ),

      nav_panel("Scenario comparison", value = "comparison",
        div(class = "section-hdr", "Scenario A and Scenario B"),
        reactableOutput("tbl_cmp_overview"),
        div(class = "section-hdr", "Difference (B − A)"),
        reactableOutput("tbl_cmp_delta"),
        div(class = "section-hdr", "NOK per m³ and volume share by assortment"),
        reactableOutput("tbl_cmp_per_m3"),
        div(class = "section-hdr", "Cutting-pattern changes"),
        uiOutput("cmp_patterns"),
        paged_table_ui("tbl_cmp_trees"),
        div(class = "section-hdr", "Example stems"),
        selectizeInput("cmp_tree", "Tree (changed trees first, largest value change first)",
                       choices = NULL, width = "100%"),
        plotOutput("stem_compare", height = "230px"),
        div(class = "section-hdr", "Value by assortment"),
        layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
          plotOutput("bar_cmp_value", height = "280px"),
          plotOutput("bar_cmp_delta", height = "280px")
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
  window_title = paste(APP_DISPLAY_NAME, "— Bucking simulator"),
  theme        = apter_theme(),
  fillable     = FALSE,
  selected     = "prices",
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = asset_url("apter.css")),
      tags$link(rel = "icon", type = "image/svg+xml", href = "apter-icon.svg"),
      tags$script(src = asset_url("apter.js"))
    ),
    banner_strip()
  ),
  prices_page,
  simulator_page,
  nav_spacer(),
  nav_item(div(class = "navbar-logo-item", navbar_logo()))
)
