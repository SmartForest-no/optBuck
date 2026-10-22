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

# One scenario column, one connected flow (layout-and-figures): the
# assortment table (click a row to select it; the selection is shared by A
# and B), a detail panel for the selected assortment (its rules, its prices
# and its actions), then the scenario actions. s = "a" or "b". The panel's
# parts show by output.asm_state_<s>: "matrix", "flat" or "none" (not in
# this scenario: no panel; each table lists its own assortments only).
scenario_column <- function(s) {
  S <- toupper(s); O <- if (S == "A") "B" else "A"
  btn <- function(id, label, cls = "btn-secondary btn-sm") actionButton(id, label, class = cls)
  state <- function(cond) sprintf("output.asm_state_%s %s", s, cond)
  apter_card(paste("Scenario", S),
    div(class = "section-hdr", "Assortments"),
    reactableOutput(paste0("tbl_asm_", s)),

    # Shown only when the selected assortment is in this scenario.
    conditionalPanel(state("!= 'none'"), div(class = "asm-detail", id = paste0("asm_detail_", s),
      uiOutput(paste0("asm_head_", s)),
      conditionalPanel(state("== 'matrix'"),
        div(class = "pm-grid",
          div(class = "pm-axis", "Top diameter (cm) × length (dm), NOK/m³ under bark"),
          rHandsontableOutput(paste0("pm_", s)),
          uiOutput(paste0("pm_", s, "_axes")))),
      conditionalPanel(state("== 'flat'"),
        div(class = "asm-flat", id = paste0("flat_wrap_", s),
          uiOutput(paste0("flat_", s)),
          uiOutput(paste0("flat_other_", s)))),
      conditionalPanel(state("!= 'none'"),
        div(class = "pm-actions",
          btn(paste0("asm_remove_", s), "Remove"),
          btn(paste0("asm_copy_one_", s), sprintf("Make assortment equal to %s", O)),
          btn(paste0("pm_", s, "_reset"), "Reset prices"),
          downloadButton(paste0("dl_", s, "_wide"), "Prices (xlsx)", class = "btn-secondary btn-sm"),
          downloadButton(paste0("dl_", s, "_long"), "Prices (CSV)", class = "btn-secondary btn-sm")),
        fileInput(paste0("pm_", s, "_upload"), "Upload prices for this assortment (xlsx or CSV)",
                  accept = c(".csv", ".xlsx"), width = "100%"))
    )),
    uiOutput(paste0("msg_", s)),

    div(class = "section-hdr", "Scenario"),
    div(class = "pm-actions",
      btn(paste0("asm_add_", s), "Add assortment", "btn-primary btn-sm"),
      btn(paste0("asm_reset_", s), "Reset to defaults"),
      downloadButton(paste0("dl_scen_", s), "Download scenario (xlsx)", class = "btn-secondary btn-sm"),
      btn(paste0("hpr_import_", s), "Import from .hpr")),
    fileInput(paste0("scen_", s, "_upload"),
              "Upload scenario (xlsx), or a price file for all assortments (xlsx or CSV)",
              accept = c(".xlsx", ".csv"), width = "100%")
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
               c("NFI data" = "nfi", "Uploaded tree list" = "list", ".hpr stems" = "hpr"),
               selected = "nfi"),
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
    # The one slider in the app (intended exception): altitude range, 50 m step.
    # force_edges keeps the value labels inside the slider, so "1,100 m" ends
    # at the right edge of the other inputs (ion.rangeSlider reads data-*).
    htmltools::tagQuery(
      sliderInput("hoh_range", "Altitude (m a.s.l.)",
                  min = HOH_RANGE[1L], max = HOH_RANGE[2L], value = HOH_RANGE,
                  step = HOH_STEP_M, post = " m", ticks = FALSE, width = "100%")
    )$find("input")$addAttrs(`data-force-edges` = "true")$allTags(),
    uiOutput("stratum_error")
  ),
  conditionalPanel("input.tree_source == 'list'",
    fileInput("tl_upload", "Tree list (CSV or xlsx)", accept = c(".csv", ".xlsx"),
              width = "100%"),
    div(class = "note-muted",
        "Columns: tree_id, species (spruce/pine), dbh (cm, over bark at 1.3 m); optional height (m), plot_id. ",
        downloadLink("dl_tree_template", "Download template (CSV)")),
    uiOutput("tl_validation")
  ),
  # hpr Phase B: the stems of an .hpr file (the file imported into a scenario
  # this session, or another one), with its species groups mapped.
  conditionalPanel("input.tree_source == 'hpr'",
    uiOutput("hs_file_ui"),
    uiOutput("hs_species_ui"),
    uiOutput("hs_validation")
  ),
  uiOutput("tree_counts"),

  actionButton("run_btn", "Run optimizer",
               class = "btn-primary", width = "100%")
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
        # Value by DBH class (2026-10-22): table, CSV and one figure (it
        # replaces the DBH density of the Trees card).
        div(class = "section-hdr", "By DBH class"),
        reactableOutput("tbl_dbh_class"),
        div(class = "tally-downloads",
          downloadButton("dl_dbh_class", "Download DBH table (CSV)", class = "btn-secondary btn-sm")),
        plotOutput("fig_dbh_class", height = "auto"),
        apter_card("Trees", collapsible = TRUE, open = FALSE,
          layout_columns(col_widths = breakpoints(sm = 12, lg = 6),
            plotOutput("fig_height", height = "260px")
          ),
          uiOutput("trees_preview_note"),
          reactableOutput("tbl_trees"),
          downloadButton("dl_trees", "Download all trees (CSV)", class = "btn-secondary btn-sm"),
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
        # Log tally per price matrix (stocknota, 2026-10-22): replaces the
        # logs-per-length and volume-per-diameter figures (its totals).
        div(class = "section-hdr", "Log tally"),
        div(class = "tally-controls",
          selectInput("tally_key", "Assortment", choices = NULL, width = "260px"),
          radioButtons("tally_measure", "Show", c("Logs" = "logs", "Volume (m³ u.b.)" = "volume"),
                       inline = TRUE),
          uiOutput("tally_scenario_ui")),
        uiOutput("tally_note"),
        reactableOutput("tbl_tally"),
        div(class = "tally-downloads",
          downloadButton("dl_tally_xlsx", "Download log tally (xlsx)", class = "btn-secondary btn-sm"),
          downloadButton("dl_tally_csv", "Download log tally (CSV)", class = "btn-secondary btn-sm")),
        div(class = "section-hdr", "Logs"),
        uiOutput("logs_preview_note"),
        reactableOutput("tbl_selected_logs"),
        downloadButton("dl_logs", "Download all logs (CSV)", class = "btn-secondary btn-sm")
      ),

      nav_panel("Scenario comparison", value = "comparison",
        # A and B themselves are in the Summary (tbl_cmp_overview removed 2026-10-22).
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
        # A and B per assortment are on the Products tab (bar_cmp_value removed 2026-10-22).
        div(class = "section-hdr", "Value by assortment"),
        plotOutput("bar_cmp_delta", height = "280px"),
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
    )
  ),
  prices_page,
  simulator_page,
  nav_spacer(),
  nav_item(div(class = "navbar-logo-item", navbar_logo()))
)
