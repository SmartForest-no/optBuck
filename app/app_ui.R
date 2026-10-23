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
          uiOutput(paste0("pm_", s, "_cap")),     # "≥ 36 cm: priced as 36 up to max top 60 cm"
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
      btn(paste0("scen_copy_", s), sprintf("Copy from %s", O)),
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

    # Four result tabs (results restructure, 2026-10-23): Summary, Products,
    # Trees, Logs. Comparison outputs sit in the tab of their content and
    # render only after a comparison; there is no comparison tab.
    navset_card_underline(id = "result_tabs",

      nav_panel("Summary", value = "summary",
        uiOutput("stale_notice"),
        uiOutput("summary_cards")
      ),

      nav_panel("Products", value = "products",
        # One row per assortment (assortment_summary()); after a comparison
        # value and NOK/m3 contribution A, B and change, volume share A and
        # B; the CSV has every column.
        reactableOutput("tbl_products"),
        div(class = "tally-downloads",
          downloadButton("dl_products", "Download products (CSV)", class = "btn-secondary btn-sm")),
        div(class = "section-hdr", "NOK per m³ and volume share by assortment type"),
        reactableOutput("tbl_per_m3_type"),
        plotOutput("bar_nok_contribution", height = "auto"),
        # After a comparison: value change of the shared assortments.
        uiOutput("cmp_value_ui")
      ),

      nav_panel("Trees", value = "trees",
        # DBH and height distributions side by side, then NOK/m3 by DBH class
        # (lines; 2026-10-23).
        plotOutput("fig_distributions", height = "300px"),
        plotOutput("fig_dbh_class", height = "auto"),
        div(class = "section-hdr", "Trees"),
        paged_table_ui("tbl_trees"),
        div(class = "tally-downloads",
          downloadButton("dl_trees", "Download all trees (CSV)", class = "btn-secondary btn-sm"),
          downloadButton("dl_dbh_class", "Download value by DBH class (CSV)", class = "btn-secondary btn-sm"),
          uiOutput("dl_changed_trees_ui", inline = TRUE)),
        # Click a tree in the table: its cutting pattern in A (and B).
        div(class = "section-hdr", "Cutting pattern"),
        uiOutput("stem_note"),
        plotOutput("stem_compare", height = "230px")
      ),

      nav_panel("Logs", value = "logs",
        # Log tally per price matrix (stocknota, 2026-10-22).
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
