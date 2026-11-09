# app/app_ui.R
#
# Purpose : Shiny UI definition (`ui`). Sourced by app.R after setup.R,
#           theme.R and helpers_ui.R.
#           Not named ui.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# Layout  : bslib page_navbar (Bootstrap 5) with four numbered tabs (1 Stems,
#           2 Assortment specifications and prices with Scenario A and B,
#           3 Simulator, 4 Price tuning) and Help. Styling is in www/optbuck.css and theme.R;
#           no inline colours here. The app shows the name "optBuck".
#
# Text    : every text is an id of the translation table (t_(), helpers_ui.R;
#           inputs/i18n/translations.csv). The page is built in APP_LANG; the
#           browser switches language without a reload (www/optbuck.js), and
#           the server re-renders its outputs in the chosen language.

# ---- 2 Assortment specifications and prices --------------------------------------------------------

# One scenario column, one connected flow (layout-and-figures): the
# assortment table (click a row to select it; the selection is shared by A
# and B), a detail panel for the selected assortment (its rules, its prices
# and its actions), then the scenario actions. s = "a" or "b". The panel's
# parts show by output.asm_state_<s>: "matrix", "flat" or "none" (not in
# this scenario: no panel; each table lists its own assortments only).
scenario_column <- function(s) {
  S <- toupper(s); O <- if (S == "A") "B" else "A"
  btn <- function(id, label, cls = "btn-secondary btn-sm") actionButton(id, label, class = cls)
  dl  <- function(id, label) downloadButton(id, label, class = "btn-secondary btn-sm")
  state <- function(cond) sprintf("output.asm_state_%s %s", s, cond)
  optbuck_card(uiOutput(paste0("scen_title_", s), inline = TRUE),
    div(class = "section-hdr", t_("hdr_assortments")),
    reactableOutput(paste0("tbl_asm_", s)),

    # Shown only when the selected assortment is in this scenario.
    conditionalPanel(state("!= 'none'"), div(class = "asm-detail", id = paste0("asm_detail_", s),
      uiOutput(paste0("asm_head_", s)),
      conditionalPanel(state("== 'matrix'"),
        # Prices | Target distribution (restructure R2): the same grid, two views.
        radioButtons(paste0("pm_", s, "_view"), NULL, inline = TRUE, selected = "prices",
                     choiceNames = list(t_("view_prices"), t_("view_target")),
                     choiceValues = c("prices", "target")),
        conditionalPanel(sprintf("input.pm_%s_view != 'target'", s),
          div(class = "pm-grid",
            div(class = "pm-axis", t_("pm_axis")),
            rHandsontableOutput(paste0("pm_", s)),
            uiOutput(paste0("pm_", s, "_cap")),     # "≥ 36 cm: priced as 36 up to max top 60 cm"
            uiOutput(paste0("pm_", s, "_axes")))),
        conditionalPanel(sprintf("input.pm_%s_view == 'target'", s),
          div(class = "pm-grid",
            div(class = "pm-axis", t_("pm_axis")),
            rHandsontableOutput(paste0("dt_", s)),
            div(class = "pm-axes",
              div(class = "pm-axes-row",
                  btn(paste0("dt_", s, "_even"), t_("btn_target_even")),
                  btn(paste0("dt_", s, "_long"), t_("btn_target_long")),
                  btn(paste0("dt_", s, "_clear"), t_("btn_target_clear")),
                  uiOutput(paste0("dt_", s, "_cat_ui"), inline = TRUE)),
              div(class = "pm-axes-row",
                  uiOutput(paste0("dt_", s, "_app_ui"), inline = TRUE)))))),
      conditionalPanel(state("== 'flat'"),
        div(class = "asm-flat", id = paste0("flat_wrap_", s),
          uiOutput(paste0("flat_", s)),
          uiOutput(paste0("flat_other_", s)))),
      conditionalPanel(state("!= 'none'"),
        div(class = "pm-actions",
          btn(paste0("asm_remove_", s), t_("btn_remove")),
          btn(paste0("asm_copy_one_", s), t_("btn_make_equal", O)),
          btn(paste0("pm_", s, "_reset"), t_("btn_reset_prices")),
          dl(paste0("dl_", s, "_wide"), t_("btn_prices_xlsx")),
          dl(paste0("dl_", s, "_long"), t_("btn_prices_csv"))),
        optbuck_file_input(paste0("pm_", s, "_upload"), t_("lbl_upload_asm_prices"),
                         accept = c(".csv", ".xlsx")))
    )),
    uiOutput(paste0("msg_", s)),

    div(class = "section-hdr", t_("hdr_scenario")),
    div(class = "pm-actions",
      btn(paste0("asm_add_", s), t_("btn_add_assortment"), "btn-primary btn-sm"),
      btn(paste0("asm_reset_", s), t_("btn_reset_defaults")),
      btn(paste0("scen_copy_", s), t_("btn_copy_from", O)),
      dl(paste0("dl_scen_", s), t_("btn_download_scenario")),
      dl(paste0("dl_pin_", s), t_("btn_download_pin"))),
    optbuck_file_input(paste0("scen_", s, "_upload"), t_("lbl_upload_scenario"),
                     accept = c(".xlsx", ".csv", ".hpr", ".pin", ".gz", ".zip")),

    # Stem grades (Phase 6): run options of this scenario, only for .hpr stems.
    conditionalPanel("input.tree_source == 'hpr'",
      div(class = "section-hdr", t_("hdr_stem_grades")),
      checkboxInput(paste0("grade_use_", s), t_("lbl_use_grades"), FALSE),
      uiOutput(paste0("grade_warn_", s)),
      checkboxInput(paste0("grade_cut_", s), t_("lbl_cut_at_changes"), FALSE))
  )
}

prices_page <- nav_panel(
  t_("tab_prices"), value = "prices",
  div(class = "optbuck-page",
    p(class = "page-intro", t_("page_intro", APP_DISPLAY_NAME)),
    layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                   scenario_column("a"), scenario_column("b"))
  )
)


# ---- Simulator -----------------------------------------------------------------------

# The run button and the results show only when stems are ready (R4 D1; output
# stems_ready); otherwise one message.
simulator_sidebar <- sidebar(
  width = 320,
  conditionalPanel("output.stems_ready",
    actionButton("run_btn", t_("btn_run"),
                 class = "btn-primary", width = "100%"))
)

# ---- Stems (restructure R1, 2026-10-30) -------------------------------------------------
# The stem source and its inputs live on their own tab, independent of the
# scenarios. Input and output ids are unchanged from the former Simulator
# sidebar. Right: a plain summary per species and, for .hpr stems, the
# buttons that open the import dialog of a scenario for the loaded file.
stems_inputs <- tagList(
  radioButtons("tree_source", t_("lbl_data_source"),
               choiceNames  = list(t_("src_nfi"), t_("src_list"), t_("src_hpr")),
               choiceValues = c("nfi", "list", "hpr"),
               selected = "nfi"),
  conditionalPanel("input.tree_source == 'nfi'",
    # Plain selects: their choices follow the language from the server
    # (updateSelectInput, app_server.R).
    selectInput("species_group", t_("lbl_species_group"),
                choices = species_group_choices(APP_LANG), selected = "spruce"),
    checkboxGroupInput("hkl_group", t_("lbl_maturity_class"),
                       choiceNames  = lapply(names(HKL_CHOICES), t_),
                       choiceValues = unname(HKL_CHOICES),
                       selected     = "4"),
    uiOutput("abonitet_ui"),
    selectInput("fylnr_sel", t_("lbl_county"),
                choices  = fylnr_choices(APP_LANG),
                selected = "__all__"),
    # The one slider in the app (intended exception): altitude range, 50 m step.
    # force_edges keeps the value labels inside the slider, so "1,100 m" ends
    # at the right edge of the other inputs (ion.rangeSlider reads data-*).
    htmltools::tagQuery(
      sliderInput("hoh_range", t_("lbl_altitude"),
                  min = HOH_RANGE[1L], max = HOH_RANGE[2L], value = HOH_RANGE,
                  step = HOH_STEP_M, post = " m", ticks = FALSE, width = "100%")
    )$find("input")$addAttrs(`data-force-edges` = "true")$allTags(),
    uiOutput("stratum_error"),
    actionButton("stems_use_nfi", t_("btn_use_selection"), class = "btn-primary btn-sm")
  ),
  conditionalPanel("input.tree_source == 'list'",
    optbuck_file_input("tl_upload", t_("lbl_tree_list"), accept = c(".csv", ".xlsx")),
    div(class = "note-muted",
        t_("tl_columns"), " ",
        downloadLink("dl_tree_template", t_("lnk_tree_template"))),
    uiOutput("tl_validation")
  ),
  # hpr Phase B: the stems of an .hpr file (the file imported into a scenario
  # this session, or another one), with its species groups mapped.
  conditionalPanel("input.tree_source == 'hpr'",
    uiOutput("hs_file_ui"),
    uiOutput("hs_species_ui"),
    uiOutput("hs_validation")
  ),
  uiOutput("tree_counts")
)

stems_page <- nav_panel(
  t_("tab_stems"), value = "stems",
  div(class = "optbuck-page",
    p(class = "page-intro", t_("intro_stems")),
    layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
      optbuck_card(t_("hdr_trees"), stems_inputs),
      optbuck_card(t_("hdr_stem_summary"),
        uiOutput("stem_summary"),
        plotOutput("fig_stem_dists", height = "320px", fill = FALSE),
        uiOutput("stems_use_ui"))))
)

simulator_page <- nav_panel(
  t_("tab_simulator"), value = "simulator",
  layout_sidebar(
    sidebar = simulator_sidebar,
    fillable = FALSE,

    p(class = "page-intro", t_("intro_simulator")),
    conditionalPanel("!output.stems_ready", div(class = "note-muted", t_("need_stems"))),
    div(id = "run_status_box", uiOutput("run_status")),
    uiOutput("height_banner"),

    # Four result tabs (results restructure, 2026-10-23): Summary, Products,
    # Trees, Logs. Comparison outputs sit in the tab of their content and
    # render only after a comparison; there is no comparison tab.
    conditionalPanel("output.stems_ready", navset_card_underline(id = "result_tabs",

      nav_panel(t_("tab_summary"), value = "summary",
        uiOutput("stale_notice"),
        uiOutput("summary_cards")
      ),

      nav_panel(t_("tab_products"), value = "products",
        # One row per assortment (assortment_summary()); after a comparison
        # value and NOK/m3 contribution A, B and change, volume share A and
        # B; the CSV has every column.
        reactableOutput("tbl_products"),
        div(class = "tally-downloads",
          downloadButton("dl_products", t_("btn_dl_products"), class = "btn-secondary btn-sm")),
        div(class = "section-hdr", t_("hdr_per_m3_type")),
        reactableOutput("tbl_per_m3_type"),
        plotOutput("bar_nok_contribution", height = "auto"),
        # After a comparison: value change of the shared assortments.
        uiOutput("cmp_value_ui")
      ),

      nav_panel(t_("tab_trees"), value = "trees",
        # NOK/m3 by DBH class
        # (lines; 2026-10-23).
        plotOutput("fig_dbh_class", height = "auto"),
        div(class = "section-hdr", t_("hdr_trees")),
        paged_table_ui("tbl_trees"),
        div(class = "tally-downloads",
          downloadButton("dl_trees", t_("btn_dl_trees"), class = "btn-secondary btn-sm"),
          downloadButton("dl_dbh_class", t_("btn_dl_dbh_class"), class = "btn-secondary btn-sm"),
          uiOutput("dl_changed_trees_ui", inline = TRUE)),
        # Click a tree in the table: its cutting pattern in A (and B).
        div(class = "section-hdr", t_("hdr_cutting_pattern")),
        uiOutput("stem_note"),
        plotOutput("stem_compare", height = "230px")
      ),

      nav_panel(t_("tab_logs"), value = "logs",
        # Log tally per price matrix (stocknota, 2026-10-22).
        div(class = "section-hdr", t_("hdr_log_tally")),
        div(class = "tally-controls",
          selectInput("tally_key", t_("lbl_assortment"), choices = NULL, width = "260px"),
          radioButtons("tally_measure", t_("lbl_show"),
                       choiceNames = list(t_("tally_logs"), t_("tally_volume")),
                       choiceValues = c("logs", "volume"), inline = TRUE),
          uiOutput("tally_scenario_ui")),
        uiOutput("tally_note"),
        reactableOutput("tbl_tally"),
        div(class = "tally-downloads",
          downloadButton("dl_tally_xlsx", t_("btn_dl_tally_xlsx"), class = "btn-secondary btn-sm"),
          downloadButton("dl_tally_csv", t_("btn_dl_tally_csv"), class = "btn-secondary btn-sm")),
        plotOutput("fig_log_lengths", height = "auto"),
        div(class = "section-hdr", t_("hdr_logs")),
        uiOutput("logs_preview_note"),
        reactableOutput("tbl_selected_logs"),
        downloadButton("dl_logs", t_("btn_dl_logs"), class = "btn-secondary btn-sm"),
        uiOutput("dl_hpr_ui", inline = TRUE)
      )
    ))
  )
)


# ---- UI ---------------------------------------------------------------------------
ui <- page_navbar(
  id           = "main_nav",
  title        = navbar_brand(),
  window_title = tr("window_title", APP_DISPLAY_NAME, lang = APP_LANG),
  theme        = optbuck_theme(),
  fillable     = FALSE,
  selected     = "stems",
  header = tagList(
    tags$head(
      tags$style(HTML(optbuck_css_vars())),          # the green and its shades (theme.R), used by optbuck.css
      tags$link(rel = "stylesheet", href = asset_url("optbuck.css")),
      tags$link(rel = "icon", type = "image/svg+xml", href = asset_url("optbuck-icon.svg")),
      i18n_head(),                                   # the ui texts of both languages
      tags$script(src = asset_url("optbuck.js"))
    )
  ),
  stems_page,
  prices_page,
  simulator_page,
  tuning_page,
  check_page,
  help_page,
  nav_spacer(),
  nav_item(lang_switch()),
  nav_item(div(class = "navbar-logo-item", navbar_logo()))
)