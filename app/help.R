# app/help.R
#
# Purpose : The Help tab (R4 D1): how to use the app, the terms it uses and the
#           accepted file formats, as plain text. All texts are in
#           inputs/i18n/translations.csv (ids help_*, written out so audit 28
#           sees them).

help_term <- function(term, definition) p(tags$strong(term), ": ", definition)

help_page <- nav_panel(
  t_("tab_help"), value = "help",
  div(class = "apter-page help-page",
    apter_card(t_("help_h_use"),
      p(t_("help_use_1")), p(t_("help_use_2")), p(t_("help_use_3")), p(t_("help_use_4"))),
    apter_card(t_("help_h_terms"),
      help_term(t_("help_t_scenario"), t_("help_d_scenario")),
      help_term(t_("help_t_target"), t_("help_d_target")),
      help_term(t_("help_t_fg"), t_("help_d_fg")),
      help_term(t_("help_t_tuning"), t_("help_d_tuning")),
      help_term(t_("help_t_cost"), t_("help_d_cost")),
      help_term(t_("help_t_files"), t_("help_d_files")),
      help_term(t_("help_t_commercial"), t_("help_d_commercial")),
      help_term(t_("help_t_price_volume"), t_("help_d_price_volume")),
      help_term(t_("help_t_grades"), t_("help_d_grades")),
      help_term(t_("help_t_assortment"), t_("help_d_assortment")),
      help_term(t_("help_t_matrix"), t_("help_d_matrix"))),
    apter_card(t_("help_h_formats"),
      p(t_("help_f_stems")), p(t_("help_f_prices")), p(t_("help_f_none"))))
)
