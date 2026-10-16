# app/theme.R
#
# Purpose : Visual design tokens and the bslib (Bootstrap 5) theme.
#           Look: option 3 "Combination" of
#           docs/design_reference/optbuck_design_directions.html (2026-10-16):
#           white page and navbar, hairline rules, forest green for actions,
#           grey-green tables and summary boxes. Component CSS lives in
#           www/apter.css; keep the two in sync.


#' Design tokens shared by the theme, the CSS and the base-R plots.
APTER_TOKENS <- list(
  page_bg     = "#FFFFFF",   # page and navbar
  border      = "#E6E6E6",   # navbar rule, card borders
  ink         = "#111111",   # primary text, active navbar tab
  ink_soft    = "#1D2321",   # secondary-button text, active result tab
  ink_muted   = "#6A726E",   # grey-green: table headers, captions
  panel       = "#F7F8F7",   # table headers, summary boxes
  table_row   = "#EDF0EE",   # table row borders
  table_head  = "#E3E6E4",   # rule under table headers
  field       = "#F3F4F3",   # selects and inputs
  diff        = "#EAF3EE",   # A/B differences
  # Forest green is the only strong colour (primary buttons, focus,
  # progress, the result tabs' underline).
  primary     = "#3F6E58",
  secondary   = "#EEF0EF",   # secondary buttons
  accent      = "#3F6E58",
  icon_green  = "#3F6E58",   # app icon tile
  danger      = "#B3261E",
  warning     = "#9A5B00",
  success     = "#2F6B3A"
)

#' Chart colours (option 3 "Combination", 2026-10-16). Ink and grid follow
#' the page's grey-green chrome.
#'
#' Requested: sawlog #3F6E58, pulpwood #C9A97A, waste #B9BFBC. Checked with
#' the dataviz skill's validator (validate_palette.js) in headless Chrome via
#' chromote, on the white surface, all pairs (in the stem diagram any two
#' assortments can touch). Pulpwood and waste as requested failed the
#' normal-vision floor (dE 8.7, floor 15) and were too close for
#' colour-blind readers (CVD dE 8.2), so both were shifted slightly:
#' pulpwood #C09A66 (a little darker sand), waste #CBD0CD (a little lighter
#' grey). With custom #3A3836 the set passes CVD separation (worst dE 15.5,
#' deutan, custom vs sawlog) and the normal-vision floor (worst dE 16.5,
#' waste vs pulpwood). Scenario A/B bars (green vs sand): CVD dE 17.9,
#' normal dE 23.5. The muted colours fall below the chroma floor and waste
#' and the charcoal sit outside the lightness band by design; pulpwood
#' (2.6:1) and waste (1.6:1) are below 3:1 contrast on white, so every log
#' carries a text label, bars carry value labels and the legend names each
#' colour: colour is never the only cue.
APTER_CHART <- list(
  series1  = "#3F6E58",   # single-series bars, Scenario A
  series2  = "#C09A66",   # Scenario B
  neutral  = "#6A726E",   # delta bars (sign is carried by position)
  ink      = "#111111",
  ink2     = "#4A4F4C",   # value labels
  muted    = "#6A726E",   # axis text
  grid     = "#EDF0EE",
  baseline = "#C9CECB",
  surface  = "#FFFFFF",
  # Assortments in the stem diagram. Custom assortment classes fold into the
  # charcoal "other" fill and are named by their label.
  assortment = c(sawlog = "#3F6E58", pulpwood = "#C09A66", waste = "#CBD0CD"),
  other      = "#3A3836",
  # Log label colour per fill (dark ink on the light sand and grey).
  label_on   = c(sawlog = "#FFFFFF", pulpwood = "#111111", waste = "#111111", other = "#FFFFFF")
)


#' The app's Bootstrap 5 theme.
apter_theme <- function() {
  t <- APTER_TOKENS
  bslib::bs_theme(
    version   = 5,
    bg        = t$page_bg,
    fg        = t$ink,
    primary   = t$primary,
    secondary = t$secondary,
    success   = t$success,
    danger    = t$danger,
    base_font    = "Inter, system-ui, 'Segoe UI', sans-serif",
    heading_font = "Inter, system-ui, 'Segoe UI', sans-serif",
    font_scale   = 0.95,
    "border-radius"      = "6px",
    "border-color"       = t$border,
    "navbar-bg"          = t$page_bg,
    "card-border-color"  = t$border,
    "card-cap-bg"        = "#FFFFFF",
    "input-border-color" = t$field,
    "input-bg"           = t$field,
    "link-color"         = t$accent
  )
}
