# app/theme.R
#
# Purpose : Visual design tokens and the bslib (Bootstrap 5) theme.
#           Look modelled on NIBIO ForestSens (Oracle APEX Redwood style; see
#           docs/design_reference/), built from our own tokens and assets.
#           Component CSS lives in www/apter.css; keep the two in sync.


#' Design tokens shared by the theme, the CSS and the base-R plots.
APTER_TOKENS <- list(
  page_bg     = "#F9F8F6",   # warm off-white page
  topbar_bg   = "#F1EFED",
  border      = "#E4E1DD",
  ink         = "#161513",   # primary text
  ink_muted   = "#6F6A66",   # labels, captions, "DONE"
  # Palette "Forest & sand" (Phase 4c): forest green is the only strong
  # colour (logo, primary buttons, active tab, status icons).
  primary     = "#3F6E58",   # forest green primary button
  secondary   = "#EAE5DC",   # sand secondary button
  accent      = "#3F6E58",   # links, focus, progress, active tab
  icon_green  = "#3F6E58",   # app icon tile
  danger      = "#B3261E",
  warning     = "#9A5B00",
  success     = "#2F6B3A"
)

#' Chart colours, "Forest & sand" (Phase 4c). Ink and grid follow the
#' reference chart chrome.
#'
#' Checked with the dataviz skill's validator in headless Chrome on the white
#' surface, all pairs (in the stem diagram any two products can touch),
#' 2026-10-13. The proposed sand #C9A97A and grey #A39E98 failed (normal-vision
#' dE 8.0 between them, floor 15), so pulpwood is a darker sand and waste a
#' lighter grey: green / sand / grey / charcoal pass CVD (worst dE 13.3,
#' protan, green vs sand) and normal vision (worst dE 16.6, grey vs sand).
#' Waste and the custom charcoal sit outside the validator's lightness band
#' and the muted hues read low-chroma by design; every log carries a text
#' label and the legend names each colour, so colour is never the only cue.
APTER_CHART <- list(
  series1  = "#3F6E58",   # single-series bars, price set A
  series2  = "#B38A52",   # price set B (A vs B: CVD dE 13.3, normal dE 19.5)
  neutral  = "#6F6A66",   # delta bars (sign is carried by position)
  ink      = "#161513",
  ink2     = "#52514e",   # value labels
  muted    = "#6F6A66",   # axis text
  grid     = "#E1E0D9",
  baseline = "#C3C2B7",
  surface  = "#FFFFFF",
  # Assortments in the stem diagram. Custom assortment classes fold into the
  # charcoal "other" fill and are named by their label.
  assortment = c(sawlog = "#3F6E58", pulpwood = "#B38A52", waste = "#C4BFB8"),
  other      = "#3A3836",
  # Log label colour per fill (light waste grey takes dark ink).
  label_on   = c(sawlog = "#FFFFFF", pulpwood = "#FFFFFF", waste = "#161513", other = "#FFFFFF")
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
    "navbar-bg"          = t$topbar_bg,
    "card-border-color"  = "rgba(0,0,0,0.06)",
    "card-cap-bg"        = "#FFFFFF",
    "input-border-color" = "#CFCAC4",
    "link-color"         = t$accent
  )
}
