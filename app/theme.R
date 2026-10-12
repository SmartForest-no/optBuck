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
  primary     = "#312D2A",   # charcoal primary button
  secondary   = "#EBE9E6",   # grey secondary button
  accent      = "#4C6B7A",   # slate status checks
  icon_green  = "#3F6E58",   # app icon tile
  danger      = "#B3261E",
  warning     = "#9A5B00",
  success     = "#2F6B3A"
)

#' Chart colours (validated with the dataviz skill's palette checker against
#' the white card surface: slot 1 blue / slot 2 orange pass CVD, normal-vision
#' and contrast checks). Ink and grid follow the reference chart chrome.
APTER_CHART <- list(
  series1  = "#2a78d6",   # single-series bars, histogram, Scenario 1
  series2  = "#eb6834",   # Scenario 2
  neutral  = "#52514e",   # delta bars (sign is carried by position)
  ink      = "#161513",
  ink2     = "#52514e",   # value labels
  muted    = "#6F6A66",   # axis text
  grid     = "#E1E0D9",
  baseline = "#C3C2B7",
  surface  = "#FFFFFF"
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
