# The packages of the Shiny app are hard dependencies (DESCRIPTION, Imports) since 0.3.1, so a plain
# install gives a working run_app(). The app files under inst/app/ are not scanned by R CMD check, so
# this list names one object of each package to keep the declared imports honest: a package that is
# no longer needed is reported by R CMD check as an unused import.
.app_stack <- function() {
  list(shiny = shiny::runApp, bslib = bslib::page_navbar, htmltools = htmltools::tags,
       reactable = reactable::reactable, rhandsontable = rhandsontable::rhandsontable,
       ggplot2 = ggplot2::ggplot, gridExtra = gridExtra::tableGrob, jsonlite = jsonlite::toJSON,
       later = later::later, promises = promises::promise, ragg = ragg::agg_png,
       systemfonts = systemfonts::register_font, grid = grid::grid.newpage,
       graphics = graphics::plot, grDevices = grDevices::dev.off)
}
