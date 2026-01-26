#' plotBucking
#'
#' Plot the bucking outcome
#'
#' @param Res output structure for single stem of getBucking(), buckStem() or buckHpr()
#' @param StemProfile StemProfile (getStemprofile())
#' @param StemKey StemKey of the stem to be plotted
#' @param ProductData Product definition table from getProductData(),
#'   used to map \code{ProductKey} to \code{ProductName}.
#' @return plot of bucking outcome
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
plotBucking <- function(Res, StemProfile, StemKey, ProductData) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  stopifnot(requireNamespace("RColorBrewer", quietly = TRUE))

  round_any <- function(x, accuracy, f = round) f(x / accuracy) * accuracy

  Res <- data.table::as.data.table(Res)
  StemProfile <- data.table::as.data.table(StemProfile)
  ProductData <- data.table::as.data.table(ProductData)

  sk <- as.integer(StemKey)

  tab <- Res[StemKey %in% sk]
  if (nrow(tab) == 0) stop("StemKey not found in Res.")

  tre <- StemProfile[StemKey %in% sk]
  if (nrow(tre) == 0) stop("StemKey not found in StemProfile.")

  prod_map <- unique(ProductData[, .(
    ProductKey = as.integer(ProductKey),
    ProductName = as.character(ProductName)
  )])

  tab <- merge(tab, prod_map, by = "ProductKey", all.x = TRUE)
  tab[is.na(ProductName), ProductName := "Unknown"]

  plot_list <- vector("list", nrow(tab))

  for (i in seq_len(nrow(tab))) {
    st <- round_any(as.numeric(tab$StartPos[i]), 10)
    en <- round_any(as.numeric(tab$StopPos[i]), 10)

    seg <- tre[diameterPosition >= st & diameterPosition <= en]
    if (nrow(seg) == 0) next

    D_Bob <- max(seg$DiameterValue) / 2
    D_Mob <- stats::median(seg$DiameterValue) / 2
    D_Tob <- min(seg$DiameterValue) / 2

    H_B <- min(seg$diameterPosition)
    H_M <- stats::median(seg$diameterPosition)
    H_T <- max(seg$diameterPosition)

    plot_list[[i]] <- data.table::data.table(
      log = i,
      diam = c(D_Bob, D_Mob, D_Tob, -D_Tob, -D_Mob, -D_Bob, D_Bob),
      diameterPosition = c(H_B, H_M, H_T, H_T, H_M, H_B, H_B),
      ProductName = tab$ProductName[i]
    )
  }

  plotdf <- data.table::rbindlist(plot_list, fill = TRUE)
  if (nrow(plotdf) == 0) stop("No segments could be plotted (check StartPos/StopPos vs StemProfile diameterPosition).")

  prod_levels <- unique(as.character(ProductData$ProductName))
  plotdf[, ProductName := factor(ProductName, levels = prod_levels)]

  n_prod <- length(levels(plotdf$ProductName))
  pal <- RColorBrewer::brewer.pal(min(max(n_prod, 3), 11), "Spectral")
  names(pal) <- levels(plotdf$ProductName)

  ticks <- seq(0, round_any(max(tre$diameterPosition), 100, ceiling), by = 200)
  lim <- c(0, round_any(max(tre$diameterPosition), 200, ceiling))

  stem_value <- if ("CumulativeValue" %in% names(tab)) max(tab$CumulativeValue, na.rm = TRUE) else NA_real_

  ggplot2::ggplot(plotdf, ggplot2::aes(x = diam, y = diameterPosition, group = log)) +
    ggplot2::geom_polygon(ggplot2::aes(fill = ProductName), colour = "black") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = lim, breaks = ticks) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      aspect.ratio = 0.1
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("Diameter position (cm)") +
    ggplot2::ggtitle(
      if (is.finite(stem_value)) {
        paste0("StemKey: ", sk, " | Stem value: ", round(stem_value, 2))
      } else {
        paste0("StemKey: ", sk)
      }
    )
}
