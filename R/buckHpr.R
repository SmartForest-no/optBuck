#' buckHpr
#'
#' Optimal bucking for all stems in an HPR file using \code{buckStem()}.
#'
#' @param XMLNode Output from \code{getXMLNode()}.
#' @param PriceMatrices List of price matrices for all ProductKeys (from \code{getPriceMatrices()}).
#' @param ProductData Data frame or data.table with product definitions (from \code{getProductData()}).
#' @param StemProfile Stem profiles for all stems (from \code{getStemprofile()} / \code{getStemProfile()}).
#' @param PermittedGrades Named list where names are ProductKeys and each element defines permitted stem grades
#'   (from \code{getPermittedGrades()}).
#' @param SpeciesGroupDefinition Species group definitions (from \code{getSpeciesGroupDefinition()}).
#'
#' @return A \code{data.table} with the bucking solution for all processed stems, row-bound across stems.
#'
#' @seealso \code{\link{buckStem}}, \code{\link{getPermittedGrades}}, \code{\link{getPriceMatrices}}, \code{\link{getProductData}}
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @references Skogforsk 2011. Introduction to StanForD 2010. URL: Skogforsk. https://www.skogforsk.se/contentassets/1a68cdce4af1462ead048b7a5ef1cc06/stanford-2010-introduction-150826.pdf
#' @export
buckHpr <- function(XMLNode,
                    PriceMatrices,
                    ProductData,
                    StemProfile,
                    PermittedGrades,
                    SpeciesGroupDefinition) {

  if (!requireNamespace("XML", quietly = TRUE)) stop("Package 'XML' is required.")
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")

  ProductData <- ProductData[!is.na(ProductData$ProductName), ]

  # Stem order from XML (if available), otherwise from StemProfile
  stems_xml <- XMLNode[["Machine"]][names(XML::xmlSApply(XMLNode[["Machine"]], XML::xmlAttrs)) == "Stem"]
  if (length(stems_xml) > 0) {
    stem_keys <- vapply(
      stems_xml,
      function(s) as.integer(XML::xmlValue(s[["StemKey"]])),
      integer(1)
    )
  } else {
    stem_keys <- unique(as.integer(StemProfile$StemKey))
  }

  StemProfileDT <- data.table::as.data.table(StemProfile)
  stems_list <- split(StemProfileDT, StemProfileDT$StemKey)

  pb <- utils::txtProgressBar(min = 0, max = length(stem_keys), style = 3, width = 50, char = "=")
  on.exit(close(pb), add = TRUE)

  result <- vector("list", length(stem_keys))

  for (i in seq_along(stem_keys)) {
    SK <- stem_keys[i]
    stem <- stems_list[[as.character(SK)]]

    if (!is.null(stem) && nrow(stem) > 0) {
      sol <- tryCatch(
        buckStem(
          stem = stem,
          ProductData = ProductData,
          PriceMatrices = PriceMatrices,
          PermittedGrades = PermittedGrades,
          StemKey = as.integer(SK)
        ),
        error = function(e) {
          warning(sprintf("buckStem failed for StemKey %s: %s", SK, conditionMessage(e)), call. = FALSE)
          NULL
        }
      )
      result[[i]] <- sol
    } else {
      result[[i]] <- NULL
    }

    utils::setTxtProgressBar(pb, i)
  }

  out <- data.table::rbindlist(result, fill = TRUE)
  return(out)
}
