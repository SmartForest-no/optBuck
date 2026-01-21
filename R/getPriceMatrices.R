#' getPriceMatrices
#'
#' Extract product data from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return list of prices matrices for all ProductKeys. Element names are productkeys.
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getPriceMatrices <- function(XMLNode){
  require(XML); require(plyr); require(dplyr)

  a <- XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]], xmlAttrs)) == "ProductDefinition"]
  price_matrices <- list()

  for(i in seq_along(a)){
    ProductKey  <- xmlValue(a[[i]][["ProductKey"]])
    ProductName <- xmlValue(a[[i]][["ClassifiedProductDefinition"]][["ProductName"]])

    if(!is.na(ProductName)){
      l <- a[[i]][["ClassifiedProductDefinition"]][["ProductMatrixes"]]
      if(length(l) == 0) next

      dCLL  <- numeric(length(l))
      lCLL  <- numeric(length(l))
      price <- numeric(length(l))

      for(m in seq_along(l)){
        Item     <- l[[m]] %>% xmlToList()
        price[m] <- as.numeric(Item$Price)
        dCLL[m]  <- as.numeric(Item$.attrs[1])
        lCLL[m]  <- as.numeric(Item$.attrs[2])
      }

      # round length classes to dm (10 cm), then aggregate duplicates
      lCLL_r <- round_any(lCLL, 10, f = floor)

      df <- data.frame(
        lCLL = lCLL_r,
        dCLL = dCLL,
        price = price
      ) %>%
        group_by(lCLL, dCLL) %>%
        summarise(price = max(price, na.rm = TRUE), .groups = "drop")

      l_levels <- sort(unique(df$lCLL))
      d_levels <- sort(unique(df$dCLL))

      # fill full grid (missing combos become NA)
      grid <- expand.grid(lCLL = l_levels, dCLL = d_levels)
      grid <- left_join(grid, df, by = c("lCLL", "dCLL"))

      mtx <- matrix(grid$price, nrow = length(l_levels), ncol = length(d_levels), byrow = FALSE)
      rownames(mtx) <- l_levels
      colnames(mtx) <- d_levels

      price_matrices[[ProductKey]] <- mtx
    }
  }

  price_matrices <- append(price_matrices, list("999999" = matrix(0, 1, 1, dimnames = list(0, 0))))
  return(price_matrices)
}
