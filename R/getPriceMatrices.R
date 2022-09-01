#' getPriceMatrices
#'
#' Extract product data from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return list of prices matrices for all ProductKeys. Element names are productkeys.
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getPriceMatrices=function(XMLNode){
  require(XML);require(dplyr)
  a=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                         xmlAttrs)) == "ProductDefinition"]
  productdata=c()
  price_matrices=list()
  i=6
  for(i in 1:length(a)){
    ProductKey=xmlValue(a[[i]][["ProductKey"]])
    ProductName=xmlValue(a[[i]][["ClassifiedProductDefinition"]][["ProductName"]])
    if(!is.na(ProductName)){
      matrixlist= xmlToList(a[[i]][["ClassifiedProductDefinition"]][["ProductMatrixes"]])
      l=a[[i]][["ClassifiedProductDefinition"]][["ProductMatrixes"]]
      prices=dCLL=lCLL=numeric(length(l))
      m=1
      for(m in 1:length(l)){
        Item=l[[m]] %>% xmlToList()
        prices[m]=Item$Price %>% as.numeric()
        dCLL[m]=Item$.attrs[1] %>% as.numeric()
        lCLL[m]=Item$.attrs[2] %>% as.numeric() %>% round_any(10,floor)
      }
      m=matrix(prices,
               length(unique(lCLL)),
               length(unique(dCLL)),
               byrow = F)
      colnames(m)=unique(dCLL)
      rownames(m)=unique(lCLL)
      price_matrices[[ProductKey]]=m
    }
  }
  price_matrices=append(price_matrices,
                        list('999999'=matrix(0,1,1,
                                             dimnames=list(0,0))))#waste
  return(price_matrices)
}
