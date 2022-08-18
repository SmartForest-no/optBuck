#' getLengthClasses
#'
#' Extract the length classes for each assortment from .hpr files, needed for volume calculation when VolumeLengthCategory=="Length as defined in LengthClasses"
#'
#' @param XMLNode Output of getXMLNode()
#' @return List of length classes for assortments, element names correspond to product keys
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getLengthClasses=function(XMLNode){
  require(XML)
  b=names(xmlSApply(XMLNode[["Machine"]], xmlAttrs)) == "ProductDefinition"
  a=XMLNode[["Machine"]][b]
  LengthClasses=list()
  i=1
  for(i in 1:length(a)){
    ProductKey=as.numeric(xmlValue(a[[i]][["ProductKey"]]))
    LengthClass=a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]]
    if(!is.null(LengthClass)){
      LengthClass=ldply(xmlToList(LengthClass), data.frame)
      LengthClass=LengthClass$LengthClassLowerLimit %>% as.character() %>% as.numeric()
      LengthClass=LengthClass[!is.na(LengthClass)]
      LengthClasses[[i]]=LengthClass
      names(LengthClasses)[i]=ProductKey
    }
  }
  LengthClasses[sapply(LengthClasses, is.null)] <- NULL
  return(LengthClasses)
}
