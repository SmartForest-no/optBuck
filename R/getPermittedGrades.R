#' getPermittedGrades
#'
#' Extract the permitted stem grades for each assortment from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return List of permitted grades for assortments, element names correspond to product keys
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getPermittedGrades=function(XMLNode){
  require(XML)
  a=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                         xmlAttrs)) == "ProductDefinition"]
  grades=list()
  for(i in 1:length(a)){
    ProductKey<-xmlValue(a[[i]][["ProductKey"]])
    if(!is.null(a[[i]][["ClassifiedProductDefinition"]][["PermittedGradesDefinition"]])){
      defs=xmlToList(a[[i]][["ClassifiedProductDefinition"]][["PermittedGradesDefinition"]])
      m=1
      idx=rownames(summary(defs))%in%"PermittedGradeNumber"
      grades[[i]]=unlist(defs[idx]) %>%
        unname() %>% as.integer()
      names(grades)[i]=ProductKey
    }
  }
  waste=c(unlist(grades)%>%unique(),-1)
  grades=append(grades,list('999999'=waste))#waste
  return(grades[lapply(grades,length)>0])
}
