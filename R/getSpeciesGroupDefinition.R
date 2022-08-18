#' getSpeciesGroupDefinition
#'
#' Extract information on species groups from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return List of species group information, with speciesgroupkey as the name of the elements
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getSpeciesGroupDefinition=function(XMLNode){
  require(XML)
  a=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                         xmlAttrs)) == "SpeciesGroupDefinition"]
  SpeciesGroupDefinition=list()
  i=1
  for(i in 1:length(a)){
    SpeciesGroupKey =as.integer(xmlValue(a[[i]][["SpeciesGroupKey"]]))
    SpeciesGroupName=xmlValue(a[[i]][["SpeciesGroupName"]])
    BarkFunction=a[[i]][["BarkFunction"]]
    ButtEndProfileExtrapolation=a[[i]][["ButtEndProfileExtrapolation"]]
    SpeciesGroupDefinition[[i]]=list(SpeciesGroupKey,
                                     SpeciesGroupName,
                                     BarkFunction,
                                     ButtEndProfileExtrapolation)
    names(SpeciesGroupDefinition[[i]])=c("SpeciesGroupKey",
                                         "SpeciesGroupName",
                                         "BarkFunction",
                                         "ButtEndProfileExtrapolation")
    names(SpeciesGroupDefinition)[i]=SpeciesGroupKey
  }
  return(SpeciesGroupDefinition)
}
