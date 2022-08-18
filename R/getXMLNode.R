
#' getXMLNode
#'
#' Parse the hpr file
#'
#' @param hprfile Path to hpr file
#' @return  object of class XMLNode, parsed from the hpr file
#' @references https://cran.r-project.org/web/packages/XML/XML.pdf
#' @export
getXMLNode=function(hprfile){
  require(XML)
  return(xmlRoot(xmlTreeParse(hprfile, getDTD = F)))
}
