#' BarkFunction
#'
#' Calculates diametervalues under bark
#'
#' @param DiameterValue Numeric vector of diameters (mm)
#' @param SpeciesGroupKey Species ID (getProductData())
#' @param SpeciesGroupDefinition List of species group information, with speciesgroupkey as the name of the elements (getSpeciesGroupDefinition())
#' @param Top_ob Top diameter ober bark
#' @param DBH in mm, for  Skogforsk 2004 barkFunction categories
#' @param LogLength in cm
#' @return Log volume in m3
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
BarkFunction<-function(DiameterValue,SpeciesGroupKey,SpeciesGroupDefinition,Top_ob,DBH,LogLength){
  BarkFunction<-SpeciesGroupDefinition[[which(names(SpeciesGroupDefinition)==as.character(SpeciesGroupKey))]]$BarkFunction
  BarkFunction<-ldply(xmlToList(BarkFunction), data.frame)
  barkFunctionCategory<-"Skogforsk 2004, Norway spruce"
  if(barkFunctionCategory=="Swedish Zacco"){
    a<-strSplits(string, c(
      '.*<ConstantA>',
      "</ConstantA>.*")) %>% as.numeric()
    b<-strSplits(string, c(
      '.*<FactorB>',
      "</FactorB>.*")) %>% as.numeric()
    Double_bark_thickness <- a + b * Top_ob
  }
  if(barkFunctionCategory=="Skogforsk 2004, Scots pine"){
    lat<-BarkFunction$X..i..[1] %>% as.numeric()
    dbh_b<-ifelse(DBH>590,
                 590,
                 DBH) # DBH maximum 590 mm.
    htg<--log(0.12/(72.1814+0.0789*dbh_b-0.9868*lat))/(0.0078557-0.0000132*dbh_b) # Break point in cm calculated
    db<-3.5808+0.0109*dbh_b+(72.1814+0.0789*dbh_b-0.9868*lat)*exp(-(0.0078557-0.0000132*dbh_b)*LogLength) # Double bark thickness below break point calculated, mm
    if(LogLength>htg){db<-3.5808+0.0109*dbh_b+0.12-0.005*(LogLength-htg)}
    #Double barkthickness above break point calculated, mm
    db<-ifelse( db<2,
               2,
               db) # Bark thickness minimum 2 mm
    Double_bark_thickness <- db
  }
  if(barkFunctionCategory=="Skogforsk 2004, Norway spruce"){
    db<-0.46146+0.01386*DBH+0.03571*DiameterValue #/* Double bark thickness calculated, mm
    db<-max(db, 2) # Bark thickness minimum 2 mm
    Double_bark_thickness <- db
  }
  if(barkFunctionCategory=="None"){
    Double_bark_thickness <- 0
  }
  return(DiameterValue-Double_bark_thickness)
}

#' strSplits
#'
#' Helper function: modified strsplit() for multiple splits
#'
#' @param x character vector to split
#' @param splits vector of character patterns used to split
#' @return List of permitted grades for assortments
#' @seealso getPriceMatrices
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
strSplits<-function(x,splits){
  for(split in splits){
    x<-unlist(strsplit(x, split))
  }
  return(unlist(x[!x ==""]))
}
