#' getProductData
#'
#' Extract product data from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return Information on ProductKeys, ProductNames, ProductGroupName, SpeciesGroupKey, DiameterUnderBark, DiameterClassLowerLimit, DiameterClassMAX, LengthClassLowerLimit, LengthClassMAX, VolumeDiameterCategory, DiameterTopPositions
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getProductData=function(XMLNode,SpeciesGroupDefinition){
  require(XML)
  b = names(xmlSApply(XMLNode[["Machine"]], xmlAttrs)) ==
    "ProductDefinition"
  a = XMLNode[["Machine"]][b]
  ProductData = c()
  i = 2
  for (i in 1:length(a)) {
    ObjectName = xmlValue(XMLNode[["Machine"]][["ObjectDefinition"]][["ObjectName"]])
    ProductKey = as.numeric(xmlValue(a[[i]][["ProductKey"]]))
    ProductName = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["ProductName"]])
    ProductGroupName = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["ProductGroupName"]])
    SpeciesGroupKey = as.numeric(xmlValue(a[[i]][["ClassifiedProductDefinition"]][["SpeciesGroupKey"]]))
    SpeciesGroupName = NA
    DiameterClassLowerLimit = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterClass"]][["DiameterClassLowerLimit"]])
    DiameterMAXButt = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterMAXButt"]])
    if (is.na(DiameterMAXButt)) {
      DiameterMAXButt = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterClassMAX"]])
    }
    DiameterUnderBark = as.logical(xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterUnderBark"]]))
    LengthClassLowerLimit = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]][["LengthClass"]][["LengthClassLowerLimit"]])
    LengthClassMAX = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]][["LengthClassMAX"]])
    DiameterTopPositions = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterTopPosition"]])
    VolumeDiameterAdjustment = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeDiameterAdjustment"]])
    VolumeDiameterCategory = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeDiameterCategory"]])
    VolumeLengthCategory = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeLengthCategory"]])
    data = c(ObjectName, ProductKey, ProductName, ProductGroupName,
             SpeciesGroupKey, SpeciesGroupName,DiameterUnderBark, DiameterClassLowerLimit, DiameterMAXButt,
             LengthClassLowerLimit, LengthClassMAX, DiameterTopPositions, VolumeDiameterAdjustment,
             VolumeDiameterCategory, VolumeLengthCategory)
    ProductData = rbind(ProductData, data)
  }
  colnames(ProductData) = c("ObjectName", "ProductKey",
                            "ProductName", "ProductGroupName", "SpeciesGroupKey","SpeciesGroupName",
                            "DiameterUnderBark", "DiameterClassLowerLimit",
                            "DiameterClassMAX", "LengthClassLowerLimit",
                            "LengthClassMAX", "DiameterTopPositions",
                            "VolumeDiameterAdjustment", "VolumeDiameterCategory",
                            "VolumeLengthCategory")
  ProductData = as.data.frame(ProductData)
  ProductData=ProductData[!is.na(ProductData$ProductKey),]
  ProductData$ObjectName = ProductData$ObjectName %>% as.character()
  ProductData$ProductName = ProductData$ProductName %>% as.character()
  ProductData$ProductGroupName = ProductData$ProductGroupName %>%
    as.character()
  ProductData$SpeciesGroupKey = ProductData$SpeciesGroupKey %>%
    as.character() %>% as.integer()
  ProductData$DiameterUnderBark = ProductData$DiameterUnderBark %>%
    as.logical()
  ProductData$DiameterClassLowerLimit = ProductData$DiameterClassLowerLimit %>%
    as.character() %>% as.numeric()
  ProductData$DiameterClassMAX = ProductData$DiameterClassMAX %>%
    as.character() %>% as.numeric()
  ProductData$LengthClassLowerLimit = ProductData$LengthClassLowerLimit %>%
    as.character() %>% as.numeric() %>%  round_any(10)
  ProductData$LengthClassMAX = ProductData$LengthClassMAX %>%
    as.character() %>% as.numeric() %>%  round_any(10)
  ProductData$DiameterTopPositions = ProductData$DiameterTopPositions %>%
    as.character() %>% as.numeric()
  ProductData$VolumeDiameterAdjustment = ProductData$VolumeDiameterAdjustment %>%
    as.character()
  ProductData$VolumeDiameterCategory = ProductData$VolumeDiameterCategory %>%
    as.character()
  ProductData$VolumeLengthCategory = ProductData$VolumeLengthCategory %>%
    as.character()
  ProductData = ProductData[!ProductData$ProductKey == 999999,
  ]
  nSpecies = length(unique(ProductData$SpeciesGroupKey))
  Waste = data.frame(ObjectName = rep(ObjectName, nSpecies),
                     ProductKey = rep("999999", nSpecies), ProductName = rep("Waste",
                                                                             nSpecies), ProductGroupName = rep("Waste",
                                                                                                               nSpecies),
                     SpeciesGroupKey = unique(ProductData$SpeciesGroupKey),
                     SpeciesGroupName = unique(ProductData$SpeciesGroupName),
                     DiameterUnderBark = rep(TRUE, nSpecies), DiameterClassLowerLimit = rep(0,
                                                                                            nSpecies), DiameterClassMAX = rep(1000, nSpecies),
                     LengthClassLowerLimit = rep(0, nSpecies), LengthClassMAX = rep(100,
                                                                                    nSpecies), DiameterTopPositions = rep(10, nSpecies),
                     VolumeDiameterAdjustment = "Measured diameter in mm",
                     VolumeDiameterCategory = "All diameters (solid volume)",
                     VolumeLengthCategory = "Physical length cm")
  ProductData = rbind(ProductData, Waste)
  ProductData$ProductKey = ProductData$ProductKey %>% as.character() %>%
    as.integer()
  rownames(ProductData) = NULL
  #add species
  for(i in 1:nSpecies){
    Key=unique(ProductData$SpeciesGroupKey)[i]
    Name=SpeciesGroupDefinition[[which(names(SpeciesGroupDefinition)==as.character(Key))]]$SpeciesGroupName
    ProductData$SpeciesGroupName[ProductData$SpeciesGroupKey==Key]=Name
  }
  return(ProductData)
}
