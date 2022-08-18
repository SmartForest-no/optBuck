#' buckHpr
#'
#' Calculate optimal bucking for all stems in a hpr file
#'
#' @param XMLNode ouput from getXMLNode()
#' @param PriceMatrices list of price matrices for all ProductKeys (getPriceMatrices())
#' @param ProductData Matrix containing product data (getProductData())
#' @param StemProfile Stem profiles for all stems in hprfile (getStemProfile())
#' @param PermittedGrades list with the same lenght of assortments, each element containing the stemgrades allowed in each assortment (getPermittedGrades())
#' @param SpeciesGroupDefinition getSpeciesGroupDefinition()
#' @param ... others
#' @return result structure with optimum bucking solution for the stems in the input .hpr file
#' @seealso getPermittedGrades, getPriceMatrices, getProductData
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @references Skogforsk 2011. Introduction to StanForD 2010. URL: Skogforsk. https://www.skogforsk.se/contentassets/1a68cdce4af1462ead048b7a5ef1cc06/stanford-2010-introduction-150826.pdf
#' @export
buckHpr=function(XMLNode,
                 PriceMatrices,
                 ProductData,
                 StemProfile,
                 PermittedGrades,
                 SpeciesGroupDefinition,
                 ...){
  require(XML);require(plyr)
  stems=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                             xmlAttrs)) == "Stem"]
  res=list()
  pb=txtProgressBar(min = 0,max = length(stems),style=3,width=50,char="=")
  ProductData=ProductData[!is.na(ProductData$ProductName),]
  i=14
  for(i in 1:length(stems)){#11
    StemKey=SK=as.integer(xmlValue(stems[[i]][["StemKey"]]))
    stem=StemProfile[StemProfile$StemKey==SK,]
    if(nrow(stem)>0){
      diameterPosition=as.numeric(stem$diameterPosition)
      DiameterValue=as.numeric(stem$DiameterValue)
      StemGrade=as.numeric(stem$StemGrade)
      SpeciesGroupKey=unique(stem$SpeciesGroupKey)
      PermittedGrades=PermittedGrades
      ProductKeys=ProductData$ProductKey
      LengthClassLowerLimit=as.numeric(ProductData$LengthClassLowerLimit)
      LengthClassMAX=as.numeric(ProductData$LengthClassMAX)
      DiameterClassLowerLimit=as.numeric(ProductData$DiameterClassLowerLimit)
      DiameterClassMAX=as.numeric(ProductData$DiameterClassMAX)
      VolumeDiameterCategory=ProductData$VolumeDiameterCategory
      PriceMatrices=PriceMatrices
      DiameterTopPositions=ProductData$DiameterTopPosition
      DBH=xmlValue(stems[[i]][["SingleTreeProcessedStem"]][["DBH"]]) %>% as.numeric()

      out=buckStem(diameterPosition,
                   DiameterValue,
                   StemGrade,
                   DBH,
                   SpeciesGroupKey,
                   ProductData,
                   ProductKeys,
                   LengthClassLowerLimit,
                   LengthClassMAX,
                   DiameterClassLowerLimit,
                   DiameterClassMAX,
                   VolumeDiameterCategory,
                   PermittedGrades,
                   PriceMatrices)

      out=cbind(rep(StemKey,nrow(out)),out)
      colnames(out)[1]=c("StemKey")
      res[[i]]=out
    }
    setTxtProgressBar(pb,i)
  }
  res=do.call(rbind.data.frame, res)
  close(pb)
  return(res)
}
