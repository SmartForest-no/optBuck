#' getBucking
#'
#' Extract bucking outcomes from a .hpr file
#'
#' @param XMLNode Output of getXMLNode()
#' @param PriceMatrices list of prices matrices for all ProductKeys (getPriceMatrices())
#' @param ProductData Matrix containing product data (getProductData())
#' @param StemProfile Stem profiles for all stems in hprfile (getStemProfile())
#' @param LengthClasses getLengthClasses()
#' @return Output structure with bucking outcomes
#' @seealso buckStem, buckHpr
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getBucking=function(XMLNode,PriceMatrices,ProductData,StemProfile,LengthClasses){
  require(XML);require(plyr);require(tcltk)
  stems=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                             xmlAttrs)) == "Stem"]
  m=c()
  StemKeys=LogKeys=StartPoss=StopPoss=LogLengths=Volumes=hprvolumes=hprm3prices=
    vol_prices=ProductKeys=Prices=Values=CumulativeValues=c()
  #for(i in 1:length(stems)){
  #  StemKey=as.integer(xmlValue(stems[[i]][["StemKey"]]))
  #
  #  if(StemKey==334020      ){print(i)}
  #}
  pb=txtProgressBar(min = 0,max = length(stems),style=3,width=50,char="=")
  i=160
  for(i in 1:length(stems)){
    StemKey=as.integer(xmlValue(stems[[i]][["StemKey"]]))
    SpeciesGroupKey=as.integer(xmlValue(stems[[i]][["SpeciesGroupKey"]]))
    logs=stems[[i]][["SingleTreeProcessedStem"]]
    if(is.null(logs)){
      logs=stems[[i]][["MultiTreeProcessedStem"]]
    }
    DBH=xmlValue(stems[[i]][["SingleTreeProcessedStem"]][["DBH"]]) %>% as.numeric
    idx=which(names(logs)=="Log")
    CumulativeValue=0
    j=2
    for(j in 1:length(idx)){
      log=logs[[idx[j]]]
      LogKey=as.numeric(xmlValue(log[["LogKey"]]))
      diameterPosition=StemProfile$diameterPosition[StemProfile$StemKey==StemKey]
      StartPos=as.numeric(xmlValue(log[["Extension"]][["StartPos"]]))
      LogLength=as.numeric(xmlValue(log[["LogMeasurement"]][["LogLength"]]))
      StopPos=StartPos+LogLength
      ProductKey=as.numeric(xmlValue(log[["ProductKey"]]))
      LengthClass=LengthClasses[names(LengthClasses)==ProductKey]
      df=ldply(xmlToList(log), data.frame)
      LogVolume=df[df$.id=="LogVolume",]
      LogVolume=LogVolume[,which(names(LogVolume)%in% c(".id","text",".attrs"))]
      m3sub=LogVolume$text[LogVolume$.attrs=="m3subEstimated"|LogVolume$.attrs=="m3sub"] %>% as.numeric()
      m3sob=LogVolume$text[LogVolume$.attrs=="m3sobEstimated"|LogVolume$.attrs=="m3sob"] %>% as.numeric()
      hprm3price=LogVolume$text[LogVolume$.attrs=="m3 (price)"|LogVolume$.attrs=="m3 (price)Estimated"] %>% as.numeric()
      LogMeasurement=log[["LogMeasurement"]]
      LogMeasurement=ldply(xmlToList(LogMeasurement), data.frame)
      LogMeasurement=LogMeasurement[,which(names(LogMeasurement)%in% c(".id","text",".attrs"))]
      LogMeasurement=LogMeasurement[!is.na(LogMeasurement$text),]
      Butt_ob=LogMeasurement$text[LogMeasurement$.attrs=="Butt ob"]%>% as.numeric()
      Butt_ub=LogMeasurement$text[LogMeasurement$.attrs=="Butt ub"]%>% as.numeric()
      Mid_ob=LogMeasurement$text[LogMeasurement$.attrs=="Mid ob"]%>% as.numeric()
      Mid_ub=LogMeasurement$text[LogMeasurement$.attrs=="Mid ub"]%>% as.numeric()
      Top_ob=LogMeasurement$text[LogMeasurement$.attrs=="Top ob"]%>% as.numeric()
      Top_ub=LogMeasurement$text[LogMeasurement$.attrs=="Top ub"]%>% as.numeric()
      if(ProductKey==999999){
        DiameterTopPosition=10
        VolumeDiameterAdjustment="Measured diameter in mm"
        VolumeDiameterCategory="All diameters (solid volume)"
        VolumeLengthCategory="Physical length cm"
        buttEndProfileExtrapolationMethod="false"
      }else{
        VolumeDiameterAdjustment=ProductData$VolumeDiameterAdjustment[ProductData$ProductKey == ProductKey]
        VolumeDiameterCategory=ProductData$VolumeDiameterCategory[ProductData$ProductKey == ProductKey]
        VolumeLengthCategory=ProductData$VolumeLengthCategory[ProductData$ProductKey == ProductKey]
        DiameterTopPosition=as.numeric(ProductData$DiameterTopPositions[ProductData$ProductKey == ProductKey])
      }

      if(length(diameterPosition)==0){
        Volume=ifelse(
          DiameterUnderBark==T,
          m3sub,
          m3sob
        )
      }else{
        IdxStart=which(diameterPosition==round((StartPos)/10)*10)
        IdxStop=which(diameterPosition==round((StopPos-DiameterTopPosition)/10)*10)
        IdxMid=(IdxStop)/2
        DiameterValue=StemProfile$DiameterValue[StemProfile$StemKey==StemKey]
        DiameterUnderBark=ifelse( ProductKey==999999,
                                  T,
                                  as.logical(ProductData$DiameterUnderBark[ProductData$ProductKey == ProductKey]))
        if(is.na(DiameterUnderBark)){#for bult
          DiameterUnderBark=T
        }
        Volume=VolumeCalc(diameterPosition,
                          DiameterValue,
                          StartPos,
                          StopPos,
                          DiameterTopPosition,
                          DiameterUnderBark,
                          SpeciesGroupKey,
                          SpeciesGroupDefinition,
                          DBH,
                          LogLength) %>% round(digits=3)
        hprvolume=ifelse(DiameterUnderBark==T,
                         m3sub,
                         m3sob)
        Volume
        hprvolume # hpr
        m3price=ifelse(ProductKey==999999 ,
                       0,
                       PriceVolumeCalc(
                         VolumeDiameterAdjustment,
                         VolumeDiameterCategory,
                         VolumeLengthCategory,
                         diameterPosition,
                         DiameterValue,
                         StartPos,
                         StopPos,
                         DiameterTopPosition,
                         DiameterUnderBark,
                         SpeciesGroupKey,
                         SpeciesGroupDefinition,
                         DBH,
                         LogLength,
                         LengthClasses,
                         ProductKey))

        m3price
        hprm3price
      }
      if(is.na(DiameterTopPosition)){#for bult
        VolumeDiameterCategory="All diameters (solid volume)"
        DiameterTopPosition=0
        Volume=ifelse(DiameterUnderBark==T,
                      m3sub,
                      m3sob)
      }
      # pricing
      row=sum(LogLength>=rownames(PriceMatrices[[
        as.character(ProductKey)]]) %>% as.numeric()) # which row in price matrix
      col=sum(Top_ob>=colnames(PriceMatrices[[
        as.character(ProductKey)]]) %>% as.numeric()) # which column
      Price=PriceMatrices[[as.character(ProductKey)]][row,col]# overwrite highest price
      if(is.null(Price)){
        Price=0
      }
      #log value
      Value=m3price*Price
      CumulativeValue=ifelse(j>1,
                             CumulativeValues[length(CumulativeValues)]+Value,
                             Value)
      StemKeys=c(StemKeys,StemKey)
      LogKeys=c(LogKeys,LogKey)
      StartPoss=c(StartPoss,StartPos)
      StopPoss=c(StopPoss,StopPos)
      ProductKeys=c(ProductKeys,ProductKey)
      LogLengths=c(LogLengths,LogLength)
      Volumes=c(Volumes,Volume)
      hprvolumes=c(hprvolumes,hprvolume)
      hprm3prices=c(hprm3prices,hprm3price)
      vol_prices=c(vol_prices,m3price)
      Prices=c(Prices,Price)
      Values=c(Values,Value)
      CumulativeValues=c(CumulativeValues,CumulativeValue)
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  m=cbind(StemKeys,StartPoss,StopPoss,
          ProductKeys,LogLengths,Volumes,
          hprvolumes,hprm3prices,vol_prices,
          Prices,Values,CumulativeValues) %>%
    as.data.frame()
  names(m)=c("StemKey","StartPos","StopPos",
             "ProductKey","LogLengths", "Volume",
             "HprVolume","hprm3prices","vol_prices",
             "Price","Value","CumulativeValue")
  return(m)
}