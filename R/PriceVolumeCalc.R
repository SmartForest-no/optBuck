#' PriceVolumeCalc
#'
#' Calculates log price volume, i.e., the volume which is used for price calculation
#'
#' @param VolumeDiameterAdjustment Volume diameter adjustment according to stanford2010 (getProductData()).
#' @param VolumeDiameterCategory Volume calculation method according to stanford2010 (getProductData()).
#' @param VolumeLengthCategory Volume length category according to stanford2010 (getProductData()).
#' @param diameterPosition numeric vector of diameter positions (cm) of a stem profile; 0,10,...,end
#' @param DiameterValue numeric vector of corresponding diameters (mm)
#' @param StartPos Starting position of log along the stem
#' @param StopPos Ending position of log
#' @param DiameterTopPosition Position from top end of log where top diameter is measured. Cm
#' @param DiameterUnderBark Logical TRUE/FALSE
#' @param SpeciesGroupKey Species ID
#' @param SpeciesGroupDefinition List of species group information, with speciesgroupkey as the name of the elements(getSpeciesGroupDefinition())
#' @param DBH Optional, in mm (BarkFunction())
#' @param LogLength Optional, in cm (for BarkFunction())
#' @param LengthClasses List of length classes for the assortments (getLengthClasses())
#' @param ProductKey Assortment key (getProductData())
#' @return Log volume in m3
#' @seealso Buck
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
PriceVolumeCalc=function(
    VolumeDiameterAdjustment,
    VolumeDiameterCategory,
    VolumeLengthCategory,
    diameterPosition,
    DiameterValue,
    StartPos,
    StopPos,
    DiameterTopPosition,
    DiameterUnderBark=T,
    SpeciesGroupKey=NA,
    SpeciesGroupDefinition=NA,
    DBH=NA,
    LogLength=NA,
    LengthClasses=NA,
    ProductKey=NA){
  IdxStart=which(diameterPosition==round((StartPos)/10)*10)
  if(VolumeLengthCategory=="Rounded downwards to nearest dm-module"){
    IdxStop=which(diameterPosition==round_any((StopPos),10,f=floor))#rounded down
  }
  if(VolumeLengthCategory=="Length as defined in LengthClasses"){
    LengthClass=LengthClasses[which(names(LengthClasses)==ProductKey)] %>% unlist() %>% unname()
    LogLength=LengthClass[max(which(LogLength>LengthClass))] #
    StopPos=StartPos+LogLength
    StopPos=round(StopPos/10)*10#rounded
    #StopPos=round_any((StopPos),10,f=floor)
    IdxStop=which(diameterPosition==StopPos)
  }
  if(VolumeLengthCategory=="Physical length cm"){
    IdxStop=which(diameterPosition==round((StopPos)/10)*10)#rounded
    #IdxStop=52
  }
  DV=DiameterValue[IdxStart:IdxStop]
  if(VolumeDiameterAdjustment=="Measured diameter rounded downwards to cm"){
    DV=round_any(DV,10,floor)
  }
  Top_ob=DiameterValue[diameterPosition==round((StopPos-DiameterTopPosition)/10)*10]
  if(VolumeDiameterCategory=="All diameters (solid volume)"){
    if(DiameterUnderBark==T){
      #SpeciesGroup=SpeciesGroupDefinition[as.character(SpeciesGroupKey)]
      DV=BarkFunction(DV,
                      SpeciesGroupKey,
                      SpeciesGroupDefinition,
                      Top_ob,
                      DBH,
                      LogLength)
    }
    r=DV/2
    v=sum(pi*(r^2)*10)/1e+08#log volumes in m3
  }
  if(VolumeDiameterCategory=="Calculated Norwegian mid"){
    #Registered diameter (Dt) is measured in cm, 10 cm from top:
    Dt=ifelse(DiameterUnderBark==T,
              BarkFunction(Top_ob,
                           SpeciesGroupKey,
                           SpeciesGroupDefinition,
                           Top_ob,
                           DBH,
                           LogLength),
              Top_ob) %>% round()
    #To get the diameter on the middle of the log (Dmid) you use the formula:
    Dmid = Dt + (LogLength/2 * 0.1) + 0.5
    v = (((Dmid/100)*(Dmid/100)) * pi/4 * (LogLength/10) )*.001
    #hprm3price
  }
  if(VolumeDiameterCategory=="Top"){#toppmalt tommer og en avsmalning pC% 1 cm pr. meter
    if(DiameterUnderBark==T){
      Top_ub=BarkFunction(DiameterValue[IdxStop],
                          SpeciesGroupKey,
                          SpeciesGroupDefinition,
                          Top_ob=Top_ob,
                          DBH=DBH,
                          LogLength=LogLength)
      r1=Top_ub/2
      r2=(Top_ub+LogLength*.01)/2
      v=((1/3)*pi*(r1^2+r2^2+(r1*r2))*LogLength)/1e+08
    }else{
      S1=pi*((Top_ob/2)^2)
      S2=pi*(((Top_ob+LogLength*.01)/2)^2)
      v=(LogLength/3)*(S1+S2+sqrt(S1*S2))/1e+08
    }
  }
  return(v)
}
