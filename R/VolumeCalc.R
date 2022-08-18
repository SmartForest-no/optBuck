#' VolumeCalc
#'
#' Calculates log volume from all diameters as solid volume
#'
#' @param diameterPosition numeric vector of diameter positions (cm) of a stem profile; 0,10,...,end
#' @param DiameterValue numeric vector of corresponding diameters (mm)
#' @param StartPos Starting position of log along the stem
#' @param StopPos Ending position of log
#' @param DiameterTopPosition Position from top end of log where top diameter is measured. Cm
#' @param DiameterUnderBark Logical TRUE/FALSE
#' @param SpeciesGroupKey Species ID
#' @param SpeciesGroupDefinition List of species group information, with speciesgroupkey as the name of the elements(getSpeciesGroupDefinition())
#' @param DBH Optional, in mm (for BarkFunction())
#' @param LogLength Optional, in cm (for BarkFunction())
#' @return Log volume in m3
#' @seealso Buck
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
VolumeCalc=function(
    diameterPosition,
    DiameterValue,
    StartPos,
    StopPos,
    DiameterTopPosition,
    DiameterUnderBark=T,
    SpeciesGroupKey=NA,
    SpeciesGroupDefinition=NA,
    DBH=NA,
    LogLength=NA){
  IdxStart=which(diameterPosition==round((StartPos)/10)*10)
  IdxStop=which(diameterPosition==round((StopPos)/10)*10)#rounded
  DV=DiameterValue[IdxStart:IdxStop]
  Top_ob=DiameterValue[diameterPosition==round((StopPos-DiameterTopPosition)/10)*10]
  DV=BarkFunction(DV,
                  SpeciesGroupKey,
                  SpeciesGroupDefinition,
                  Top_ob,
                  DBH,
                  LogLength)
  r=DV/2
  v=sum(pi*(r^2)*10)/1e+08#log volumes in m3
  return(v)
}
