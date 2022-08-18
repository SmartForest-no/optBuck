#' getLogs
#'
#' Extract information on harvested logs from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return data table with log information
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getLogs=function(XMLNode){
  require(XML);require(data.table);require(tcltk);require(plyr)
  stems=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                             xmlAttrs)) == "Stem"]
  res=data.table()
  pb=txtProgressBar(min = 0,max = length(stems),style=3,width=50,char="=")
  i=1
  for(i in 1:length(stems)){
    StemKey=xmlValue(stems[[i]][["StemKey"]]) %>% as.numeric()
    logs=stems[[i]][["SingleTreeProcessedStem"]]
    idx=which(names(logs)=="Log")

    if(length(idx)>0){
      StartPos=0
      for(j in 1:length(idx)){
        log=logs[[idx[j]]]
        LogKey=as.numeric(xmlValue(log[["LogKey"]]))
        ProductKey=as.numeric(xmlValue(log[["ProductKey"]]))
        df=ldply(xmlToList(log), data.frame)
        LogVolume=df[df$.id=="LogVolume",]
        LogVolume=LogVolume[,which(names(LogVolume)%in% c(".id","text",".attrs"))]
        m3sub=LogVolume$text[LogVolume$.attrs=="m3subEstimated"|LogVolume$.attrs=="m3sub"] %>% as.numeric()
        m3sob=LogVolume$text[LogVolume$.attrs=="m3sobEstimated"|LogVolume$.attrs=="m3sob"] %>% as.numeric()
        StartPos=ifelse(j==1,
                        0,
                        StartPos+LogLength)
        LogLength=as.numeric(xmlValue(log[["LogMeasurement"]][["LogLength"]]))
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
        log=data.table(StemKey,LogKey,ProductKey,
                       StartPos,LogLength,
                       m3sub,m3sob,Butt_ob,Butt_ub,
                       Mid_ob,Mid_ub,Top_ob,Top_ub)
        res=rbindlist(list(res,log))
      }
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  return(res)
}
