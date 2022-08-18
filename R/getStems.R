#' getStems
#'
#' Extract information on harvested stems from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return data table with stem information
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getStems=function(XMLNode)
{
  require(XML);require(data.table);require(tcltk);require(dplyr)
  stems = XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                               xmlAttrs)) == "Stem"]
  res = data.table()
  pb=txtProgressBar(min = 0,max = length(stems),style=3,width=50,char="=")
  StemKey = SpeciesGroupKey = Date = Latitude = Longitude =
    Altitude = DBH = m3sub = m3sob = ComHeight = c()
  i = 1
  for (i in 1:length(stems)) {#
    SK = xmlValue(stems[[i]][["StemKey"]])
    SGK = as.numeric(xmlValue(stems[[i]][["SpeciesGroupKey"]]))
    D = xmlValue(stems[[i]][["HarvestDate"]])
    if (!is.na(D) & D == "vasket") {
      D = xmlValue(stems[[i]][["Extension"]][["FellCutStartTime"]])
    }
    D=substr(D,1,10)
    coord = stems[[i]][names(xmlSApply(stems[[i]], xmlAttrs)) ==
                         "StemCoordinates"][2]
    Lat = coord[["StemCoordinates"]][["Latitude"]] %>%
      xmlValue() %>% as.numeric()
    Lon = coord[["StemCoordinates"]][["Longitude"]] %>%
      xmlValue() %>% as.numeric()
    Alt = as.numeric(xmlValue(stems[[i]][["StemCoordinates"]][["Altitude"]]))
    dbh = xmlValue(stems[[i]][["SingleTreeProcessedStem"]][["DBH"]]) %>%
      as.numeric
    logs = stems[[i]][["SingleTreeProcessedStem"]]
    idx = which(names(logs) == "Log")
    if (length(idx) > 0) {
      m3subs = m3sobs = CH = numeric(length(idx))
      j=1
      for (j in 1:length(idx)) {
        log = logs[[idx[j]]]
        LogKey = as.numeric(xmlValue(log[["LogKey"]]))
        df = ldply(xmlToList(log), data.frame)
        LogVolume = df[df$.id == "LogVolume", ]
        LogVolume = LogVolume[, which(names(LogVolume) %in%
                                        c(".id", "text", ".attrs"))]
        sub = LogVolume$text[LogVolume$.attrs == "m3subEstimated" |
                               LogVolume$.attrs == "m3sub"] %>% as.numeric()
        sob = LogVolume$text[LogVolume$.attrs == "m3sobEstimated" |
                               LogVolume$.attrs == "m3sob"] %>% as.numeric()
        LogLength = as.numeric(xmlValue(log[["LogMeasurement"]][["LogLength"]]))
        m3subs[j] = sub
        m3sobs[j] = sob
        CH[j] = LogLength
      }
      StemKey = c(StemKey,SK)
      SpeciesGroupKey = c(SpeciesGroupKey,SGK)
      Date = c(Date,D)
      Latitude = c(Latitude,Lat)
      Longitude = c(Longitude,Lon)
      Altitude = c(Altitude,Alt)
      DBH = c(DBH,dbh)
      m3sub = c(m3sub,sum(m3subs))
      m3sob = c(m3sob,sum(m3sobs))
      ComHeight = c(ComHeight,sum(CH))
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  res = as.data.frame(cbind(StemKey, SpeciesGroupKey, Date,
                            Altitude, DBH, m3sub, m3sob, ComHeight))
  #res = res[!res[, 1] == 0, ]
  for(i in 4:ncol(res)){
    res[,i]=as.numeric(res[,i])
  }
  return(res)
}
