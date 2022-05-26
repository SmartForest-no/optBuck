
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

#' getProductData
#'
#' Extract product data from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return Information on ProductKeys, ProductNames, ProductGroupName, SpeciesGroupKey, DiameterUnderBark, DiameterClassLowerLimit, DiameterClassMAX, LengthClassLowerLimit, LengthClassMAX, VolumeDiameterCategory, DiameterTopPositions
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getProductData=function(XMLNode){
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
    mindiam = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterClass"]][["DiameterClassLowerLimit"]])
    maxdiam = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterMAXButt"]])
    if (is.na(maxdiam)) {
      maxdiam = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterClassMAX"]])
    }
    DiameterUnderBark = as.logical(xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterClasses"]][["DiameterUnderBark"]]))
    minleng = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]][["LengthClass"]][["LengthClassLowerLimit"]])
    maxleng = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]][["LengthClassMAX"]])
    DiameterTopPositions = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["DiameterDefinition"]][["DiameterTopPosition"]])
    VolumeDiameterAdjustment = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeDiameterAdjustment"]])
    VolumeDiameterCategory = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeDiameterCategory"]])
    VolumeLengthCategory = xmlValue(a[[i]][["ClassifiedProductDefinition"]][["PriceDefinition"]][["VolumeLengthCategory"]])
    data = c(ObjectName, ProductKey, ProductName, ProductGroupName,
             SpeciesGroupKey, SpeciesGroupName,DiameterUnderBark, mindiam, maxdiam,
             minleng, maxleng, DiameterTopPositions, VolumeDiameterAdjustment,
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
    as.character() %>% as.numeric()
  ProductData$LengthClassMAX = ProductData$LengthClassMAX %>%
    as.character() %>% as.numeric()
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

#' getPriceMatrices
#'
#' Extract product data from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return list of prices matrices for all ProductKeys. Element names are productkeys.
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getPriceMatrices=function(XMLNode){
  require(XML);require(dplyr)
  a=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                         xmlAttrs)) == "ProductDefinition"]
  productdata=c()
  price_matrices=list()
  i=1
  for(i in 1:length(a)){
    ProductKey=xmlValue(a[[i]][["ProductKey"]])
    ProductName=xmlValue(a[[i]][["ClassifiedProductDefinition"]][["ProductName"]])
    if(!is.na(ProductName)){
      matrixlist= xmlToList(a[[i]][["ClassifiedProductDefinition"]][["ProductMatrixes"]])
      l=a[[i]][["ClassifiedProductDefinition"]][["ProductMatrixes"]]
      prices=dCLL=lCLL=numeric(length(l))
      m=1
      for(m in 1:length(l)){
        Item=l[[m]] %>% xmlToList()
        prices[m]=Item$Price %>% as.numeric()
        dCLL[m]=Item$.attrs[1] %>% as.numeric()
        lCLL[m]=Item$.attrs[2] %>% as.numeric()
      }
      m=matrix(prices,
               length(unique(lCLL)),
               length(unique(dCLL)),
               byrow = F)
      colnames(m)=unique(dCLL)
      rownames(m)=unique(lCLL)
      price_matrices[[ProductKey]]=m
    }
  }
  price_matrices=append(price_matrices,
                        list('999999'=matrix(0,1,1,
                                             dimnames=list(0,0))))#waste
  return(price_matrices)
}
#' getPermittedGrades
#'
#' Extract the permitted stem grades for each assortment from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @return List of permitted grades for assortments, element names correspond to product keys
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getPermittedGrades=function(XMLNode){
  require(XML)
  a=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                         xmlAttrs)) == "ProductDefinition"]
  grades=list()
  for(i in 1:length(a)){
    ProductKey<-xmlValue(a[[i]][["ProductKey"]])
    if(!is.null(a[[i]][["ClassifiedProductDefinition"]][["PermittedGradesDefinition"]])){
      defs=xmlToList(a[[i]][["ClassifiedProductDefinition"]][["PermittedGradesDefinition"]])
      m=1
      idx=rownames(summary(defs))%in%"PermittedGradeNumber"
      grades[[i]]=unlist(defs[idx]) %>%
        unname() %>% as.integer()
      names(grades)[i]=ProductKey
    }
  }
  waste=c(unlist(grades)%>%unique(),-1)
  grades=append(grades,list('999999'=waste))#waste
  return(grades[lapply(grades,length)>0])
}
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
#' getLengthClasses
#'
#' Extract the length classes for each assortment from .hpr files, needed for volume calculation when VolumeLengthCategory=="Length as defined in LengthClasses"
#'
#' @param XMLNode Output of getXMLNode()
#' @return List of length classes for assortments, element names correspond to product keys
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getLengthClasses=function(XMLNode){
  require(XML)
  b=names(xmlSApply(XMLNode[["Machine"]], xmlAttrs)) == "ProductDefinition"
  a=XMLNode[["Machine"]][b]
  LengthClasses=list()
  i=1
  for(i in 1:length(a)){
    ProductKey=as.numeric(xmlValue(a[[i]][["ProductKey"]]))
    LengthClass=a[[i]][["ClassifiedProductDefinition"]][["LengthDefinition"]]
    if(!is.null(LengthClass)){
      LengthClass=ldply(xmlToList(LengthClass), data.frame)
      LengthClass=LengthClass$LengthClassLowerLimit %>% as.character() %>% as.numeric()
      LengthClass=LengthClass[!is.na(LengthClass)]
      LengthClasses[[i]]=LengthClass
      names(LengthClasses)[i]=ProductKey
    }
  }
  LengthClasses[sapply(LengthClasses, is.null)] <- NULL
  return(LengthClasses)
}
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
    if (D == "vasket") {
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

#' buckStem
#'
#' Optimal bucking of a tree stem
#'
#' @param diameterPosition numeric vector of diameter positions (cm) of a stem profile; 0,10,20,...
#' @param DiameterValue numeric vector of corresponding diameters (mm)
#' @param StemGrade vector of corresponding stem grades
#' @param SpeciesGroupKey Species group key for the stem
#' @param PermittedGrades list with the same length of assortments, each element containing the stemgrades allowed in each assortment (getPermittedGrades())
#' @param ProductKeys Vector of assortment keys (Integer)
#' @param LengthClassLowerLimit numeric vector of minimum log lengths corresponding to the assortments
#' @param LengthClassMAX numeric vector of maximum log lengths corresponding to the assortments
#' @param DiameterClassLowerLimit numeric vector of minimum log diameters corresponding to the assortments
#' @param DiameterClassMAX numeric vector of maximum log diameters corresponding to the assortments
#' @param VolumeDiameterCategory vector (character) of Stanford 2010 volume measurement methods corresponding to the assortments. Alternatives are "All diameters (solid volume)", "Calculated Norwegian mid" and "Top".
#' @param PriceMatrices list of prices matrices for all ProductKeys (getPriceMatrices())
#' @param LengthClasses list of log length classes for all ProductKeys (getLengthClasses())
#' @return result structure with optimum bucking solution
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @references Skogforsk 2011. Introduction to StanForD 2010. URL: Skogforsk. https://www.skogforsk.se/contentassets/1a68cdce4af1462ead048b7a5ef1cc06/stanford-2010-introduction-150826.pdf
#' @export
buckStem=function (diameterPosition, DiameterValue, StemGrade, DBH, SpeciesGroupKey,
                   ProductData, ProductKeys, LengthClassLowerLimit, LengthClassMAX,
                   DiameterClassLowerLimit, DiameterClassMAX, VolumeDiameterCategory,
                   PermittedGrades, PriceMatrices, LengthClasses)
{
  require(magrittr)
  require(data.table)
  grdFinder = function(x) {
    unique(StemGrade[idxstart:x])
  }
  asoFinder = function(x) {
    names(SGKG)[sapply(SGKG, function(y) all(grd[[x]] %in%
                                               y))]
  }
  DiameterValueFinder = function(x) {
    DiameterValue[vec[[x]]]
  }
  Rounder = function(x) {
    res = round_any(DV[idx][[x]], 10, floor)
    if (sum(idx) > 1) {
      res
    }
    else {
      list(res)
    }
  }
  BarkFinder = function(x) {
    BarkFunction(DV[idx][[x]], SpeciesGroupKey, SpeciesGroupDefinition,
                 Top_ob = tab[idx, ][x]$Top_ob, DBH = DBH, LogLength = tab[idx,
                 ][x]$LogLength)
  }
  rowFinder = function(x) sum(commercial$LogLength[x] >= rownames[[x]] %>%
                                as.numeric())
  colFinder = function(x) sum(commercial$topdiam[x] >= colnames[[x]] %>%
                                as.numeric())
  priceFinder = function(x) lis[[x]][row[x], col[x]]
  seqVectozied = Vectorize(seq.default, vectorize.args = c("from",
                                                           "to"))
  SeqStart = min(LengthClassLowerLimit[LengthClassLowerLimit >
                                         0])
  SeqStop = ifelse(max(LengthClassMAX) < max(diameterPosition),
                   max(LengthClassMAX), max(diameterPosition))
  DiameterTopPositions = ProductData$DiameterTopPositions
  bult = seq(10, 100, 10)
  SeqAsp = seq(SeqStart, SeqStop, 10)
  if (SeqStart < SeqStop) {
    res = data.table()
    res[, `:=`(StartPos = -1, StopPos = 0, Top_ub = NA,
               LogLength = NA, ProductKey = NA, Volume = 0, Value = 0,
               Acc_Value = 0)]
    StartPos = 0
    while (StartPos < max(diameterPosition) - min(LengthClassLowerLimit[LengthClassLowerLimit >
                                                                        0])) {
      StartPos = sort(res$StopPos[!res$StopPos %in% res$StartPos])[1]
      if (StartPos == 0) {
        StopPos = StartPos + c(bult, SeqAsp)
      }else{
        StopPos = StartPos + SeqAsp
      }
      StopPos = StopPos[StopPos <= max(diameterPosition)&StopPos > 0]
      if (length(StopPos) < 1) {
        break
      }
      LogLength = StopPos - StartPos
      rotdiam = DiameterValue[which(near(diameterPosition, StartPos))]
      idxstart = which(near(diameterPosition, StartPos))
      idxstop = match(StopPos, diameterPosition)
      grd = lapply(idxstop, grdFinder)
      SGPK = ProductData$ProductKey[ProductData$SpeciesGroupKey ==
                                      SpeciesGroupKey[1]]
      m = data.table(idxstart, idxstop, StartPos, StopPos,
                     LogLength, rotdiam)
      m = m[, lapply(.SD, as.numeric)]
      m = m[m$StopPos <= max(diameterPosition), ]
      SGKG = PermittedGrades[as.character(SGPK)]
      asos = lapply(1:length(grd), asoFinder)
      m$Price = 0
      r = rep(idxstop, len = sum(lengths(asos)))
      r = r[order(r)]
      tab = data.table(idxstop = r, ProductKey = unlist(asos))
      idx = match(tab$ProductKey, ProductData$ProductKey)
      tab[, `:=`(DiameterUnderBark = ProductData$DiameterUnderBark[idx],
                 LengthClassLowerLimit = ProductData$LengthClassLowerLimit[idx],
                 LengthClassMAX = ProductData$LengthClassMAX[idx],
                 DiameterClassLowerLimit = ProductData$DiameterClassLowerLimit[idx],
                 DiameterClassMAX = ProductData$DiameterClassMAX[idx],
                 VolumeDiameterAdjustment = ProductData$VolumeDiameterAdjustment[idx],
                 VolumeDiameterCategory = ProductData$VolumeDiameterCategory[idx],
                 VolumeLengthCategory = ProductData$VolumeLengthCategory[idx],
                 DiameterTopPosition = as.numeric(ProductData$DiameterTopPositions[idx]))]
      tab = merge(m, tab, "idxstop")
      tab$StopPosAdj = round((tab$StopPos - tab$DiameterTopPosition)/10) *
        10
      tab$Top_ob = DiameterValue[match(tab$StopPosAdj,
                                       diameterPosition)]
      tab$Top_ub = BarkFunction(tab$Top_ob, SpeciesGroupKey,
                                SpeciesGroupDefinition, Top_ob = Top_ob, DBH = DBH,
                                LogLength = LogLength)
      tab$topdiam = ifelse(tab$DiameterUnderBark, tab$Top_ub,
                           tab$Top_ob)
      check = tab[tab$ProductKey == "8019" & tab$LogLength ==
                    440, ]
      tab = tab[tab$LogLength >= tab$LengthClassLowerLimit]
      tab = tab[tab$LogLength <= tab$LengthClassMAX]
      tab = tab[tab$topdiam > tab$DiameterClassLowerLimit]
      tab = tab[tab$rotdiam < tab$DiameterClassMAX]
      if (nrow(tab) > 0) {
        commercial = tab[tab$ProductKey != "999999"]
        if (nrow(commercial) > 0) {
          lis = PriceMatrices[commercial$ProductKey]
          rownames = lapply(lis, rownames)
          colnames = lapply(lis, colnames)
          row = sapply(1:length(commercial$LogLength),
                       rowFinder)
          col = sapply(1:length(commercial$topdiam),
                       colFinder)
          tab$Price[tab$ProductKey != "999999"] = sapply(1:length(lis),
                                                         priceFinder)
        }
        head(tab)
        tab$idxstop[tab$VolumeLengthCategory == "Rounded downwards to nearest dm-module"] = match(round_any((tab$StopPos[tab$VolumeLengthCategory ==
                                                                                                                           "Rounded downwards to nearest dm-module"]),
                                                                                                            10, f = floor), diameterPosition)
        WithLengthClass = tab[tab$VolumeLengthCategory ==
                                "Length as defined in LengthClasses" &
                                tab$ProductKey != "999999", ]
        lis = LengthClasses[WithLengthClass$ProductKey]
        WithLengthClass$LogLength
        if (nrow(WithLengthClass) > 0) {
          l = 1
          for (l in 1:nrow(WithLengthClass)) {
            LengthClass = LengthClasses[[WithLengthClass$ProductKey[l]]]
            WithLengthClass$LogLength[l] = LengthClass[max(which(WithLengthClass$LogLength[l] >=
                                                                   LengthClass))]
            WithLengthClass$StopPos[l] = WithLengthClass$StartPos[l] +
              WithLengthClass$LogLength[l]
            WithLengthClass$idxstop[l] = which(diameterPosition ==
                                                 paste(WithLengthClass$StopPos[l]))
          }
          tab$LogLength[tab$VolumeLengthCategory == "Length as defined in LengthClasses"] = WithLengthClass$LogLength
          tab$StopPos[tab$VolumeLengthCategory == "Length as defined in LengthClasses"] = WithLengthClass$StopPos
          tab$idxstop[tab$VolumeLengthCategory == "Length as defined in LengthClasses"] = WithLengthClass$idxstop
        }
        vec = seqVectozied(from = tab$idxstart, to = tab$idxstop,
                           by = 1)
        DV = sapply(1:length(vec), DiameterValueFinder)
        idx = tab$VolumeDiameterAdjustment == "Measured diameter rounded downwards to cm"
        if (sum(idx) > 0) {
          DV[idx] = sapply(1:length(DV[idx]), Rounder)
        }
        idx = tab$DiameterUnderBark == T
        if (sum(idx) > 0) {
          DV[idx] = sapply(1:length(DV[idx]), BarkFinder)
        }
        RV = relist(unlist(DV)/2, skeleton = as.relistable(DV))
        if (!is.list(RV)) {
          RV = list(RV)
        }
        tab$Volume = -1
        idx = tab$VolumeDiameterCategory == "All diameters (solid volume)"
        x = 2
        tab$Volume[idx] = sapply(1:length(RV), function(x) sum(pi *
                                                                 (unlist(RV[x])^2) * 10)/1e+08)[idx]
        idx = tab$VolumeDiameterCategory == "Calculated Norwegian mid"
        Dmid = tab$Top_ub + (tab$LogLength/2 * 0.1) +
          0.5
        tab$Volume[idx] = ((((Dmid/100) * (Dmid/100)) *
                              pi/4 * (tab$LogLength/10)) * 0.001)[idx]
        idx = tab$VolumeDiameterCategory == "Top" &
          tab$DiameterUnderBark == T
        r1 = tab$Top_ub/2
        r2 = (tab$Top_ub + tab$LogLength * 0.01)/2
        tab$Volume[idx] = (((1/3) * pi * (r1^2 + r2^2 +
                                            (r1 * r2)) * tab$LogLength)/1e+08)[idx]
        idx = tab$VolumeDiameterCategory == "Top" &
          tab$DiameterUnderBark == F
        r1 = tab$Top_ob/2
        r2 = (tab$Top_ob + tab$LogLength * 0.01)/2
        tab$Volume[idx] = (((1/3) * pi * (r1^2 + r2^2 +
                                            (r1 * r2)) * tab$LogLength)/1e+08)[idx]
        tab$Value = tab$Volume * tab$Price
        head(tab)
        m = tab[, c("StartPos", "StopPos",
                    "Top_ub", "LogLength", "ProductKey",
                    "Volume", "Value")]
        sub = res[res$StopPos == paste(tab$StartPos[1])]
        sub = sub[which.max(sub$Acc_Value), ]
        m$Acc_Value = m$Value + sub$Acc_Value
      }
      else {
        m = data.table(StartPos = StartPos, StopPos = StopPos,
                       Top_ub = NA, LogLength = NA, ProductKey = NA,
                       Volume = NA, Value = NA, Acc_Value = NA)
      }
      res = rbindlist(list(res, m))
    }
  }
  res = res[!is.na(res$LogLength)]
  tt = res[which.max(res$Acc_Value)]
  if (nrow(tt) == 1) {
    res = trackTrace(res, tt)
  }
  res = cbind(1:nrow(res), res)
  colnames(res)[1] = "LogKey"
  return(res)
}
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
#' getStemprofile
#'
#' Extract stem profiles from .hpr files
#'
#' @param XMLNode Output of getXMLNode()
#' @param Logs Harvested logs (getLogs())
#' @return Stem profiles of harvested stems with stem grades
#' @seealso buckStem
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getStemprofile=function(XMLNode,Logs){
  require(XML);require(data.table);require(tcltk);require(plyr)
  stems = XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                               xmlAttrs)) == "Stem"]
  StemProfile = stemGradedata = data.table()
  NoStemProfile = c()
  pb = txtProgressBar(min = 0, max = length(stems), style = 3,
                      width = 50, char = "=")
  createStemProfile=function(i){
    S = xmlValue(stems[[i]][["StemKey"]]) %>% as.numeric()
    Waste = Logs[Logs$StemKey == S & Logs$ProductKey == 999999,
    ]
    diams = stems[[i]][["SingleTreeProcessedStem"]][["StemDiameters"]]
    if (is.null(diams)) {
      NoStemProfile = c(NoStemProfile, xmlValue(stems[[i]][["StemKey"]]))
    }
    else{
      ObjectName = xmlValue(r[["Machine"]][["ObjectDefinition"]][["ObjectName"]])
      StemKey = as.integer(xmlValue(stems[[i]][["StemKey"]]))
      SpeciesGroupKey = as.integer(xmlValue(stems[[i]][["SpeciesGroupKey"]]))
      diams = diams[names(xmlSApply(diams, xmlAttrs)) ==
                      "DiameterValue"]
      grds = stems[[i]][["SingleTreeProcessedStem"]][names(xmlSApply(stems[[i]][["SingleTreeProcessedStem"]],
                                                                     xmlAttrs)) == "StemGrade"]
      stempr = matrix(NA, length(diams), 6) %>% as.data.frame()
      grades = startpos = c()
      for (g in 1:length(grds)) {
        grades = c(grades, xmlValue(grds[[g]]))
        startpos = c(startpos, unlist(xmlNode(grds[[g]]))[3] %>%
                       unname() %>% as.numeric())
      }
      for (j in 1:length(diams)) {
        stempr[j, 1] = ObjectName
        stempr[j, 2] = StemKey
        stempr[j, 3] = SpeciesGroupKey
        stempr[j, 4] = xmlAttrs(diams[[j]])[1] %>% unname() %>%
          as.numeric()
        stempr[j, 5] = as.numeric(xmlValue(diams[[j]]))
        stempr[j, 6] = grades[findInterval(stempr[j,
                                                  4], startpos)]
      }
      if (nrow(Waste) > 0) {
        StartWaste = EndWaste = c()
        w = 1
        for (w in 1:nrow(Waste)) {
          StartWaste[w] = Waste$StartPos[w] %>% round_any(10,
                                                          f = ceiling) + 10
          EndWaste[w] = StartWaste[w] + Waste$LogLength[w] %>%
            round_any(10, f = floor) - 10
          stempr$V6[which(stempr$V4 == StartWaste[w]):which(stempr$V4 ==
                                                              EndWaste[w])] = "-1"
        }
      }
      StemProfile = rbindlist(list(StemProfile, stempr))
    }
    setTxtProgressBar(pb, i)
    return(StemProfile)
  }
  StemProfile=rbindlist(lapply(1:length(stems),
                               createStemProfile))
  close(pb)
  head(StemProfile)
  names(StemProfile) = c("ObjectName", "StemKey",
                         "SpeciesGroupKey", "diameterPosition", "DiameterValue",
                         "StemGrade")
  return(StemProfile)
}


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
  if(VolumeDiameterCategory=="Top"){#toppmalt tommer og en avsmalning på 1 cm pr. meter
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
BarkFunction=function(DiameterValue,SpeciesGroupKey,SpeciesGroupDefinition,Top_ob,DBH,LogLength){
  #names(SpeciesGroupDefinition)
  BarkFunction=SpeciesGroupDefinition[[which(names(SpeciesGroupDefinition)==as.character(SpeciesGroupKey))]]$BarkFunction
  BarkFunction=ldply(xmlToList(BarkFunction), data.frame)
  barkFunctionCategory="Skogforsk 2004, Norway spruce"
  if(barkFunctionCategory=="Swedish Zacco"){
    a=strSplits(string, c(
      '.*<ConstantA>',
      "</ConstantA>.*")) %>% as.numeric()
    b=strSplits(string, c(
      '.*<FactorB>',
      "</FactorB>.*")) %>% as.numeric()
    Double_bark_thickness = a + b * Top_ob
  }
  if(barkFunctionCategory=="Skogforsk 2004, Scots pine"){
    lat=BarkFunction$X..i..[1] %>% as.numeric()
    dbh_b=ifelse(DBH>590,
                 590,
                 DBH) # DBH maximum 590 mm.
    htg=-log(0.12/(72.1814+0.0789*dbh_b-0.9868*lat))/(0.0078557-0.0000132*dbh_b) # Break point in cm calculated
    db=3.5808+0.0109*dbh_b+(72.1814+0.0789*dbh_b-0.9868*lat)*exp(-(0.0078557-0.0000132*dbh_b)*LogLength) # Double bark thickness below break point calculated, mm
    if(LogLength>htg){db=3.5808+0.0109*dbh_b+0.12-0.005*(LogLength-htg)}
    #Double barkthickness above break point calculated, mm
    db=ifelse( db<2,
               2,
               db) # Bark thickness minimum 2 mm
    Double_bark_thickness = db
  }
  if(barkFunctionCategory=="Skogforsk 2004, Norway spruce"){
    db=0.46146+0.01386*DBH+0.03571*DiameterValue #/* Double bark thickness calculated, mm
    db=max(db, 2) # Bark thickness minimum 2 mm
    Double_bark_thickness = db
  }
  if(barkFunctionCategory=="None"){
    Double_bark_thickness = 0
  }
  return(DiameterValue-Double_bark_thickness)
}
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
#' predictStemprofile
#'
#' Predict and extract Norway spruce stem profiles using taper models based on the log dimensions, for cases when no stem profile is recorded in the hpr file.
#'
#' @param XMLNode Output of getXMLNode()
#' @param ProductData output of getProductData()
#' @param PermittedGrades output of getPermittedGrades()
#' @return Output structure with stem profile containing stem grades
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
predictStemprofile=function(XMLNode,ProductData,PermittedGrades){
  require(XML);require(data.table);require(tcltk)
  require(TapeR);require(tidyverse)
  options(scipen=999)#suppress scientific notation
  stems=XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                       xmlAttrs)) == "Stem"]
  NoStemProfile=c()
  pb=txtProgressBar(min = 0,max = length(stems),style=3,width=50,char="=")
  result=list()
  i=1
  for(i in 1:length(stems)){#
    S=xmlValue(stems[[i]][["StemKey"]]) #%>% as.numeric()
    SpeciesGroupKey=as.integer(
      xmlValue(stems[[i]][["SpeciesGroupKey"]]))
    gran=ProductData$SpeciesGroupKey_enkel[ProductData$SpeciesGroupKey==SpeciesGroupKey]%>% unique()==1
    gran=gran[!is.na(gran)]
    gran=ifelse(length(gran)<1,F,T)
    if(gran){
      l=stems[[i]][["SingleTreeProcessedStem"]]
      l=l[which(names(l)=="Log")]
      LogKey=ProductKeys=LogLength=Buttob=
        Midob=Topob=numeric(length(l))
      j=1
      for(j in 1:length(l)){
        Item=l[[j]] %>% xmlToList()
        LogKey[j]=Item$LogKey %>% as.integer()
        ProductKeys[j]=Item$ProductKey %>% as.integer()
        LogLength[j]=Item$LogMeasurement$LogLength %>% as.integer()
        l[[j]] %>% class()
        m=l[[j]][["LogMeasurement"]]
        m=ldply(xmlToList(m), data.frame)
        m=m[,which(names(m)%in% c(".id","text",".attrs"))]
        m=m[!is.na(m$text),]
        Buttob[j]=m$text[m$.attrs=="Butt ob"]%>% as.numeric()
        Midob[j]=m$text[m$.attrs=="Mid ob"]%>% as.numeric()
        Topob[j]=m$text[m$.attrs=="Top ob"]%>% as.numeric()
      }
      Dm=rbind(Buttob,
               Midob,
               Topob) %>% as.vector()/10
      LogLengths=c(0*LogLength,
                   .5*LogLength,
                   1*LogLength)
      seqs=ProductKey=c()
      for(k in seq_len(j)){
        seqs=c(seqs,seq(k,j*3,j))
        ProductKey=c(ProductKey,rep(ProductKeys[k],3))
      }
      LogLengths=LogLengths[seqs]
      l=cbind(LogLengths,Dm,ProductKey)
      Hm=Reduce("+",l[,1],accumulate=T)/100
      l=cbind(l,Hm) %>% data.table() %>% tibble()
      l=tibble(l)
      lm=l[!duplicated(Dm),]
      lm=lm[lm$Hm>=0.5,]
      if(nrow(lm)>2){
        # funksjon fra taperNO
        mHt=hfromd(d = lm$Dm,
                   h = lm$Hm,
                   sp="spruce",
                   output = "h")
        if(length(mHt[[1]][1])>0&(!is.na(mHt[[1]][1]))){
          diameterPosition=seq(0,max(l$Hm),.1)
          DiameterValue=kublin_no(Hx = diameterPosition,
                                  Hm = lm$Hm,
                                  Dm = lm$Dm,
                                  mHt = mHt[[1]][1],
                                  sp = 1)
          DiameterValue=sort(DiameterValue$DHx,decreasing = T)
          df=data.frame(d=DiameterValue,h=diameterPosition)
          cat(plot(lm$Hm,lm$Dm,xlab = "height (m)",ylab="diameter (cm)"))
          cat(points(df$h,df$d,type="l"))
          StemGrade=rep(NA,length(diameterPosition))
          l
          keys=c()
          for(k in 1:nrow(l)){
            key=l$ProductKey[k]
            if(!key%in%keys[length(keys)]){
              keys=c(keys,key)
            }
          }
          k=1
          for(k in  1:length( keys)){
            min=min(l$Hm[l$ProductKey==keys[k]])
            max=max(l$Hm[l$ProductKey==keys[k]])
            idxmin=which(near(diameterPosition,round_any(min,.1)))
            idxmax=which(near(diameterPosition,round_any(max,.1,f = floor)))
            if(!length(idxmin)==0&!length(idxmax)==0){
              grade=PermittedGrades[[as.character(keys[k])]] %>% max()
              StemGrade[idxmin:idxmax]=grade
            }
          }
          stempr=cbind(S,SpeciesGroupKey,
                       diameterPosition,
                       DiameterValue,StemGrade) %>% data.table()
          if(is.na(stempr$StemGrade[nrow(stempr)])){
            stempr=stempr[-nrow(stempr),]
          }
          colnames(stempr)=c("StemKey",
                             "SpeciesGroupKey",
                             "diameterPosition",
                             "DiameterValue","StemGrade")
          result[[i]]=stempr
        }
      }
      setTxtProgressBar(pb,i)
    }
  }
  result=rbindlist(result)
  result$diameterPosition=as.numeric(result$diameterPosition)*100
  result$DiameterValue=as.numeric(result$DiameterValue)*10
  result$StemGrade=as.integer(result$StemGrade)
  close(pb)
  return(result)
}

#' getHarvestedArea
#'
#' Extract harvested area
#'
#' @param Stems output of getStems()
#' @return Simple feature object of area around harvested trees
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @examples
#' Stems=getStems(hprfile)
#' getHarvestedArea(Stems)
#' @export
getHarvestedArea=function(Stems){
  require(sp);require(sf)
  sf=st_as_sf(as.data.frame(Stems),coords=c("Latitude","Longitude"))
  st_crs(sf)=CRS("+proj=longlat +datum=WGS84")
  sf=st_transform(sf, CRS("+init=epsg:25832"))
  sf=st_buffer(sf,5) %>% st_union()
  plot(sf,col="red") %>% print()
  return(sf)
}

#' trackTrace
#'
#' helper function for buckStem: back-track optimum bucking solution
#'
#' @param res data table of potential cuts
#' @param tt log segment which maximize cumulative value
#' @return Optimal bucking pattern
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
trackTrace=function(res, tt){
  low = min(tt[, "StartPos"])
  while (low > 0){
    id_previous = tt$Acc_Value[order(tt$StartPos)[1]]-tt$Value[order(tt$StartPos)[1]]
    sub=res[res$StopPos==low,]
    prev = sub[which(near(sub$Acc_Value,id_previous)),]
    if (!is.vector(prev)){
      prev = prev[1, ]
    }
    tt = rbind(tt, prev) #%>% unname()
    low = min(tt$StartPos)
  }
  tt = tt[nrow(tt):1, ]
  if (is.vector(tt)) {
    tt[5] = ifelse(tt[5] == 0, 999999, tt[5])
  }
  return(tt)
}
#' impute_top
#'
#' Impute unused top of stem into result matrix of buckStem (waste)
#'
#' @param tt matrix of log segments which maximize cumulative value
#' @return new matrix which includes the tree top as waste
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
impute_top=function(tt){ # impute unused top of stem (waste)
  start=tt[,2][1] %>% unname()
  stop=max(tre[,1]) %>% unname()
  if(!near(start,stop)){
    idx_bottom=which(near(tre[,1],tt[,2][1]))
    idx_top=which(near(tre[,1],max(tre[,1])))
    idx_mid=(idx_top+idx_bottom)/2
    r=(tre[,2][idx_mid])/2 #middle radius
    v=pi*(r^2)*(stop-start)/1e+05#log volumes in l
    rest=c(start,stop,0,v,0,tt[,6][1])
    rbind(rest,tt) %>% unname()
  }else{
    return(tt)
  }
}
#' plotBucking
#'
#' Plot the bucking outcome
#'
#' @param Bucking output structure of getBucking(), buckStem() or buckHpr()
#' @param StemProfile StemProfile (getStemprofile())
#' @param Key StemKey of the stem to be plotted
#' @return plot of bucking outcome
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
plotBucking=function(Res, StemProfile, Stem, ProductData){
  require(ggplot2);require(plyr);require(RColorBrewer)
  tab=Res[Res$StemKey==Stem,]
  tre=StemProfile[StemProfile$StemKey==paste(Stem),]
  h = tre$diameterPosition
  plotdf=c()
  i=1
  for (i in 1:nrow(tab)){
    log = tre[which(tre$diameterPosition == round_any(tab[, which(colnames(tab)=="StartPos")][i],10)):which(tre$diameterPosition == round_any(tab[, which(colnames(tab) =="StopPos")][i],10)),]
    log = cbind(log, unique(ProductData$ProductName[which(ProductData$ProductKey==tab$ProductKey[i])]) ) %>% as.data.frame()
    names(log)[ncol(log)]="ProductName"
    D_Bob = max(log$DiameterValue)/2
    D_Mob = median(log$DiameterValue)/2
    D_Tob = min(log$DiameterValue)/2
    H_B = min(log$diameterPosition)
    H_M = median(log$diameterPosition)
    H_T = max(log$diameterPosition)
    log=data.frame(log=i,
                   diam=c(D_Bob, D_Mob, D_Tob, -D_Tob, -D_Mob, -D_Bob, D_Bob),
                   diameterPosition=c(H_B, H_M, H_T, H_T, H_M, H_B, H_B),
                   ProductName=unique(log$ProductName))
    plotdf=rbind(plotdf,log)
  }
  ProductData$ProductName=factor(
    ProductData$ProductName, levels=unique(ProductData$ProductName))
  plotdf$ProductName=factor(plotdf$ProductName,
                            levels=unique(ProductData$ProductName))
  colors=brewer.pal(length(unique(ProductData$ProductName)),"Spectral")
  colors=colors[unique(ProductData$ProductName)%in%unique(plotdf$ProductName)]
  ticks=seq(0,round_any(max(plotdf$diameterPosition),100),by=200)
  lim=c(0,round_any(max(tre$diameterPosition),200,f = ceiling))
  plot=ggplot(plotdf, aes(x=diam,y=diameterPosition,group=log)) +
    geom_polygon(aes(fill = ProductName),color="black")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="bottom",
          axis.text.y=element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          aspect.ratio = .1)+
    scale_y_continuous(limits=lim,breaks = ticks)+
    xlab("")+
    ylab("Diameter position (cm)")+
    scale_fill_manual(values=colors)+
    coord_flip()
  plot
  return(plot)
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
strSplits=function(x,splits){
  for(split in splits){
    x=unlist(strsplit(x, split))
  }
  return(unlist(x[!x ==""]))
}
#' getSortimentOverview
#'
#' Extract distribution of harvested volume over assortments
#'
#' @param Logs otput from getLogs
#' @param ProductData output from getProductData
#' @return figure in viewer
#' @seealso getLogs, getProductData
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
getSortimentOverview=function(Logs,ProductData){
  require(ggplot2)
  res=aggregate(Logs$m3sob,by=list(Logs$ProductKey),sum)
  names(res)=c("ProductKey","m3sob")
  ProductData=ProductData[ProductData$ProductKey%in%res$ProductKey,]
  res=merge(res,ProductData,by="ProductKey")
  res=res[,c("m3sob","ProductName")] %>% distinct()
  res$m3sob=res$m3sob %>% round(digits=2)
  res$ProductName[is.na(res$ProductName)]="Waste"
  p=ggplot(res, aes(m3sob, ProductName)) +
    geom_col()
  print(p)
  return(res[,c(2,1)])
}

#' StemprofileIncrement
#'
#' Predict Stemprofile at another point in time given a vector of new DBHs
#'
#' @param Stemprofile Stem profiles for all stems in hprfile (getStemProfile)
#' @param DBH2 a numeric vector of new DBHs, of the same length as unique StemKeys in Stemprofile
#' @param breastheight height in cm which is considered breastheight (numeric), typically 110 or 130.
#' @return A new Stemprofile object in which the new diameters are added
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
StemprofileIncrement=function(Stemprofile,DBH2,breastheight){
  res=list()
  i=1
  for(i in 1:length(unique(Stemprofile$StemKey))){
    SK=unique(Stemprofile$StemKey)[i]
    Stem=Stemprofile[Stemprofile$StemKey==SK,]
    DBH1=Stem$DiameterValue[Stem$diameterPosition==paste(breastheight)]
    ratio=DBH2[i]/DBH1
    Stem$DiameterValue2=Stem$DiameterValue*ratio
    res[[i]]=Stem
    print(i)
  }
  res=do.call(rbind.data.frame, res)
  return(res)
}

