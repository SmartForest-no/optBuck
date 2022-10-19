#' predictStemprofile
#'
#' Predict and extract Norway spruce stem profiles using taper models based on the log dimensions, for cases when no stem profile is recorded in the hpr file.
#'
#' @param XMLNode Output of getXMLNode()
#' @param ProductData output of getProductData()
#' @param PermittedGrades output of getPermittedGrades()
#' @param GradeDetermination Either "GradeValue" or "LogsProductKey". Shoud stem grade be based on StemGrade and gradeStartPosition or selected Products of logs
#' @return Output structure with stem profile containing stem grades
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @export
predictStemprofile=function(XMLNode,ProductData,PermittedGrades,GradeDetermination="GradeValue"){
  require(XML)
  require(data.table)
  require(tcltk)
  require(TapeR)
  require(taperNO)
  require(tidyverse)
  require(plyr)
  options(scipen = 999)
  stems = XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                               xmlAttrs)) == "Stem"]
  gran=unique(ProductData$SpeciesGroupKey[toupper(ProductData$SpeciesGroupName) %in%
                                            c("GRAN","SPRUCE")])
  if(length(gran)<1){stop("SpeciesGroupKey for Norway spruce does not seem to be specified in ProductData")}
  NoStemProfile = c()
  pb = txtProgressBar(min = 0, max = length(stems), style = 3,
                      width = 50, char = "=")
  result = list()

  i = 4
  for (i in 1:length(stems)) {
    S = xmlValue(stems[[i]][["StemKey"]])
    SpeciesGroupKey = as.integer(xmlValue(stems[[i]][["SpeciesGroupKey"]]))
    if (SpeciesGroupKey==gran) {
      l = stems[[i]][["SingleTreeProcessedStem"]]
      if(length(l)<1){
        l = stems[[i]][["MultiTreeProcessedStem"]]
      }
      SG=l[which(names(l)%in%"StemGrade")]
      l = l[which(names(l) == "Log")]
      LogKey = ProductKey = LogLength = Buttob = Midob = Topob = Dm = numeric(length(l))
      j = 2
      for (j in 1:length(l)) {
        Item = l[[j]] %>% xmlToList()
        LogKey[j] = Item$LogKey %>% as.integer()
        PK = Item$ProductKey %>% as.integer()
        LogLength[j] = Item$LogMeasurement$LogLength %>%
          as.integer()
        l[[j]] %>% class()
        m = l[[j]][["LogMeasurement"]]
        m = ldply(xmlToList(m), data.frame)
        m = m[, which(names(m) %in% c(".id", "text",
                                      ".attrs"))]
        m = m[!is.na(m$text), ]
        Buttob[j] = m$text[m$.attrs == "Butt ob"] %>%
          as.numeric()
        Midob[j] = m$text[m$.attrs == "Mid ob"] %>%
          as.numeric()
        Topob[j] = m$text[m$.attrs == "Top ob"] %>%
          as.numeric()
        if(j==1){
          Hm=c(0,LogLength[j]/2,LogLength[j])
          Dm=c(Buttob[j],Midob[j],Topob[j])
          ProductKey=rep(PK,3)
        }else{
          Hm=c(Hm,Hm[length(Hm)],Hm[length(Hm)]+LogLength[j]/2,Hm[length(Hm)]+LogLength[j])
          Dm=c(Dm,Buttob[j],Midob[j],Topob[j])
          ProductKey=c(ProductKey,rep(PK,3))
        }
      }
      l = cbind(Hm, Dm, ProductKey)%>% data.table() %>% tibble()
      l$Hm=l$Hm/100
      l$Dm=l$Dm/10
      lm = l[!duplicated(Dm), ]
      lm = lm[lm$Hm >= 0.5, ]
      if (nrow(lm) > 2) {
        mHt = hfromd(d = lm$Dm, h = lm$Hm, sp = "spruce",
                     output = "h")
        if (length(mHt[[1]][1]) > 0 & (!is.na(mHt[[1]][1]))) {
          diameterPosition = seq(0, max(l$Hm), 0.1)
          DiameterValue = kublin_no(Hx = diameterPosition,
                                    Hm = lm$Hm, Dm = lm$Dm, mHt = mHt[[1]][1],
                                    sp = 1)
          DiameterValue = sort(DiameterValue$DHx, decreasing = T)
          df = data.frame(d = DiameterValue, h = diameterPosition)
          cat(plot(lm$Hm, lm$Dm, xlab = "height (m)",
                   ylab = "diameter (cm)"))
          cat(points(df$h, df$d, type = "l"))
          StemGrade = rep(NA, length(diameterPosition))
          if(GradeDetermination=="GradeValue"){
            SG
            s=2
            for(s in 1:length(SG)){
              GradeValue = SG[[s]] %>% xmlToList()
              gradeStartPosition=as.numeric(GradeValue[[2]])/100
              GradeValue=as.numeric(GradeValue[[1]])
              idxmin = which(near(diameterPosition, round_any(gradeStartPosition,
                                                              0.1, f = floor)))
              StemGrade[idxmin:length(StemGrade)] = GradeValue
            }
          }
          if(GradeDetermination=="LogsProductKey"){
            keys = c()
            for (k in 1:nrow(l)) {
              key = l$ProductKey[k]
              if (!key %in% keys[length(keys)]) {
                keys = c(keys, key)
              }
            }
            k = 1
            for (k in 1:length(keys)) {
              min = min(l$Hm[l$ProductKey == keys[k]])
              max = max(l$Hm[l$ProductKey == keys[k]])
              idxmin = which(near(diameterPosition, round_any(min,
                                                              0.1)))
              idxmax = which(near(diameterPosition, round_any(max,
                                                              0.1, f = floor)))
              if (!length(idxmin) == 0 & !length(idxmax) ==
                  0) {
                GradeValue = PermittedGrades[[as.character(keys[k])]] %>%
                  max()
                StemGrade[idxmin:idxmax] = GradeValue
              }
            }
          }

          stempr = cbind(S, SpeciesGroupKey, diameterPosition,
                         DiameterValue, StemGrade) %>% data.table()
          if (is.na(stempr$StemGrade[nrow(stempr)])) {
            stempr = stempr[-nrow(stempr), ]
          }
          colnames(stempr) = c("StemKey", "SpeciesGroupKey",
                               "diameterPosition", "DiameterValue",
                               "StemGrade")
          result[[i]] = stempr
        }
      }
      setTxtProgressBar(pb, i)
    }
  }
  result = rbindlist(result)
  result$diameterPosition = as.numeric(result$diameterPosition) *
    100
  result$DiameterValue = as.numeric(result$DiameterValue) *
    10
  result$StemGrade = as.integer(result$StemGrade)
  close(pb)
  return(result)
}
