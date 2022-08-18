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
      ObjectName = xmlValue(XMLNode[["Machine"]][["ObjectDefinition"]][["ObjectName"]])
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
