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
#' @return result structure with optimum bucking solution for the stems in the input .hpr file
#' @seealso getPermittedGrades, getPriceMatrices, getProductData
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @references Skogforsk 2011. Introduction to StanForD 2010. URL: Skogforsk. https://www.skogforsk.se/contentassets/1a68cdce4af1462ead048b7a5ef1cc06/stanford-2010-introduction-150826.pdf
#' @export

buckHpr=function (XMLNode, PriceMatrices, ProductData, StemProfile, PermittedGrades,
                  SpeciesGroupDefinition)
{
  require(XML)
  require(plyr)
  require(dplyr)
  require(data.table)

  grdFinder = function(x) {
    unique(StemGrade[idxstart:x])
  }
  asoFinder = function(x) {
    names(SGKG)[which(colSums(matrix(sapply(SGKG,
                                            FUN = function(X) all(grd[[x]] %in% X)), ncol = length(SGKG))) >
                        0)]
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
  rowFinder = function(x) sum(commercial$LogLength[x] >=
                                rownames[[x]] %>% as.numeric())
  colFinder = function(x) sum(commercial$topdiam[x] >=
                                colnames[[x]] %>% as.numeric())
  priceFinder = function(x) lis[[x]][row[x], col[x]]
  seqVectozied = Vectorize(seq.default, vectorize.args = c("from",
                                                           "to"))
  trackTrace = function(res, tt) {
    low = min(tt[, "StartPos"])
    while (low > 0) {
      id_previous = tt$CumulativeValue[order(tt$StartPos)[1]] -
        tt$Value[order(tt$StartPos)[1]]
      sub = res[res$StopPos == low, ]
      prev = sub[which(near(sub$CumulativeValue,
                            id_previous)), ]
      if (!is.vector(prev)) {
        prev = prev[1, ]
      }
      tt = rbind(tt, prev)
      low = min(tt$StartPos)
    }
    tt = tt[nrow(tt):1, ]
    if (is.vector(tt)) {
      tt[5] = ifelse(tt[5] == 0, 999999, tt[5])
    }
    return(tt)
  }

  stems <- XMLNode[["Machine"]][names(xmlSApply(XMLNode[["Machine"]],
                                                xmlAttrs)) == "Stem"]
  pb <- txtProgressBar(min = 0, max = length(stems), style = 3,
                       width = 50, char = "=")
  ProductData <- ProductData[!is.na(ProductData$ProductName),]
  result <- list()
  for (i in 1:length(stems)) {
    StemKey <- SK <- as.integer(xmlValue(stems[[i]][["StemKey"]]))
    stem <- StemProfile[StemProfile$StemKey == SK, ]
    if (nrow(stem) > 0) {
      diameterPosition <- as.numeric(stem$diameterPosition)
      DiameterValue <- as.numeric(stem$DiameterValue)
      StemGrade <- as.numeric(stem$StemGrade)
      SpeciesGroupKey <- unique(stem$SpeciesGroupKey)
      ProductKeys <- ProductData$ProductKey
      LengthClassLowerLimit <- as.numeric(ProductData$LengthClassLowerLimit)
      LengthClassMAX <- as.numeric(ProductData$LengthClassMAX)
      DiameterClassLowerLimit <- as.numeric(ProductData$DiameterClassLowerLimit)
      DiameterClassMAX <- as.numeric(ProductData$DiameterClassMAX)
      VolumeDiameterCategory <- ProductData$VolumeDiameterCategory
      DiameterTopPositions <- ProductData$DiameterTopPosition
      DBH <- xmlValue(stems[[i]][["SingleTreeProcessedStem"]][["DBH"]]) %>%
        as.numeric()

      SeqStart = round_any(min(LengthClassLowerLimit[LengthClassLowerLimit >
                                                       0]), 10)
      SeqStop = ifelse(max(LengthClassMAX) < max(diameterPosition),
                       max(LengthClassMAX), max(diameterPosition))
      DiameterTopPositions = ProductData$DiameterTopPositions
      bult = seq(10, 100, 10)
      res = data.table(StartPos = -1, StopPos = 0, Top_ub = NA,
                       LogLength = NA, ProductKey = NA, Volume = 0,
                       Value = 0, CumulativeValue = 0)
      if (SeqStart < SeqStop) {
        SeqAsp = seq(SeqStart, SeqStop, 10)
        StartPos = 0
        while (StartPos < max(diameterPosition) - min(LengthClassLowerLimit[LengthClassLowerLimit >
                                                                            0])) {
          StartPos = sort(res$StopPos[!res$StopPos %in%
                                        res$StartPos])[1]

          if (StartPos == 0) {
            StopPos = StartPos + c(bult, SeqAsp)
          } else {
            StopPos = StartPos + SeqAsp
          }
          StopPos = StopPos[StopPos <= max(diameterPosition) &
                              StopPos > 0]
          if (length(StopPos) < 1) {
            break
          }
          LogLength = StopPos - StartPos
          rotdiam = DiameterValue[which(near(diameterPosition,
                                             StartPos))]
          idxstart = as.numeric(which(near(diameterPosition,
                                           StartPos)))
          idxstop = as.numeric(match(as.character(StopPos),
                                     as.character(diameterPosition)))
          grd = lapply(idxstop, grdFinder)
          SGPK = ProductData$ProductKey[ProductData$SpeciesGroupKey ==
                                          SpeciesGroupKey[1]]
          m = data.table(idxstart, idxstop, StartPos,
                         StopPos, LogLength, rotdiam)
          m = m[m$StopPos <= max(diameterPosition), ]
          SGKG = PermittedGrades[as.character(SGPK)]
          asos = lapply(1:length(grd), asoFinder)
          lapply(1:length(grd), function(x) {
            names(SGKG)[which(colSums(matrix(sapply(SGKG,
                                                    FUN = function(X) all(grd[[x]] %in% X)),
                                             ncol = length(SGKG))) > 0)]
          })
          m$Price = 0
          r = rep(idxstop, len = sum(lengths(asos)))
          r = r[order(r)]
          tab = data.table(idxstop = r, ProductKey = unlist(asos))
          idx = match(as.character(tab$ProductKey), as.character(ProductData$ProductKey))
          tab = cbind(tab, data.table(DiameterUnderBark = ProductData$DiameterUnderBark[idx],
                                      LengthClassLowerLimit = ProductData$LengthClassLowerLimit[idx],
                                      LengthClassMAX = ProductData$LengthClassMAX[idx],
                                      DiameterClassLowerLimit = ProductData$DiameterClassLowerLimit[idx],
                                      DiameterClassMAX = ProductData$DiameterClassMAX[idx],
                                      VolumeDiameterAdjustment = ProductData$VolumeDiameterAdjustment[idx],
                                      VolumeDiameterCategory = ProductData$VolumeDiameterCategory[idx],
                                      VolumeLengthCategory = ProductData$VolumeLengthCategory[idx],
                                      DiameterTopPosition = as.numeric(ProductData$DiameterTopPositions[idx])))
          tab = merge(m, tab, "idxstop", allow.cartesian = TRUE)
          tab$StopPosAdj = round((tab$StopPos - tab$DiameterTopPosition)/10) *
            10
          tab$Top_ob = DiameterValue[match(as.character(tab$StopPosAdj),
                                           as.character(diameterPosition))]
          tab$Top_ub = BarkFunction(tab$Top_ob, SpeciesGroupKey,
                                    SpeciesGroupDefinition, Top_ob = Top_ob,
                                    DBH = DBH, LogLength = LogLength)
          tab$topdiam = ifelse(tab$DiameterUnderBark,
                               tab$Top_ub, tab$Top_ob)
          tab = tab[tab$LogLength >= tab$LengthClassLowerLimit,]
          tab = tab[tab$LogLength <= tab$LengthClassMAX,]
          tab = tab[tab$topdiam > tab$DiameterClassLowerLimit,]
          tab = tab[tab$rotdiam < tab$DiameterClassMAX,]
          if (nrow(tab) > 0){
            commercial = tab[tab$ProductKey != "999999",]
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
            tab$idxstop[tab$VolumeLengthCategory == "Rounded downwards to nearest dm-module"] = match(as.character(round_any((tab$StopPos[tab$VolumeLengthCategory ==
                                                                                                                                            "Rounded downwards to nearest dm-module"]),
                                                                                                                             10, f = floor)), as.character(diameterPosition))
            WithLengthClass = tab[tab$VolumeLengthCategory ==
                                    "Length as defined in LengthClasses" &
                                    tab$ProductKey != "999999", ]
            lis = LengthClasses[WithLengthClass$ProductKey]
            if (nrow(WithLengthClass) > 0) {
              l = 1
              for (l in 1:nrow(WithLengthClass)) {
                LengthClass = LengthClasses[[WithLengthClass$ProductKey[l]]]
                WithLengthClass$LogLength[l] = round_any(LengthClass[max(which(WithLengthClass$LogLength[l] >=
                                                                                 LengthClass))], 10, f = ceiling)
                WithLengthClass$StopPos[l] = WithLengthClass$StartPos[l] +
                  WithLengthClass$LogLength[l]
                WithLengthClass$idxstop[l] = which(diameterPosition ==
                                                     paste(round_any(WithLengthClass$StopPos[l],
                                                                     10, f = ceiling)))
              }
              tab$LogLength[tab$VolumeLengthCategory ==
                              "Length as defined in LengthClasses"] = WithLengthClass$LogLength
              tab$StopPos[tab$VolumeLengthCategory ==
                            "Length as defined in LengthClasses"] = WithLengthClass$StopPos
              tab$idxstop[tab$VolumeLengthCategory ==
                            "Length as defined in LengthClasses"] = WithLengthClass$idxstop
            }
            vec = seqVectozied(from = tab$idxstart, to = tab$idxstop,
                               by = 1)
            DV = sapply(1:length(vec), DiameterValueFinder)
            idx = tab$VolumeDiameterAdjustment == "Measured diameter rounded downwards to cm"
            if (sum(idx) > 0) {
              if (sum(idx) > 1) {
                DV[which(idx > 0)] = lapply(1:sum(idx),
                                            function(x) {
                                              as.vector(sapply(1:length(DV[which(idx >
                                                                                   0)[x]]), Rounder))
                                            })
              }
              else {
                DV[idx] = sapply(1:length(DV[idx]), Rounder)
              }
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
            tab$Volume[idx] = (((1/3) * pi * (r1^2 +
                                                r2^2 + (r1 * r2)) * tab$LogLength)/1e+08)[idx]
            idx = tab$VolumeDiameterCategory == "Top" &
              tab$DiameterUnderBark == F
            r1 = tab$Top_ob/2
            r2 = (tab$Top_ob + tab$LogLength * 0.01)/2
            tab$Volume[idx] = (((1/3) * pi * (r1^2 +
                                                r2^2 + (r1 * r2)) * tab$LogLength)/1e+08)[idx]
            tab$Value = tab$Volume * tab$Price
            head(tab)
            m = tab[, c("StartPos", "StopPos", "Top_ub",
                        "LogLength", "ProductKey", "Volume", "Value")]
            idx=paste(res$StopPos)==paste(StartPos)
            sub = res[idx,]

            CumulativeValue = ifelse(nrow(sub)>0&any(!is.na(sub$CumulativeValue)),
                                                max(sub$CumulativeValue,na.rm = T),
                                                0)
            m$CumulativeValue = m$Value + CumulativeValue
          }else {
            m = data.table(StartPos = StartPos, StopPos = StopPos,
                           Top_ub = NA, LogLength = NA, ProductKey = NA,
                           Volume = NA, Value = NA, CumulativeValue = NA)
          }
          res = rbindlist(list(res, m))
        }
      }
      res = res[!is.na(res$LogLength),]
      tt = res[which.max(res$CumulativeValue),]
      if (nrow(tt) == 1){
        res = trackTrace(res, tt)
      }else{
        res = data.table(StartPos = NA, StopPos = NA,
                         Top_ub = NA, LogLength = 1, ProductKey = NA,
                         Volume = NA, Value = 0, CumulativeValue = 0)
      }
      res = cbind(1:nrow(res), res)
      colnames(res)[1] = "LogKey"
      out <- cbind(rep(StemKey, nrow(res)), res)
      colnames(out)[1] <- c("StemKey")
      result[[i]] <- out
    }
    setTxtProgressBar(pb, i)
    print(i)
  }
  result <- do.call(rbind.data.frame, result)
  close(pb)
  return(result)
}

