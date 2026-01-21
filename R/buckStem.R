#' buckStem
#'
#' Optimal bucking of a single tree stem using dynamic programming.
#'
#' @param stem data.frame or data.table containing a stem profile on a 10 cm grid. Must include
#'   columns \code{diameterPosition} (cm), \code{DiameterValue} (mm), \code{StemGrade},
#'   and \code{SpeciesGroupKey}. If \code{StemKey} is present it is ignored unless passed via \code{StemKey}.
#' @param ProductData data.frame or data.table with product definitions. Must include
#'   \code{ProductKey}, \code{SpeciesGroupKey}, \code{LengthClassLowerLimit}, \code{LengthClassMAX},
#'   \code{DiameterClassLowerLimit}, and \code{DiameterClassMAX}.
#' @param PriceMatrices named list of price matrices indexed by \code{ProductKey} (character). Each matrix must have
#'   row names as length classes (cm) and column names as diameter classes (mm). Values are prices per volume unit.
#' @param PermittedGrades named list indexed by \code{ProductKey} (character). Each element is an integer vector of
#'   stem grades permitted for that product. A candidate segment is feasible for a product if all stem grades observed
#'   within the segment are contained in the product's permitted grade set.
#' @param StemKey optional integer stem identifier included in the returned table.
#'
#' @return A \code{data.table} with the optimal bucking solution, one row per log, containing
#'   \code{StemKey}, \code{LogKey}, \code{StartPos} (cm), \code{StopPos} (cm), \code{Top_ub} (mm),
#'   \code{LogLength} (cm), \code{ProductKey}, \code{Volume}, \code{Value}, and \code{CumulativeValue}.
#'   Returns an empty \code{data.table} if no feasible solution is found.
#'
#' @details
#' The algorithm evaluates all feasible transitions between diameter positions on a 10 cm grid.
#' For each candidate log segment, feasibility is determined by length and diameter class limits from \code{ProductData}
#' and by the requirement that all stem grades encountered in the segment are permitted by the product.
#' Prices are looked up in \code{PriceMatrices} by flooring length and diameter to the nearest lower class boundary.
#' Volume is computed with a frustum approximation based on rotation diameter at the segment start and top diameter at the segment end.
#'
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#'
#' @references
#' Skogforsk 2011. Introduction to StanForD 2010. URL: Skogforsk.
#' https://www.skogforsk.se/contentassets/1a68cdce4af1462ead048b7a5ef1cc06/stanford-2010-introduction-150826.pdf
#'
#' @export
buckStem <- function(stem,
                            ProductData,
                            PriceMatrices,
                            PermittedGrades,
                            StemKey = NA_integer_) {
  stopifnot(requireNamespace("data.table", quietly = TRUE))
  DT <- data.table::data.table
  asDT <- data.table::as.data.table

  stem <- asDT(stem)
  diameterPosition <- as.integer(stem$diameterPosition)
  DiameterValue <- as.numeric(stem$DiameterValue)
  StemGrade <- as.integer(stem$StemGrade)
  SpeciesGroupKey <- unique(stem$SpeciesGroupKey)

  if (length(diameterPosition) < 2L) return(DT())
  if (length(SpeciesGroupKey) != 1L) SpeciesGroupKey <- SpeciesGroupKey[1]

  ## pure helpers
  grdFinder2 <- function(idxstop, idxstart, StemGrade) {
    if (is.na(idxstop) || is.na(idxstart)) return(integer(0))
    if (idxstop < idxstart) return(integer(0))
    unique(as.integer(StemGrade[idxstart:idxstop]))
  }

  ## SGKG_list: list keyed by ProductKey, values are permitted grades for that product
  asoFinder2 <- function(grd, SGKG_list) {
    if (!length(grd) || !length(SGKG_list)) return(integer(0))

    grd <- unique(as.integer(grd))
    grd <- grd[!is.na(grd)]

    keep <- vapply(SGKG_list, function(g) {
      g <- unique(as.integer(g))
      g <- g[!is.na(g)]
      all(grd %in% g)
    }, logical(1))

    as.integer(names(SGKG_list)[keep])
  }

  ## price lookup: floor to class boundaries in matrix dimnames
  price_lookup <- function(pk, top_mm, len_cm) {
    pm <- PriceMatrices[[as.character(pk)]]
    if (is.null(pm)) return(0.0)

    rn <- suppressWarnings(as.integer(rownames(pm)))
    cn <- suppressWarnings(as.integer(colnames(pm)))
    if (!length(rn) || !length(cn)) return(0.0)

    r <- max(rn[rn <= len_cm], na.rm = TRUE)
    c <- max(cn[cn <= top_mm], na.rm = TRUE)
    if (!is.finite(r) || !is.finite(c)) return(0.0)

    as.numeric(pm[as.character(r), as.character(c)])
  }

  ## positions (10 cm grid)
  pos <- sort(unique(diameterPosition[diameterPosition >= 0L]))
  npos <- length(pos)
  if (npos < 2L) return(DT())

  pos_to_i <- setNames(seq_len(npos), as.character(pos))
  if (!("0" %in% names(pos_to_i))) return(DT())

  dp <- rep(-Inf, npos)
  prev_i <- rep(NA_integer_, npos)
  prev_pk <- rep(NA_integer_, npos)
  prev_topub <- rep(NA_real_, npos)
  prev_vol <- rep(NA_real_, npos)
  prev_len <- rep(NA_integer_, npos)

  dp[pos_to_i[["0"]]] <- 0

  ## products for this species group
  SGPK <- ProductData$ProductKey[ProductData$SpeciesGroupKey == SpeciesGroupKey]
  if (!length(SGPK)) return(DT())

  ## pruning by minimum length across products
  Lmin_all <- as.numeric(ProductData$LengthClassLowerLimit)
  Lmin_all <- Lmin_all[is.finite(Lmin_all) & Lmin_all > 0]
  if (!length(Lmin_all)) return(DT())

  maxPos <- max(diameterPosition, na.rm = TRUE)
  bult <- seq(10L, 100L, 10L)

  ## permitted grades object (ProductKey -> grades)
  SGKG_list <- PermittedGrades[as.character(SGPK)]

  for (si in seq_len(npos)) {
    sp <- pos[si]
    if (!is.finite(dp[si])) next
    if (sp >= (maxPos - min(Lmin_all))) next

    stopPos <- if (sp == 0L) sort(unique(c(sp + bult, sp + pos[pos > 0L]))) else (sp + pos[pos > 0L])
    stopPos <- stopPos[stopPos <= maxPos & stopPos > sp]
    if (!length(stopPos)) next

    idxstart <- which(diameterPosition == sp)
    if (!length(idxstart)) next
    idxstart <- idxstart[1]
    rotdiam <- DiameterValue[idxstart]

    idxstop <- match(stopPos, diameterPosition)
    ok <- !is.na(idxstop)
    if (!any(ok)) next
    idxstop <- idxstop[ok]

    ## grades encountered from start to stop
    grd <- lapply(idxstop, grdFinder2, idxstart = idxstart, StemGrade = StemGrade)

    ## permitted productkeys per stop position
    asos <- lapply(grd, asoFinder2, SGKG_list = SGKG_list)
    if (!length(asos) || all(lengths(asos) == 0L)) next

    ## build transitions
    tab_list <- vector("list", length(idxstop))
    kk <- 1L
    for (j in seq_along(idxstop)) {
      pk <- asos[[j]]
      if (!length(pk)) next
      tab_list[[kk]] <- DT(idxstop = rep.int(idxstop[j], length(pk)),
                           ProductKey = as.integer(pk))
      kk <- kk + 1L
    }
    tab <- data.table::rbindlist(tab_list, use.names = TRUE, fill = TRUE)
    if (!nrow(tab)) next

    idxp <- match(tab$ProductKey, ProductData$ProductKey)
    tab[, `:=`(
      LengthClassLowerLimit = as.integer(ProductData$LengthClassLowerLimit[idxp]),
      LengthClassMAX = as.integer(ProductData$LengthClassMAX[idxp]),
      DiameterClassLowerLimit = as.integer(ProductData$DiameterClassLowerLimit[idxp]),
      DiameterClassMAX = as.integer(ProductData$DiameterClassMAX[idxp])
    )]

    tab[, StartPos := sp]
    tab[, StopPos := diameterPosition[idxstop][match(tab$idxstop, idxstop)]]
    tab[, StopPos := diameterPosition[tab$idxstop]]
    tab[, LogLength := as.integer(StopPos - StartPos)]

    tab <- tab[is.finite(LogLength) &
                 LogLength >= LengthClassLowerLimit &
                 LogLength <= LengthClassMAX]
    if (!nrow(tab)) next

    tab[, Top_ub := as.integer(round(DiameterValue[tab$idxstop]))]
    tab <- tab[is.finite(Top_ub) &
                 Top_ub >= DiameterClassLowerLimit &
                 Top_ub <= DiameterClassMAX]
    if (!nrow(tab)) next

    ## price by matrix classing
    tab[, Price := {
      out <- numeric(.N)
      for (i in seq_len(.N)) {
        out[i] <- price_lookup(ProductKey[i], Top_ub[i], LogLength[i])
      }
      out
    }]
    tab[!is.finite(Price), Price := 0.0]

    ## volume
    tab[, Volume := {
      r1 <- rotdiam / 2
      r2 <- (Top_ub + LogLength * 0.01) / 2
      ((1 / 3) * pi * (r1^2 + r2^2 + (r1 * r2)) * LogLength) / 1e8
    }]
    tab[!is.finite(Volume), Volume := 0.0]

    tab[, Value := Volume * Price]
    tab[!is.finite(Value), Value := 0.0]

    tab[, ti := pos_to_i[as.character(StopPos)]]
    tab <- tab[!is.na(ti)]
    if (!nrow(tab)) next

    cand <- dp[si] + tab$Value
    better <- cand > dp[tab$ti]

    if (any(better)) {
      upd <- tab[better, .(
        ti,
        cand = dp[si] + Value,             # compute per-row after filtering
        pk = as.integer(ProductKey),
        topub = as.numeric(Top_ub),
        vol = as.numeric(Volume),
        len = as.integer(LogLength)
      )]

      data.table::setorder(upd, ti, -cand)
      upd <- upd[!duplicated(ti)]

      dp[upd$ti] <- upd$cand
      prev_i[upd$ti] <- si
      prev_pk[upd$ti] <- upd$pk
      prev_topub[upd$ti] <- upd$topub
      prev_vol[upd$ti] <- upd$vol
      prev_len[upd$ti] <- upd$len
    }
  }

  ## best reachable end position
  end_i <- which.max(ifelse(is.na(prev_i), -Inf, dp))
  if (!is.finite(dp[end_i]) || dp[end_i] <= 0) return(DT())

  ## backtrack
  out <- list()
  ci <- end_i
  while (!is.na(prev_i[ci])) {
    pi <- prev_i[ci]
    out[[length(out) + 1L]] <- DT(
      StemKey = as.integer(StemKey),
      StartPos = pos[pi],
      StopPos = pos[ci],
      Top_ub = prev_topub[ci],
      LogLength = prev_len[ci],
      ProductKey = prev_pk[ci],
      Volume = prev_vol[ci],
      Value = dp[ci] - dp[pi],
      CumulativeValue = dp[ci]
    )
    ci <- pi
  }

  out <- data.table::rbindlist(rev(out))
  out[, LogKey := seq_len(.N)]
  data.table::setcolorder(out, c("StemKey", "LogKey", "StartPos", "StopPos",
                                 "Top_ub", "LogLength", "ProductKey",
                                 "Volume", "Value", "CumulativeValue"))
  out
}
