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
