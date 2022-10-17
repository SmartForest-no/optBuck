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
  res<-aggregate(Logs$m3sob,by=list(Logs$ProductKey),sum)
  names(res)<-c("ProductKey","m3sob")
  ProductData<-ProductData[ProductData$ProductKey%in%res$ProductKey,]
  res<-merge(res,ProductData,by="ProductKey")
  res<-res[,c("m3sob","ProductName")] %>% distinct()
  res$m3sob<-res$m3sob %>% round(digits=2)
  res$ProductName[is.na(res$ProductName)]<-"Waste"
  p<-ggplot(res, aes(m3sob, ProductName)) +
    geom_col()
  print(p)
  return(res[,c(2,1)])
}
