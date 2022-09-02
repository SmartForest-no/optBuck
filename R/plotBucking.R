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
    log = tre[which(tre$diameterPosition == round_any(tab$StartPos[i], 10)):which(tre$diameterPosition ==round_any(tab$StopPos[i],
                                                                                                                   10)), ]
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
  colors = c("#3288BD", "#F46D43" ,"#E6F598")
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
    coord_flip()+
    ggtitle(paste("Stem value:", round(max(tab$CumulativeValue))))
  plot
  return(plot)
}
