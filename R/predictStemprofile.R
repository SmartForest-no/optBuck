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
