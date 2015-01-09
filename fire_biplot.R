


make.fire.biplot<-function(fss=fss){
  ss<-fss$massLoss>50&fss$clade=="g"&!is.na(fss$frontSpeed)
  colors<-c("red","darkgreen","blue")
  
  fire.out<-cbind(maxTemp=fss$maxTemp[ss],Front.speed=6.25*60/fss$frontSpeed[ss],
                  Burning.time=fss$whole.ring.burning.time[ss],
                  Temp.sum=fss$temp.sum[ss],
                  Litterbed.Density=fss$litterbedDens[ss])
  
  PC<-prcomp(fire.out,scale=TRUE)
  
  #pdf("Figure4_v1.1.pdf")
  lambda <- PC$sdev * sqrt(nrow(PC$x))
  plot (t(t(PC$x)/lambda),pch=16,col=colors[fss$clade2[ss]],xlim=c(-0.5,0.5),ylim=c(-0.4,0.4))
  par (new=T)
  Rot <- t(t(PC$rotation)*lambda)
  XLIM <- c(-max(abs(Rot[,1])),max(abs(Rot[,1])))
  XLIM <- XLIM+(XLIM*0.2)
  plot(Rot,col=4,axes=FALSE,xlim=XLIM,ylim=XLIM,pch="")
  arrows (rep(0,nrow(PC$rotation)),rep(0,nrow(PC$rotation)),Rot[,1],Rot[,2],col=4)
  text (Rot[,1:2],rownames(Rot),col=1,cex=0.7)
  axis (3)
  axis (4)
  legend("topleft",legend=c("cycads","other","Pinus spp."),cex=0.7,pch=16,col=colors)
  #dev.off()
}
