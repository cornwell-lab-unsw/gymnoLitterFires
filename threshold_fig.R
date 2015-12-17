

make_threshold_fig<-function(alba,multiSpp){
  options(warn=-1)
  
  #mean packing ratios from the manipulations
  cun<-c(0.23,0.09,0.03,0.03)
  crypt<-c(0.2,0.1,0.09,0.065)
  
  alba$percent.sample.burned[is.na(alba$percent.sample.burned)]<-100
  psb.mean<-tapply(alba$percent.sample.burned,alba[,1:2],mean)
  psb.se<-tapply(alba$percent.sample.burned,alba[,1:2],function(x)sd(x)*length(x)^-0.5)
  
  
  #pdf("threshold_fig.pdf",width=8.5,height=4)
  #including only gymnosperms for this figure
  gy<-subset(multiSpp,multiSpp$clade=="g")
  x<-multiSpp$packRatio[multiSpp$clade=="g"]
  
  par(mfrow=c(1,2),cex.axis=0.75)
  #first the mass loss--percent burned
  gy$ml<-gy$massLoss/100
  g=glm(ml~packRatio,family=binomial,gy) 
  plot(gy$packRatio,gy$ml,log="",yaxs="r",xlim=c(0.015,0.45),pch=16,ylab="Proportion of sample burned",xlab="Packing ratio (vol. litter / vol. litterbed)",bty="n",col=rgb(0,0,0,0.5))
  curve(predict(g,data.frame(packRatio=x),type="response"),to=max(x),add=TRUE)
  
  psb.y<-psb.mean[2,c(1,3,4,2)]/100
  points(cun,psb.y,col="blue",pch=16)
  arrows(x0=cun, y0=psb.y-psb.se[2,c(1,3,4,2)]/100, y1=psb.y+psb.se[2,c(1,3,4,2)]/100,length=0, angle=90,code=3,col="blue")
  lines(cun,psb.y,col="blue",pch=16,lty=2)
  
  psb.y<-psb.mean[1,c(1,3,4,2)]/100
  points(crypt,psb.y,col="red",pch=16)
  arrows(x0=crypt, y0=psb.y-psb.se[1,c(1,3,4,2)]/100, y1=psb.y+psb.se[1,c(1,3,4,2)]/100,length=0, angle=90,code=3,col="red")
  lines(crypt,psb.y,col="red",pch=16,lty=2)
  legend("topright", c("35 Gymno. species","Cryptomeria","Cunninghamia"), pch=16,col=c("black","red","blue"),lty=c(1,2,2),inset = .02,cex=0.7)
  text(x=0.21,y=1.0,"(a)",cex=0.7)
  
  ###  Second panel
  #second--maximum temperature
  
  x<-gy$packRatio[gy$clade=="g"]
  y<-gy$maxTemp[gy$clade=="g"]
  y.l<-c(-0,850)
  y.l2<-c(20,1000)
  x<-x[!is.na(x)]
  y<-y[!is.na(x)]
  
  plot(x,y,log="y",yaxs="r",xlim=c(0.015,0.45),ylim=y.l2,pch=16,ylab="Max. temperature (deg. C)",xlab="Packing ratio (vol. litter / vol. litterbed)",bty="n",col=rgb(0,0,0,0.5))
  gcoeffs <-nls(y-35~c*exp(-(x-a)^2/(2*b^2)),start=list(a=0.2,b=1,c=500), trace=FALSE)
  #plot(x,y,log="xy",yaxs="r",xlim=c(0.0175,0.45),ylim=y.l2,pch=16,ylab="Max temperature",xlab="Packing ratio",bty="n")
  lines(y=predict(gcoeffs,newdata=data.frame(x=seq(0.019,0.387,0.001)))+50,x=seq(0.019,0.387,0.001),col="black",lty=1)
  
  tmax.mean<-tapply(alba$maxtemp,alba[,1:2],mean)
  tmax.se<-tapply(alba$maxtemp,alba[,1:2],function(x)sd(x)*length(x)^-0.5)
  
  tmax.y<-tmax.mean[2,c(1,3,4,2)]
  points(cun,tmax.y,col="blue",pch=16)
  arrows(x0=cun, y0=tmax.mean[2,c(1,3,4,2)]-tmax.se[2,c(1,3,4,2)], y1=tmax.mean[2,c(1,3,4,2)]+tmax.se[2,c(1,3,4,2)],length=0, angle=90,code=3,col="blue")
  lines(cun,tmax.y,col="blue",pch=16,lty=2)
  
  tmax.y<-tmax.mean[1,c(1,3,4,2)]
  points(crypt,tmax.y,col="red",pch=16)
  arrows(x0=crypt, y0=tmax.mean[1,c(1,3,4,2)]-tmax.se[1,c(1,3,4,2)], y1=tmax.mean[1,c(1,3,4,2)]+tmax.se[1,c(1,3,4,2)],length=0, angle=90,code=3,col="red")
  lines(crypt,tmax.y,col="red",pch=16,lty=2)
  text(x=0.39,y=1000,"(b)",cex=0.7)
  
  #dev.off()
}