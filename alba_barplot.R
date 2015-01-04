
process_data_manipulation<-function(aa){
  return(read.delim(aa))
  aa$percent.sample.burned[is.na(aa$percent.sample.burned)]<-100
  # get correct units for speed, changing from sec to cm / min
  aa$speed.to.sensors<-6.25*60/aa$speed.to.sensors
  return(aa)
}

make.barplot<-function(x,aa,yName){
  treat.mean<-tapply(x,aa[,1:2],mean,na.rm=TRUE)
  # this codes the order of the bars to get them into decreasing order of packing density
  colnames(treat.mean)<-c("1-cm","12.5-cm","4-cm art","4-cm nat")
  treat.mean.ord<-treat.mean[,c(1,3,4,2)]
  bar.locs<-barplot(treat.mean.ord,col=c("grey90","grey40"),ylab=yName,ylim=c(0,max(x,na.rm=TRUE)*1.1),cex.names=0.5,beside=TRUE)
  treat.se<-tapply(x,aa[,1:2],function(x)sd(x,na.rm=TRUE)*sum(!is.na(x))^-0.5)
  treat.se.ord<-treat.se[,c(1,3,4,2)]
  arrows(x0=bar.locs, y0=treat.mean.ord-treat.se.ord, y1=treat.mean.ord+treat.se.ord,length=0.1, angle=90,code=3)
}

alba.plot<-function(aa){
  #pdf("alba_barplots.pdf")
  par(mfrow=c(2,2))
  make.barplot(x=aa$maxtemp,aa=aa,yName="Max temperature (C)")
  make.barplot(x=aa$speed.to.sensors,aa=aa,yName="Fire front speed (cm/min)")
  make.barplot(x=aa$total.burning.time,aa=aa,yName="Total burning time (s)")
  make.barplot(x=aa$temp.sum,aa=aa,yName="Temp sum (deg*s)")
  legend("topleft",c("Cryptomeria","Cunninghamia"),fill=c("grey90","grey40"))
  #dev.off()
}

# 
# mt<-lm(aa$maxtemp~aa$size*aa$species)
# mta<-anova(mt)
# mta$Sum/sum(mta$Sum)
# 
# mt<-lm(aa$temp.sum~aa$size*aa$species)
# mta<-anova(mt)
# mta
# mta$Sum/sum(mta$Sum)
# 
# 
# mt<-lm(aa$speed.to.sensors~aa$size*aa$species)
# mta<-anova(mt)
# mta
# mta$Sum/sum(mta$Sum)
# 
# psb<-aa$percent.sample.burned/100
# mt<-glm(psb~aa$size*aa$species,family=binomial)
# mta<-anova(mt,test="Chisq")
# mta
# mta$Sum/sum(mta$Sum)
# 
# per.anova.test
# 
# 
# anova(lm(aa$maxtemp~aa$size*aa$species))
# anova(lm(aa$maxtemp~aa$size*aa$species))
# anova(lm(aa$maxtemp~aa$size*aa$species))
# 
# 
# x<-
# 
# dev.off()
#mtext adds a label on the margin of the plot