rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")
dsub<-d

dsub$mediancut<-ave(dsub$cutoff,dsub$idu,FUN=function(x) mean(x, na.rm=T))
dsub<-dsub[,c("gender","idu","tres","tree","treo","mediancut")]
dsub<-unique(dsub)

cdf_x<-  ecdf(dsub$mediancut[dsub$treo==1])
cdf_xs<-  ecdf(dsub$mediancut[dsub$tres==1])
cdf_xe<-  ecdf(dsub$mediancut[dsub$tree==1])
x<-seq(0.0,100,5)
plot(cdf_x,verticals=T,xlim=c(0,100),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xs,verticals=T,lty=2,cex=0,lwd=1)
lines(cdf_xe,verticals=T,lty=3,cex=0,lwd=2)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))



wilcox.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "g")
ks.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "l")
      
ks.test(dsub$mediancut[dsub$gender==1 & dsub$tree==1],dsub$mediancut[dsub$gender==1 & dsub$treo==1],alternative = "l")
      
      
cdf_x<-  ecdf(dsubme_late$player.xchoice[dsubme_late$tre==0])
