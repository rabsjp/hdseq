rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

## We work with periods 6-11 to account for learning
dsub<-d[d$period>5,]

## We take the median cutoff and the average move rate
dsub$mediancut<-ave(dsub$cutoff,dsub$idu,FUN=function(x) median(x, na.rm=T))
dsub$moverate<-ave(dsub$move,dsub$idu,FUN=function(x) mean(x, na.rm=T))

dsub<-dsub[,c("gender","idu","tres","tree","treo","mediancut","moverate")]
dsub<-unique(dsub)

cdf_x<-  ecdf(dsub$mediancut[dsub$treo==1])
cdf_xs<-  ecdf(dsub$mediancut[dsub$tres==1])
cdf_xe<-  ecdf(dsub$mediancut[dsub$tree==1])
x<-seq(0.0,100,5)

pdf("cdfcutoff.pdf")
plot(cdf_x,verticals=T,xlim=c(0,100),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xs,verticals=T,lty=2,cex=0,lwd=1)
lines(cdf_xe,verticals=T,lty=3,cex=0,lwd=2)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo

text(92.5,0.25,expression(x^{CGS}),cex = 1)
text(36,0.825,expression(x^{CGO}),cex = 1)
legend(0.01,0.9,legend=c("CGO","CGS","CGE"),lwd=c(3,1,2),lty=c(1,2,3),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()



cdf_xm<-  ecdf(dsub$mediancut[dsub$tree==1 & dsub$gender==1])
cdf_xf<-  ecdf(dsub$mediancut[dsub$tree==1 & dsub$gender==0])
x<-seq(0.0,100,5)
plot(cdf_xm,verticals=T,xlim=c(0,100),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xf,verticals=T,lty=2,cex=0,lwd=1)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))



dsub$gtree<-dsub$tree*dsub$gender
dsub$gtres<-dsub$tres*dsub$gender

summary(lm(dsub$cutoff[dsub$tree==1] ~ dsub$move[dsub$tree==1]))





plot(dsub$mediancut[dsub$tree==1],dsub$moverate[dsub$tree==1])


wilcox.test(dsub$mediancut[dsub$tres==1],dsub$mediancut[dsub$treo==1],alternative = "g")
ks.test(dsub$mediancut[dsub$tres==1],dsub$mediancut[dsub$treo==1],alternative = "l")

wilcox.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "g")
ks.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "l")
      
ks.test(dsub$mediancut[dsub$gender==1 & dsub$tree==1],dsub$mediancut[dsub$gender==1 & dsub$treo==1],alternative = "l")
      
      
