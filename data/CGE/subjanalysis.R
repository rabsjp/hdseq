rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

## We work with periods 6-11 to account for learning
dsub<-d[d$period>5,]

## We take the median cutoff, average move rate, average choice rate
dsub$mediancut<-ave(dsub$cutoff,dsub$idu,FUN=function(x) median(x, na.rm=T))
dsub$moverate<-ave(dsub$move,dsub$idu,FUN=function(x) mean(x, na.rm=T))
dsub$choicerate<-ave(dsub$play,dsub$idu,FUN=function(x) mean(x, na.rm=T))

#Important variables are gender, subject_id, treatmens (seq,endog,one) 
dsub<-dsub[,c("gender","idu","tres","tree","treo","mediancut","moverate","choicerate")]
dsub<-unique(dsub)

cdf_x<-  ecdf(dsub$mediancut[dsub$treo==1])
cdf_xs<-  ecdf(dsub$mediancut[dsub$tres==1])
cdf_xe<-  ecdf(dsub$mediancut[dsub$tree==1])
x<-seq(0.0,101,5)


#CDF of cutoff across treatments. Theory predicts CGS,CGS to FSD: CGO
pdf("cdfcutoff.pdf")
plot(cdf_x,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
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

#CDF of cutoff early/late move in CGE. Theory predicts early FSD late
day_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate<=0.5]
nigth_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate>0.5]
cdf_xe_day<-  ecdf(day_cutoff)
cdf_xe_night<-  ecdf(night_cutoff)
wilcox.test(day_cutoff,night_cutoff,alternative = "g")
ks.test(day_cutoff,night_cutoff,alternative = "l")

pdf("cdfmove_cutoff.pdf")
plot(cdf_xe_day,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xe_nigth,verticals=T,lty=2,cex=0,lwd=1)
legend(0.01,0.9,legend=c("CGE first mover","CGE second mover"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
axis(1, at=x,labels=as.character.numeric_version(x), las=1)

text(92.5,0.25,expression(x^{CGE}),cex = 1)
dev.off()

days_cutoff<-dsub$mediancut[dsub$tres==1 & dsub$moverate<=0.5]
nights_cutoff<-dsub$mediancut[dsub$tres==1 & dsub$moverate>0.5]
cdf_xs_day<-  ecdf(days_cutoff)
cdf_xs_night<-  ecdf(nights_cutoff)

pdf("cdfmove_s_cutoff.pdf")
plot(cdf_xs_day,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xs_night,verticals=T,lty=2,cex=0,lwd=1)
legend(0.01,0.9,legend=c("CGS first mover","CGS second mover"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
axis(1, at=x,labels=as.character.numeric_version(x), las=1)

text(92.5,0.25,expression(x^{CGS}),cex = 1)
dev.off()







day_m_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate<=0.5 & dsub$gender==1]
night_m_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate>0.5& dsub$gender==1]
day_f_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate<=0.5 & dsub$gender==0]
night_f_cutoff<-dsub$mediancut[dsub$tree==1 & dsub$moverate>0.5& dsub$gender==0]

cdf_xem_night<-  ecdf(night_m_cutoff)
cdf_xem_day<-ecdf(day_m_cutoff)
cdf_xef_night<-  ecdf(night_f_cutoff)
cdf_xef_day<-ecdf(day_f_cutoff)

pdf("cdfmove_m_cutoff.pdf")
plot(cdf_xem_day,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xem_night,verticals=T,lty=2,cex=0,lwd=1)
legend(0.01,0.9,legend=c("CGE first mover","CGE second mover"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
axis(1, at=x,labels=as.character.numeric_version(x), las=1)

text(92.5,0.25,expression(x^{CGE}),cex = 1)
dev.off()

pdf("cdfmove_f_cutoff.pdf")
plot(cdf_xef_day,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xef_night,verticals=T,lty=2,cex=0,lwd=1)
legend(0.01,0.9,legend=c("CGE first mover","CGE second mover"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
axis(1, at=x,labels=as.character.numeric_version(x), las=1)

text(92.5,0.25,expression(x^{CGE}),cex = 1)
dev.off()



cdf_ch<-  ecdf(dsub$choicerate[dsub$treo==1])
cdf_chs<-  ecdf(dsub$choicerate[dsub$tres==1])
cdf_che<-  ecdf(dsub$choicerate[dsub$tree==1])
pdf("cdfchoice.pdf")
plot(cdf_ch,verticals=T,xlim=c(0,1),cex=0,lwd=3,main="",xlab="choice (D)",ylab="CDF",xaxt='n')
lines(cdf_chs,verticals=T,lty=2,cex=0,lwd=1)
lines(cdf_che,verticals=T,lty=3,cex=0,lwd=2)
abline(h=.5,col="gray",lty=c(1))
xch<-seq(0.0,1.0,.1)
axis(1, at=xch,labels=as.character.numeric_version(xch), las=1)
dev.off()

## Now we study gender differences
cdf_xmo<-  ecdf(dsub$mediancut[dsub$treo==1 & dsub$gender==1])
cdf_xfo<-  ecdf(dsub$mediancut[dsub$treo==1 & dsub$gender==0])
cdf_xms<-  ecdf(dsub$mediancut[dsub$tres==1 & dsub$gender==1])
cdf_xfs<-  ecdf(dsub$mediancut[dsub$tres==1 & dsub$gender==0])
cdf_xme<-  ecdf(dsub$mediancut[dsub$tree==1 & dsub$gender==1])
cdf_xfe<-  ecdf(dsub$mediancut[dsub$tree==1 & dsub$gender==0])

pdf("cdfcutoffgender_o.pdf")
plot(cdf_xmo,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xfo,verticals=T,lty=2,cex=0,lwd=1)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
#abline(v=90,col="gray",lty=c(1)) #eqm for cge
abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo
text(36,0.825,expression(x^{CGO}),cex = 1)
legend(0.01,0.9,legend=c("CGO men","CGO women"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()

pdf("cdfcutoffgender_s.pdf")
plot(cdf_xms,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xfs,verticals=T,lty=2,cex=0,lwd=1)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
#abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo
text(92.5,0.25,expression(x^{CGS}),cex = 1)
legend(0.01,0.9,legend=c("CGS men","CGS women"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()

pdf("cdfcutoffgender_e.pdf")
plot(cdf_xme,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xfe,verticals=T,lty=2,cex=0,lwd=1)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo
text(92.5,0.25,expression(x^{CGS}),cex = 1)
text(36,0.825,expression(x^{CGO}),cex = 1)

legend(0.01,0.9,legend=c("CGE men","CGE women"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()

pdf("cdfcutoffgender_female.pdf")
plot(cdf_xfo,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xfs,verticals=T,lty=2,cex=0,lwd=1)
lines(cdf_xfe,verticals=T,lty=3,cex=0,lwd=2)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo

text(92.5,0.25,expression(x^{CGS}),cex = 1)
text(36,0.825,expression(x^{CGO}),cex = 1)
legend(0.01,0.9,legend=c("CGO women","CGS women","CGE women"),lwd=c(3,1,2),lty=c(1,2,3),bty = "n",y.intersp=2,border=F,cex=1)

dev.off()

pdf("cdfcutoffgender_male.pdf")
plot(cdf_xmo,verticals=T,xlim=c(0,101),cex=0,lwd=3,main="",xlab="cutoff",ylab="CDF",xaxt='n')
lines(cdf_xms,verticals=T,lty=2,cex=0,lwd=1)
lines(cdf_xme,verticals=T,lty=3,cex=0,lwd=2)
axis(1, at=x,labels=as.character.numeric_version(x), las=1)
abline(h=.5,col="gray",lty=c(1))
abline(v=90,col="gray",lty=c(1)) #eqm for cge
abline(v=33.33,col="gray",lty=c(1)) #eqm for cgo

text(92.5,0.25,expression(x^{CGS}),cex = 1)
text(36,0.825,expression(x^{CGO}),cex = 1)
legend(0.01,0.9,legend=c("CGO men","CGS men","CGE men"),lwd=c(3,1,2),lty=c(1,2,3),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()

wilcox.test(dsub$mediancut[dsub$tres==1 & dsub$gender==0],dsub$mediancut[dsub$treo==1 & dsub$gender==0],alternative = "g")
ks.test(dsub$mediancut[dsub$tres==1 & dsub$gender==0],dsub$mediancut[dsub$treo==1 & dsub$gender==0],alternative = "l")



# who moves early? 

cdfmovem<-ecdf(dsub$moverate[dsub$tree==1 & dsub$gender==1])
cdfmovef<-ecdf(dsub$moverate[dsub$tree==1 & dsub$gender==0])

#summary(lm(dsub$moverate[dsub$tree==1] ~ dsub$gender[dsub$tree==1]))
pdf("cdfmove_gender.pdf")
plot(cdfmovem,verticals=T,xlim=c(0,1),cex=0,lwd=3,main="",xlab="move (second)",ylab="CDF",xaxt='n')
lines(cdfmovef,verticals=T,lty=2,cex=0,lwd=1)
abline(h=.5,col="gray",lty=c(1))
xch<-seq(0.0,1.0,.1)
axis(1, at=xch,labels=as.character.numeric_version(xch), las=1)
legend(0.01,0.9,legend=c("men","women"),lwd=c(3,1),lty=c(1,2),bty = "n",y.intersp=2,border=F,cex=1)
dev.off()


dsub$gtree<-dsub$tree*dsub$gender
dsub$gtres<-dsub$tres*dsub$gender

summary(lm(dsub$cutoff[dsub$tree==1] ~ dsub$move[dsub$tree==1]))

###Tests
n<-length(dsub$mediancut[dsub$treo==1])
ks.test(dsub$mediancut[dsub$treo==1],dsub$mediancut[dsub$tres==1],alternative = c("greater"))

ks.test(dsub$mediancut[dsub$tres==1],dsub$mediancut[dsub$tree==1],alternative = c("two.sided"))


wilcox.test(dsub$mediancut[dsub$tres==1],dsub$mediancut[dsub$treo==1],alternative = "g")
ks.test(dsub$mediancut[dsub$tres==1],dsub$mediancut[dsub$treo==1],alternative = "l")

wilcox.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "g")
ks.test(dsub$mediancut[dsub$tree==1],dsub$mediancut[dsub$treo==1],alternative = "l")
      
wilcox.test(dsub$mediancut[dsub$treo==1 & dsub$gender==0],dsub$mediancut[dsub$treo==1 & dsub$gender==1],alternative = "two.sided")
ks.test(dsub$mediancut[dsub$treo==1 & dsub$gender==0],dsub$mediancut[dsub$treo==1 & dsub$gender==1],alternative = "two.sided")


x