rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

## We work with periods 6-11 to account for learning
dpool<-d[d$period>5,]
dpool$cell<-ave(dpool$play,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$movewhere<-ave(dpool$move,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$gendergroup<-ave(dpool$gender,dpool$idg,FUN=function(x) sum(x, na.rm=T))

cello<-table(dpool$cell[dpool$treo==1])/length(dpool$cell[dpool$treo==1])
cells<-table(dpool$cell[dpool$tres==1])/length(dpool$cell[dpool$tres==1])
celle<-table(dpool$cell[dpool$tree==1])/length(dpool$cell[dpool$tree==1])
cell.bar<-cbind(cello,cells,celle)
colnames(cell.bar)<-c("CGO","CGS","CGE")
poolcut<-ave(dpool$cutoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolcut<-unique(poolcut)

poolcut_m<-ave(dpool$cutoff[dpool$gender==1],dpool$tre[dpool$gender==1],FUN=function(x) mean(x, na.rm=T))
poolcut_m<-unique(poolcut_m)
poolcut_f<-ave(dpool$cutoff[dpool$gender==0],dpool$tre[dpool$gender==0],FUN=function(x) mean(x, na.rm=T))
poolcut_f<-unique(poolcut_f)

pdf("jointcut.pdf")
cell.barp<-barplot(cell.bar,beside=T,ylim=c(0,.8),border="white")
lines(x = cell.barp[1,]+1, y = poolcut_m/101,lty=2,lwd=2)
lines(x = cell.barp[1,]+1, y = poolcut_f/101,lty=1,lwd=3)
legend(0.6,0.8,inset=.02,legend=c("HH","HD","DD"),fill=gray.colors(3),bty = "n",border=F,y.intersp=.8,cex=.9)
text(cell.barp[1,3]+1.5,0.77,"cutoff male",cex = .8)
text(cell.barp[1,3],0.6,"cutoff female",cex = .8)
dev.off()


table(dpool$cell[dpool$tree==1],dpool$movewhere[dpool$tree==1])

table(dpool$play[dpool$tree==1 & dpool$gender==0],dpool$move[dpool$tree==1  & dpool$gender==0])




poolchoice<-ave(dpool$play,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolchoice<-unique(poolchoice)

poolpayoff<-ave(dpool$payoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolpayoff<-unique(poolpayoff)



