rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

## We work with periods 6-11 to account for learning
dpool<-d[d$period>5,]
dpool$cell<-ave(dpool$play,dpool$idg,FUN=function(x) sum(x, na.rm=T))
table(dpool$cell[dpool$treo==1])/sum(dpool$cell[dpool$treo==1])
table(dpool$cell[dpool$tres==1])/sum(dpool$cell[dpool$tres==1])
table(dpool$cell[dpool$tree==1])/sum(dpool$cell[dpool$tree==1])


poolcut<-ave(dpool$cutoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolcut<-unique(poolcut)

poolchoice<-ave(dpool$play,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolchoice<-unique(poolchoice)

poolpayoff<-ave(dpool$payoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolpayoff<-unique(poolpayoff)




mean(dpool)

