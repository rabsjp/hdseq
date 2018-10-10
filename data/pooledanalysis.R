rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

### Gender composition in each session
gendersession<-table(d$session,d$gender)
profitsession<-ave(d$payoff,d$session,FUN=function(x) mean(x, na.rm=T))
profitsession<-unique(profitsession)
totalobs<-apply(table(d$session,d$gender),1,sum)


## We work with periods 6-11 to account for learning
dpool<-d[d$period>5,]
dpool$firstmover<-0
dpool$cell<-ave(dpool$play,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$movewhere<-ave(dpool$move,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$gendergroup<-ave(dpool$gender,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$firstmover[dpool$move==0 & dpool$tre>0]<-dpool$play[dpool$move==0& dpool$tre>0]
dpool$firstmover<-ave(dpool$firstmover,dpool$idg,FUN=function(x) sum(x, na.rm=T))

table(dpool$play[dpool$movewhere==1 & dpool$move==1 & dpool$tree==1],dpool$firstmover[dpool$movewhere==1 & dpool$move==1 & dpool$tree==1])




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

###What outcomes (CELLs) we observe in CGS at different moves? 
table(dpool$cell[dpool$tree==1],dpool$movewhere[dpool$tree==1])

### Who moves first by gender? 
movegender<-table(dpool$gender[dpool$tree==1],dpool$move[dpool$tree==1])
colnames(movegender)<-c("1st","2nd")

##For those that pick first, what do they play? 
playfirstgender<-table(dpool$gender[dpool$tree==1 & dpool$move==0],dpool$play[dpool$tree==1 & dpool$move==0])
colnames(playfirstgender)<-c("1st H","1st D")
playsecondgender<-table(dpool$gender[dpool$tree==1 & dpool$move==1],dpool$play[dpool$tree==1 & dpool$move==1])
colnames(playsecondgender)<-c("2nd H","2nd D")

pdf("countgendermoveplay.pdf")
barplot((cbind(movegender,playfirstgender,playsecondgender)/sum(movegender)),beside=T,border="white",ylim=c(0,0.4))
legend(12,0.3,inset=.02,legend=c("female","male"),fill=gray.colors(2),bty = "n",border=F,y.intersp=1.5,cex=1.2)
dev.off()

##For those that pick second and its a one-shot, what do they play? 
playsecondogender<-table(dpool$play[dpool$tree==1 & dpool$move==1 & dpool$movewhere==2],dpool$gender[dpool$tree==1 & dpool$move==1  & dpool$movewhere==2])
barplot(cbind(playfirstgender,playsecondogender),beside=T)

##For those that pick second and its a seq, what do they play? 
table(dpool$play[dpool$tree==1 & dpool$move==1 & dpool$movewhere==1],dpool$gender[dpool$tree==1 & dpool$move==1  & dpool$movewhere==1])


poolchoice<-ave(dpool$play,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolchoice<-unique(poolchoice)

poolpayoff<-ave(dpool$payoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolpayoff<-unique(poolpayoff)

### Regressions
dreg<-dpool[dpool$tree==1 & dpool$move==0,]
mylogit <- lm(play ~ play, data = dereg, family = "binomial")
m <- lm(play ~ gender, data = dreg)
library(sandwich)


