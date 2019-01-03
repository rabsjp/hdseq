### It contains analysis using pooled data
### The data from sessions are in /data folder

rm(list = ls())
setwd("/cloud/project/data/")
load("hdall.Rda")

### Table with gender composition in each session
gendersession<-table(d$session,d$gender)
profitsession<-ave(d$payoff,d$tre,FUN=function(x) mean(x, na.rm=T))
profitsession<-unique(profitsession)
totalobs<-apply(table(d$session,d$gender),1,sum)


### Creating important variables
#dpool<-d[d$period>5,]
dpool<-d
dpool$firstmover<-0
dpool$cell<-ave(dpool$play,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$dd<-0
dpool$dd[dpool$cell==2]<-1
dpool$movewhere<-ave(dpool$move,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$gendergroup<-ave(dpool$gender,dpool$idg,FUN=function(x) sum(x, na.rm=T))
dpool$firstmover[dpool$move==0 & dpool$tre>0]<-dpool$play[dpool$move==0& dpool$tre>0]
dpool$firstmover<-ave(dpool$firstmover,dpool$idg,FUN=function(x) sum(x, na.rm=T))

## Analyzing choices of first mover and gender
table(dpool$play[dpool$movewhere==1 & dpool$move==1 & dpool$tres==1],dpool$firstmover[dpool$movewhere==1 & dpool$move==1 & dpool$tres==1])/396
table(dpool$gender[dpool$movewhere==1 & dpool$move==1 & dpool$tres==1 & dpool$firstmover==1],dpool$play[dpool$movewhere==1 & dpool$move==1 & dpool$tres==1 & dpool$firstmover==1])

## Frequency of cells 
cello<-table(dpool$cell[dpool$treo==1])/length(dpool$cell[dpool$treo==1])
cells<-table(dpool$cell[dpool$tres==1])/length(dpool$cell[dpool$tres==1])
celle<-table(dpool$cell[dpool$tree==1])/length(dpool$cell[dpool$tree==1])
cell.bar<-cbind(cello,cells,celle)
colnames(cell.bar)<-c("CGO","CGS","CGE")

### Creating cutoff choices. Mean by treatment 
poolcut<-ave(dpool$cutoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolcut<-unique(poolcut)

poolcut_m<-ave(dpool$cutoff[dpool$gender==1],dpool$tre[dpool$gender==1],FUN=function(x) mean(x, na.rm=T))
poolcut_m<-unique(poolcut_m)
poolcut_f<-ave(dpool$cutoff[dpool$gender==0],dpool$tre[dpool$gender==0],FUN=function(x) mean(x, na.rm=T))
poolcut_f<-unique(poolcut_f)


## Plot of cutoffs and cell choices
pdf("jointcut.pdf")
cell.barp<-barplot(cell.bar,beside=T,ylim=c(0,.8),border="white")
lines(x = cell.barp[1,]+1, y = poolcut_m/101,lty=2,lwd=2)
lines(x = cell.barp[1,]+1, y = poolcut_f/101,lty=1,lwd=3)
legend(0.6,0.8,inset=.02,legend=c("HH","HD","DD"),fill=gray.colors(3),bty = "n",border=F,y.intersp=.8,cex=.9)
text(cell.barp[1,3]+1.5,0.77,"men cutoff",cex = .8)
text(cell.barp[1,3],0.6,"women cutoff ",cex = .8)
dev.off()

### Table of outcomes in CGE treatment
cellmove<-round(table(dpool$cell[dpool$tree==1],dpool$movewhere[dpool$tree==1])/sum(table(dpool$movewhere[dpool$tree==1]))*100,2)
rownames(cellmove)<-c("HH","HD","DD")
colnames(cellmove)<-c("oneshot 1st","seq.","oneshot 2nd")

### Who moves first by gender? 
movegender<-table(dpool$gender[dpool$tree==1],dpool$move[dpool$tree==1])
colnames(movegender)<-c("1st","2nd")

##For those that pick first, what do they play? 
playfirstgender<-table(dpool$gender[dpool$tree==1 & dpool$move==0],dpool$play[dpool$tree==1 & dpool$move==0])
colnames(playfirstgender)<-c("1st H","1st D")
playsecondgender<-table(dpool$gender[dpool$tree==1 & dpool$move==1],dpool$play[dpool$tree==1 & dpool$move==1])
colnames(playsecondgender)<-c("2nd H","2nd D")


## Plot of order of play and gender
pdf("countgendermoveplay.png")
barplot((cbind(movegender,playfirstgender,playsecondgender)/sum(movegender)),beside=T,border="white",ylim=c(0,0.4))
legend(12,0.3,inset=.02,legend=c("women","men"),fill=gray.colors(2),bty = "n",border=F,y.intersp=1.5,cex=1.2)
dev.off()

## Creating variables by gender 
playgendero<-table(dpool$play[dpool$treo==1],dpool$gender[dpool$treo==1])
playgenders<-table(dpool$play[dpool$tres==1],dpool$gender[dpool$tres==1])
playgender<-table(dpool$play[dpool$tree==1],dpool$gender[dpool$tree==1])
playo<-playgendero[2,]/apply(playgendero,2,sum)*100
plays<-playgenders[2,]/apply(playgenders,2,sum)*100
playe<-playgender[2,]/apply(playgender,2,sum)*100
play.bar<-cbind(playo,plays,playe)
colnames(play.bar)<-c("CGO","CGS","CGE")

## Plot of gender and play
pdf("genderplay.png")
play.barp<-barplot(play.bar,beside=T,ylim=c(0,100),border="white")
legend(2.0,100,inset=.1,legend=c("% women playing D","% men playing D"),fill=gray.colors(2),bty = "n",border=F,y.intersp=2,cex=0.9)
dev.off()

##For those that pick second and its a one-shot, what do they play? 
playsecondogender<-table(dpool$play[dpool$tree==1 & dpool$move==1 & dpool$movewhere==2],dpool$gender[dpool$tree==1 & dpool$move==1  & dpool$movewhere==2])
barplot(cbind(playfirstgender,playsecondogender),beside=T)

##For those that pick second and its a seq, what do they play? 
table(dpool$play[dpool$tree==1 & dpool$move==1 & dpool$movewhere==1],dpool$gender[dpool$tree==1 & dpool$move==1  & dpool$movewhere==1])

## Average of choices and payoffs 
poolchoice<-ave(dpool$play,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolchoice<-unique(poolchoice)

poolpayoff<-ave(dpool$payoff,dpool$tre,FUN=function(x) mean(x, na.rm=T))
poolpayoff<-unique(poolpayoff)

### Regressions. Not reported in the paper. 
dreg<-dpool[dpool$treo==1,]
mylogit <- lm(play ~ gender, data = dereg, family = "binomial")
m <- lm(play ~ gender, data = dreg)
m <- lm(payoff ~ gender, data = dreg)

###Non-parametric tests. Included in the paper. 

##DD per session
dpool$tredd<-ave(dpool$dd,dpool$session,FUN=function(x) mean(x, na.rm=T))
testdd<-unique(dpool[,c("session","tredd")])

ks.test(testdd$tredd[testdd$session<20],testdd$tredd[testdd$session>30],alternative = c("g"))
wilcox.test(testdd$tredd[testdd$session<20],testdd$tredd[testdd$session>30],alternative = c("l"))

ks.test(testdd$tredd[testdd$session>20 & testdd$session<30],testdd$tredd[testdd$session>30],alternative = c("two.sided"))
wilcox.test(testdd$tredd[testdd$session>20 & testdd$session<30],testdd$tredd[testdd$session>30],alternative = c("two.sided"))


