### It saves the raw data from REBEL lab
### The output file is hdall.Rda
### Recall that there are three treatments CGO, CGS and CGE. 

rm(list = ls())
setwd("/cloud/project/data/CGO")
files = list.files(pattern="exp*")
library(xlsx)

d<-NULL
for (i in seq_along(files)) {
  filename = files[[i]]
  df<-read.xlsx(filename,1)
  df<-df[1:132,]
  df<-as.data.frame(df[,c(1,16,17,18,19,21,23,24,27)])
  names(df)<-c('id','gender','cutoff','play','move','ra_pi','payoff','group','period')
  df$session<-10+i
  df$play<-as.numeric(df$play)-1 #B is 1 and A is 0
  df$gender<-as.numeric(df$gender)-1 #male is 1 female is 0 
  df$gender<-ave(df$gender,df$id,FUN=function(x) max(x, na.rm=T))
  d<-rbind(d,df)
}

setwd("/cloud/project/data/CGS")
files = list.files(pattern="exp*")
for (i in seq_along(files)) {
  filename = files[[i]]
  df<-read.xlsx(filename,1)
  df<-df[1:132,]
  df<-as.data.frame(df[,c(1,16,17,18,19,21,23,24,27)])
  names(df)<-c('id','gender','cutoff','play','move','ra_pi','payoff','group','period')
  df$session<-20+i
  df$move<-as.numeric(df$move)-1 #dia is 0 and noche 1 
  df$play<-as.numeric(df$play)-1 #B is 1 and A is 0
  df$gender<-as.numeric(df$gender)-1 #male is 1 female is 0 
  df$gender<-ave(df$gender,df$id,FUN=function(x) max(x, na.rm=T))
  d<-rbind(d,df)
}

setwd("/cloud/project/data/CGE")
files = list.files(pattern="exp*")
for (i in seq_along(files)) {
  filename = files[[i]]
  df<-read.xlsx(filename,1)
  df<-df[1:132,]
  df<-as.data.frame(df[,c(1,16,17,18,19,21,23,24,27)])
  names(df)<-c('id','gender','cutoff','play','move','ra_pi','payoff','group','period')
  df$session<-30+i
  df$move<-as.numeric(df$move)-1 #dia is 0 and noche 1 
  df$play<-as.numeric(df$play)-1 #B is 1 and A is 0
  df$gender<-as.numeric(df$gender)-1 #men is 1 women is 0 
  df$gender<-ave(df$gender,df$id,FUN=function(x) max(x, na.rm=T))
  d<-rbind(d,df)
}

d$tre<-0
d$tre[d$session>20]<-1
d$tre[d$session>30]<-2
d$tres<-0
d$tres[d$tre==1]<-1
d$tree<-0
d$tree[d$tre==2]<-1
d$treo<-0
d$treo[d$tre==0]<-1
d$idu<-d$session*100+d$id
d$idg<-d$session*1000+d$period*10+d$group
save(d,file='hdall.Rda')
