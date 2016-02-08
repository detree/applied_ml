setwd('/Users/SC/Documents/cs498_ml/hw1')
getwd()
rm(list=ls())
rawdata<-read.csv('diabete.data', header = FALSE)
library(caret)
library(klaR)
oldbigx<-rawdata[,-c(9)]
bigx<-oldbigx
bigy<-rawdata[,9]
TrainScore<-array(dim=10)
TestScore<-array(dim=10)

for (i in c(3, 4, 6, 8))
{
  vw<-oldbigx[, i]==0
  bigx[vw, i]=NA
}

for (i in 1:1)
{
  #get the train data set
  trdata<-createDataPartition(y=bigy, p=.8, list=FALSE)
  trainx<-bigx[trdata, ]
  trainy<-bigy[trdata]
  trposflag<-(trainy>0)
  postrx<-trainx[trposflag, ]
  negtrx<-trainx[!trposflag, ]
  testx<-bigx[-trdata, ]
  testy<-bigy[-trdata]
  
  #train: get intermidiate results
  ptrmean<-sapply(postrx, mean, na.rm=TRUE)
  ntrmean<-sapply(negtrx, mean, na.rm=TRUE)
  ptrstd<-sapply(postrx, sd, na.rm=TRUE)
  ntrstd<-sapply(negtrx, sd, na.rm=TRUE)
  ptroffsets<-t(t(trainx)-ptrmean)
  ptrscales<-t(t(ptroffsets)/ptrstd)
  ntroffsets<-t(t(trainx)-ntrmean)
  ntrscales<-t(t(ntroffsets)/ntrstd)
  #train: final results
  ptrlogs<--(1/2)*rowSums(apply(ptrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(sqrt(2*pi)*log(ptrstd))+nrow(postrx)/nrow(trainx)
  ntrlogs<--(1/2)*rowSums(apply(ntrscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(sqrt(2*pi)*log(ntrstd))+nrow(negtrx)/nrow(trainx)
  traspos<-ptrlogs>ntrlogs
  trgotright<-traspos==trainy
  TrainScore[i]<-sum(trgotright)/(sum(trgotright)+sum(!trgotright))
  
  #test: get intermidiate results
  pteoffsets<-t(t(testx)-ptrmean)
  ptescales<-t(t(pteoffsets)/ptrstd)
  nteoffsets<-t(t(testx)-ntrmean)
  ntescales<-t(t(nteoffsets)/ntrstd)
  #test: final results
  ptelogs<--(1/2)*rowSums(apply(ptescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(sqrt(2*pi)*log(ptrstd))+nrow(postrx)/nrow(trainx)
  ntelogs<--(1/2)*rowSums(apply(ntescales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(sqrt(2*pi)*log(ntrstd))+nrow(negtrx)/nrow(trainx)
  teaspos<-ptelogs>ntelogs
  tegotright<-teaspos==testy
  TestScore[i]<-sum(tegotright)/(sum(tegotright)+sum(!tegotright))
}
print(mean(TestScore))
print(TestScore)
