setwd('~/Documents/cs498_ml/hw1')
rm(list=ls())
rawdata<-read.csv('diabete.data', header = FALSE)
library(klaR)
library(caret)
bigy<-as.factor(rawdata[,9])
partition<-createDataPartition(y=bigy, p=.8, list=FALSE)
testData<-rawdata[partition, ]
for (i in c(3, 4, 6, 8))
{
  vw<-testData[, i]==0
  testData[vw, i]=NA
}
ntestData<-na.omit(testData)
testx<-ntestData[, -c(9)]
testy<-ntestData[,9]

