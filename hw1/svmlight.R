setwd('~/Documents/cs498_ml/hw1')
rm(list=ls())
wdat<-read.csv('diabete.data', header=FALSE)
library(klaR)
library(caret)
TestScore<-array(dim=5)

for (i in 1:5)
{
  bigx<-wdat[,-c(9)]
  bigy<-as.factor(wdat[,9])
  wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
  svm<-svmlight(bigx[wtd,], bigy[wtd], pathsvm='~/SVMlight')
  labels<-predict(svm, bigx[-wtd,])
  foo<-labels$class
  TestScore[i]<-sum(foo==bigy[-wtd])/(sum(foo==bigy[-wtd])+sum(!(foo==bigy[-wtd])))
}

print(mean(TestScore))