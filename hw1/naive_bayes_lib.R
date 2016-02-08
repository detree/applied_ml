setwd('/Users/SC/Documents/cs498_ml/hw1')
rm(list=ls())
rawdata<-read.csv('diabete.data', header = FALSE)
library(caret)
library(klaR)
bigx<-rawdata[,-c(9)]
bigy<-as.factor(rawdata[,9])

#get the train data set
trdata<-createDataPartition(y=bigy, p=.8, list=FALSE)
trainx<-bigx[trdata, ]
trainy<-bigy[trdata]
testx<-bigx[-trdata, ]
testy<-bigy[-trdata]

#using the library to train and test
#trctrl<-trainControl(method='cv', number=10)
model<-train(trainx, trainy, 'nb', trctrl=trainControl(method='cv', number=10))
teclasses<-predict(model,newdata=bigx[-trdata,])
confusionMatrix(data=teclasses, bigy[-trdata])
