setwd('/Users/SC/Documents/cs498_ml/hw3')
rm(list=ls())
rawdata<-read.csv('train_50k.txt', header = FALSE, sep = "\t")
library(caret)
library(klaR)
bigx<-rawdata[,-c(1)]
bigy<-as.factor(rawdata[,1])

#get the train data set
trdata<-createDataPartition(y=bigy, p=.8, list=FALSE)
trainx<-bigx[trdata, ]
trainy<-bigy[trdata]
testx<-bigx[-trdata, ]
testy<-bigy[-trdata]

#using the library to train and test
#trctrl<-trainControl(method='cv', number=10)
model<-train(trainx, trainy, 'nb', trctrl=trainControl(method='cv', number=1))
teclasses<-predict(model,newdata=bigx[-trdata,])
confusionMatrix(data=teclasses, bigy[-trdata])
