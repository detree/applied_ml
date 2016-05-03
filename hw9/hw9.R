rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw9"
setwd(workdir)
library(glmnet)
library(caret)
library(ROCR)
#================================para====================================
gene_file <- "I2000.txt"
tissue_file <- "tissues.txt"
#================================para end================================
generaw<- read.csv(gene_file, header=FALSE, sep=' ')
tissueraw<- read.csv(tissue_file, header=FALSE, sep=' ')
#get x and labels
bigx<- data.matrix(t(generaw))
bigy<- as.list(t(tissueraw))
#generate the two-level label
for(i in 1:length(bigy)){
  if(bigy[i]<0){
    bigy[i]=1
  }
  else{
    bigy[i]=0
  }
}
    
#for the AOC plot
lmout<-cv.glmnet(bigx, as.numeric(bigy), family="binomial", alpha = 1, type.measure="auc", nfolds = 5)
plot(lmout)
#get average AOC.
summary(lmout$cvm)
lmout2$nzero[which(lmout$cvm==max(lmout$cvm))]
test <- predict(lmout,type="response", newx = bigx, s = 'lambda.min')

#code for plot the ROC plot
testlen<-10000
thresholds<-seq(from=0, to=1, length.out = testlen)
roc<-matrix( nrow = length(thresholds), ncol=2 )
for(i in 1:length(thresholds)){
  posidx=which(bigy>0)
  negidx=which(bigy==0)
  #different values used in calculate ROC
  truepos<-sum(test[posidx]>thresholds[i])
  falsepos<-sum(test[negidx]>thresholds[i])
  trueneg<-sum(test[negidx]<thresholds[i])
  falseneg<-sum(test[posidx]<thresholds[i])
  roc[i,1]<-truepos/(truepos+falseneg)
  roc[i,2]<-falsepos/(falsepos+trueneg)
}
plot(x=roc, type='l', xlab='true positive rate', ylab='false positive rate')

#for the deviance and err rate
lmout2<-cv.glmnet(bigx, as.numeric(bigy), family="binomial", alpha = 1, type.measure="deviance", nfolds = 5)
summary(lmout2$cvm)
lmout2$lambda.min
lmout2$nzero[which(lmout2$lambda==lmout2$lambda.min)]
plot(lmout2)
