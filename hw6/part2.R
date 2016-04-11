rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw6"
setwd(workdir)
library(glmnet)
library(gdata)
#================================para====================================
data_file <- "default of credit card clients.xls"
#================================para end================================
rawd<-read.xls(data_file, header=FALSE)
rawd2<-rawd[3:nrow(rawd),]
bigx<-rawd2[,2:24]
bigy<-as.numeric(rawd2[,25])-1
bigy<-data.matrix(bigy)
mat<-data.matrix(bigx)
mat[is.na(mat)]<-0

#L1, alpha=1; L2, alpha=2
lmout<-cv.glmnet(mat, (bigy), family="binomial", alpha = 0.75, type.measure="class")
plot(lmout)
1-min(lmout$cvm)
lmout$lambda.min
lmout$nzero[which(lmout$lambda==lmout$lambda.min)]