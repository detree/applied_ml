rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw6"
setwd(workdir)
library(glmnet)
library(MASS)
#================================para====================================
data_file <- "default_plus_chromatic_features_1059_tracks.txt"
#================================para end================================
rawd<-read.csv(data_file, sep=',', header=FALSE)
feature<-rawd[,1:116]
pos<-rawd[,117:118]

mat<-as.matrix(feature)
#L1, alpha=1; L2, alpha=2
lmout<-cv.glmnet(mat, pos[,2], alpha = 1)
plot(lmout)
min(lmout$cvm)
lmout$lambda.min
lmout$nzero[which(lmout$lambda==lmout$lambda.min)]