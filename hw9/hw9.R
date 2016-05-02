rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw9"
setwd(workdir)
library(glmnet)
library(gdata)
#================================para====================================
gene_file <- "I2000.txt"
tissue_file <- "tissues.txt"
#================================para end================================
generaw<- read.xls(gene_file, header=FALSE, sep=' ')
tissueraw<- read.xls(tissue_file, header=FALSE, sep=' ')


#L1, alpha=1; L2, alpha=2
lmout<-cv.glmnet(mat, (bigy), family="binomial", alpha = 1, type.measure="class")
plot(lmout)
1-min(lmout$cvm)
lmout$lambda.min
lmout$nzero[which(lmout$lambda==lmout$lambda.min)]