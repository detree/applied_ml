rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw9"
setwd(workdir)
library(glmnet)
library(gdata)
library(ROCR)
#================================para====================================
gene_file <- "I2000.txt"
tissue_file <- "tissues.txt"
#================================para end================================
generaw<- read.csv(gene_file, header=FALSE, sep=' ')
tissueraw<- read.csv(tissue_file, header=FALSE, sep=' ')

bigx<- data.matrix(t(generaw))
bigy<- as.list(t(tissueraw))
for(i in 1:length(bigy)){
  if(bigy[i]>0){
    bigy[i]=1
  }
  else{
    bigy[i]=0
  }
}
    

#L1, alpha=1; L2, alpha=2
lmout<-cv.glmnet(bigx, as.numeric(bigy), family="binomial", alpha = 1, type.measure="class")
plot(lmout)
1-min(lmout$cvm)
lmout$lambda.min
lmout$nzero[which(lmout$lambda==lmout$lambda.min)]
