rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw5"
setwd(workdir)
library(klaR)
library(caret)
#================================para====================================
data_file <- "docword.nips.txt"
clustern=30
#================================para end================================
rawd<-read.csv(data_file, sep=' ', header=FALSE)
docn<-rawd[1,1]
wordn<-rawd[2,1]
NNZ<-rawd[3,1]
xik=matrix(data=0, nrow=docn, ncol=wordn)
for( i in 4:nrow(rawd) )
  xik[rawd[i,1], rawd[i,2]] = rawd[i,3]
delta=matrix(data=0, nrow = docn, ncol=clustern)

#randomly generate the delta
delta_init<-sample(1:clustern, docn, replace=TRUE)
for( i in 1:docn)
  delta[i,delta_init[i]]=1
#generate pjk, prob of word j in cluster k
eachword_cnt<-array(data=0, dim=wordn)
eachword_cnt<-colSums(xik)
doc_ink<-matrix(nrow=clustern)
eachword_ink_cnt<-matrix(data=0, nrow=wordn, ncol=clustern)
for( i in 1:docn ){
  tcluster<-which(delta[i,]==1)
  for( j in 1:wordn ){
    eachword_ink_cnt[j,k]+=xik[i, j]
  }
}

pjk<-matrix(data=0, nrow=clustern, rcol=wordn)
