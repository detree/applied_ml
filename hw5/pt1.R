rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw5"
setwd(workdir)
library(klaR)
library(caret)
library(matrixStats)
#================================para====================================
data_file <- "docword.nips.txt"
clustern=30
smooth_const=1
#================================para end================================
rawd<-read.csv(data_file, sep=' ', header=FALSE)
docn<-rawd[1,1]
wordn<-rawd[2,1]
NNZ<-rawd[3,1]
#document i, word k, topic j
xik=matrix(data=0, nrow=docn, ncol=wordn)
for( i in 4:nrow(rawd) )
  xik[rawd[i,1], rawd[i,2]] = rawd[i,3]
xik<-xik+smooth_const
xk<-colSums(xik)
delta=matrix(data=0, nrow = docn, ncol=clustern)

#randomly generate the delta
delta_init<-sample(1:clustern, docn, replace=TRUE)
for( i in 1:docn)
  delta[i,delta_init[i]]=1
#generate pjk, prob of word k in cluster j
eachword_cnt<-array(data=0, dim=wordn)
eachword_cnt<-colSums(xik)
eachword_inj_cnt<-matrix(data=0, nrow=wordn, ncol=clustern)
for( i in 1:docn ){
  tcluster<-which(delta[i,]==1)
  for( k in 1:wordn ){
    eachword_inj_cnt[k,tcluster] = eachword_inj_cnt[k,tcluster] + xik[i, k]
  }
}
pjk<-matrix(data=0, nrow=clustern, ncol=wordn)
for( j in 1:clustern ){
  for( k in 1:wordn ){
    pjk[j,k] = eachword_inj_cnt[k,j]/eachword_cnt[k]
  }
}
#generate pi_j
PIj<-array(data=0, dim=clustern)
PIj<-colSums(delta)/docn

for( loop in 1:200){
  logAj <- xik %*% t(log(pjk))
}


