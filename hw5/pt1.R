rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw5"
setwd(workdir)
library(klaR)
library(caret)
library(matrixStats)
library(qlcMatrix)
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

#randomly generate the delta
delta_init<-sample(1:clustern, docn, replace=TRUE)
delta=matrix(data=0, nrow = docn, ncol=clustern)
#delta_init<-kmeans(xik, clustern, iter.max=10, nstart=1)$cluster
for( i in 1:docn)
  delta[i,delta_init[i]]=1
#generate pjk, prob of word k in cluster j
# eachword_cnt<-array(data=0, dim=wordn)
# eachword_cnt<-colSums(xik)
# eachword_inj_cnt<-matrix(data=0, nrow=wordn, ncol=clustern)
# for( i in 1:docn ){
#   tcluster<-which(delta[i,]==1)
#   for( k in 1:wordn ){
#     eachword_inj_cnt[k,tcluster] = eachword_inj_cnt[k,tcluster] + xik[i, k]
#   }
# }
pjk<-matrix(data=0, nrow=clustern, ncol=wordn)
for( j in 1:clustern ){
  for( k in 1:wordn ){
    #pjk[j,k] = eachword_inj_cnt[k,j]/eachword_cnt[k]
    pjk[j,k] = sum(xik[delta_init==j,k])
  }
}
pjk = pjk/rowSums(pjk)
#generate pi_j
PIj<-array(data=0, dim=clustern)
PIj<-colSums(delta)/docn
Qold=1
Qnew=0
while(abs(Qold-Qnew)>0.01){
  logAj <- xik %*% t(log(pjk)) + t( matrix(nrow = clustern ,ncol = docn, data = log(PIj) ) )
  logAmax<-rowMax(logAj)@x
  right <- array(data = 0, dim = docn)
  for(i in 1:docn){
    right[i]<-logSumExp(logAj[i,]-logAmax[i])
  }
  logwij<-logAj - matrix(data=logAmax, nrow=docn, ncol=clustern) - matrix(data=right, nrow=docn, ncol=clustern)
  wij<-exp(logwij)
  pjk_new <- t(wij) %*% xik
  pjk_new <- pjk_new/rowSums(pjk_new)
  pjk_new[is.na(pjk_new)] <- 0
  pjk<-pjk_new
  Qold=Qnew
  Qnew=(sum(xik %*% t(log(pjk)) + t(matrix(data=log(PIj), nrow=clustern, ncol=docn))))
  print(c(Qold, Qnew))
}


