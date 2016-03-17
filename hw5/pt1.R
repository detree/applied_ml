rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw5"
setwd(workdir)
library(klaR)
library(caret)
library(matrixStats)
library(qlcMatrix)
#================================para====================================
data_file <- "docword.nips.txt"
vocab_file <- "vocab.nips.txt"
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
pjk<-matrix(data=0, nrow=clustern, ncol=wordn)
for( j in 1:clustern ){
  for( k in 1:wordn ){
    #pjk[j,k] = eachword_inj_cnt[k,j]/eachword_cnt[k]
    pjk[j,k] = sum(xik[delta_init==j,k])
  }
}
pjk<- pjk/rowSums(pjk)
#generate pi_j
PIj<- array(data=0, dim=clustern)
PIj<- colSums(delta)/docn
Qold=1
Qnew=0
while(abs(Qold-Qnew)>0.01){
  logAj<- xik %*% t(log(pjk)) + t( matrix(nrow = clustern ,ncol = docn, data = log(PIj) ) )
  logAmax<- rowMax(logAj)@x
  #init for the log(sig(e^(logAj-logAmax)))
  logpart<- array(data = 0, dim = docn)
  for(i in 1:docn){
    logpart[i]<-logSumExp(logAj[i,]-logAmax[i])
  }
  logwij<- logAj - matrix(data=logAmax, nrow=docn, ncol=clustern) - matrix(data=logpart, nrow=docn, ncol=clustern)
  wij<- exp(logwij)
  pjk_new<- t(wij) %*% xik
  pjk_new<- pjk_new/rowSums(pjk_new)
  pjk_new[is.na(pjk_new)] <- 0
  pjk<- pjk_new
  PIj_new<- colSums(wij)/docn
  PIj<- PIj_new
  Qold=Qnew
  Qnew=(sum(xik %*% t(log(pjk)) + t(matrix(data=log(PIj), nrow=clustern, ncol=docn))))
  cat( sprintf("old=%f, new=%f\n", Qold, Qnew) )
}

#the graph for each topic's probability
plot(c(1:clustern), PIj, xlab="cluster#", ylab="probability", type="h")
#the top10 word for each cluster
vocab<-read.csv(vocab_file, header=FALSE)
pjkcpy<-pjk
words<-matrix(data="", nrow=clustern, ncol=10)
for(j in 1:clustern){
  cat( sprintf("cluster%d: ", j) )
  for(cnt in 1:10){
    index = which.max(pjkcpy[j,])
    cat( sprintf("%s, ", vocab[index,1]) )
    pjkcpy[j,index]=-9999
  }
  cat( sprintf("\n") )
}
