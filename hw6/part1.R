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
#outtry<-glmnet(mat, pos[,1], alpha = 0)
curry<-pos[,2]
lmout<-lm(formula = curry~mat, na.action = na.omit)

rslt<-summary.lm(lmout)
coeff<-coefficients(lmout)[2:117]
coeff[is.na(coeff)]<-0
coeff<-as.matrix(coeff)
ypred<-as.matrix(feature)%*%coeff


#==================plot for pure regression=====================
#plot(ypred, curry, type="n", las=1, xlab="predicted",
#                                 ylab="original y",
#                                 main="predicted-y vs. original-y")
#points(ypred, curry, pch=16, col="blue")
#with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch=17, col="blue"))
#===========for the box-cox===========

curry2<-curry+abs(min(curry))+1
brslt<-boxcox(lm(formula = curry2~mat), lambda = seq(0, 4, 1/1000))
lambda = brslt$x[ which.max(brslt$y) ]
newy<- (curry2^lambda-1)/lambda
lmout<-lm(formula = newy~mat, na.action = na.omit)
rslt<-summary.lm(lmout)
rslt
#divnum<-2000
#maxlambda<-50
#rsqr<-rep(0,divnum+1)
#for(lambda in seq(0, maxlambda, maxlambda/divnum)){
#  if(lambda!=0)
#    newy<- (curry2^lambda-1)/lambda
#  else
#    newy<- log(curry2)
#  lmout<-lm(formula = newy~mat, na.action = na.omit)
#  rslt<-summary.lm(lmout)
#  rsqr[lambda/(maxlambda/divnum)+1]<-rslt$adj.r.squared
#}
#plot(rsqr)
