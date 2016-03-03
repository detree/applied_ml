rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw4"
setwd(workdir)
library(klaR)
library(caret)
#================================para====================================
data_file <- "wine.data"
#================================para end================================
rawdata<-read.csv(data_file, header=FALSE, sep = ",")
bigx<-rawdata[,-c(1)]
bigy<-rawdata[,1]
#center the x
bigx<-scale(bigx,scale = FALSE)
covmat<-t(bigx)%*%(bigx)
eigenv<-eigen(covmat) #the eigenxxx value and vector.
eVal<-eigenv$values
eVec<-eigenv$vectors
#lambda<-t(eVec)%*%covmat%*%eVec
#colnames(eVec)<-c("Alcohol", "Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
plot(eVec[,1], type="h",main="first princinple component", ylab="value")
plot(eVec[,2], type="h",main="second princinple component", ylab="value")
plot(eVec[,3], type="h",main="third princinple component", ylab="value")
#plot(eVal, xlab="index", ylab="eigenvalue", type="l")

PCs<-prcomp(bigx,center=TRUE, scale.=FALSE)
proj_of2 <-PCs$x[,c(1,2)]
labels<-c('1','2','3')
color<-c('red','green','blue','yellow','orange')
pchr<-c(1, 2, 3)
ss<-expand.grid(kinds=1:3)
parset<-with(ss,simpleTheme(pch = pchr[kinds],
                            col = color[kinds]))
splom(proj_of2,groups=bigy, par.settings=parset, varnames=c('pc1','pc2'),
      key = list(text = list(labels), points = list(pch = pchr),columns=3))