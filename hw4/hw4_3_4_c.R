#r code for scatterplot of iris data
setwd("~/Documents/cs498_ml/hw4")
rm(list=ls())
irisdat<-read.csv('iris.data',header = FALSE)
library('lattice')
library('MASS')
library('chemometrics')
numiris = irisdat[,c(1:4)]
label <- irisdat[,c(5)]

#center data
numiris<-scale(numiris, scale = FALSE)

#calculate y for PLS1
y_value <- matrix(nrow = nrow(irisdat), ncol = 3)
for(i in 1 : nrow(y_value)){
  if(label[i] == 'Iris-setosa')
    y_value[i,1] = TRUE
  else
    y_value[i,1] = FALSE
    
  if(label[i] == 'Iris-versicolor')
    y_value[i,2] = TRUE
  else
    y_value[i,2] = FALSE
  
  if(label[i] == 'Iris-virginica')
    y_value[i,3] = TRUE
  else
    y_value[i,3] = FALSE
}

y <- matrix(ncol = 3, nrow = nrow(irisdat),data = 0)
y[y_value] = 1
y<-scale(y, scale = FALSE)

pls2<-pls2_nipals (numiris,y,2)
result <- pls2$T
postscript("principal_PLS1.eps")

speciesnames<-c('setosa','versicolor','virginica')
color<-c('red','green','blue','yellow','orange')
pchr<-c(1, 2, 3)
ss<-expand.grid(species=1:3)
species=1:3
parset<-with(ss,simpleTheme(pch = pchr[species],
                            col = color[species]))
splom(result,groups=irisdat$V5,
      par.settings=parset,
      varnames=c('pr1','pr2'),
      key = list(text = list(speciesnames),
                 points = list(pch = pchr),columns=3))

dev.off()