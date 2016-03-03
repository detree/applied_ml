setwd('Documents/CS498/HW4')
rm(list=ls())

irisdat<-read.csv('iris_data.txt',header = FALSE)
library('lattice')
numiris = irisdat[,-c(5)]

# center the data and get the first and the second principals
analyze_principal<-prcomp(numiris, center=TRUE, scale.=FALSE)
select_principal<-analyze_principal$x[,c(1:2)]


postscript("two_principal.eps")

speciesnames<-c('setosa','versicolor','virginica')
pchr<-c(1, 2, 3)
color<-c('red','green','blue','yellow','orange')
ss<-expand.grid(species=1:3)
parset<-with(ss,simpleTheme(pch = pchr[species],
                            col = color[species]))
splom(select_principal,groups=irisdat$V5,
      par.settings=parset,
      varnames=c('principal1','principal2'),
      key = list(text = list(speciesnames),
                 points = list(pch = pchr),columns=3))
dev.off()
