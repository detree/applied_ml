setwd('Documents/CS498/HW4')
rm(list=ls())

irisdat<-read.csv('iris_data.txt',header = FALSE)
library('lattice')
numiris = irisdat[,-c(5)]
postscript("irisscatterplot.eps")
#so the I get a postscript file
speciesnames<-c('setosa','versicolor','virginica')
pchr<-c(1, 2, 3)
color<-c('red','green','blue','yellow','orange')
ss<-expand.grid(species=1:3)
parset<-with(ss,simpleTheme(pch = pchr[species],
                            col = color[species]))
splom(irisdat[,c(1:4)],groups=irisdat$V5,
      par.settings=parset,
      varnames=c('sepal\nLength','sepal\nWidth',
                 'Petal\nLength','Petal\nWidth'),
      key = list(text = list(speciesnames),
                 points = list(pch = pchr),columns=3))
dev.off()
