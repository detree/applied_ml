rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw4"
setwd(workdir)
library(klaR)
library(caret)
library(MASS)
library(scatterplot3d)
#================================para====================================
data_file <- "wdbc.data"
#================================para end================================
rawdata<-read.csv(data_file, header=FALSE, sep = ",")
bigx = rawdata[,-c(1:2)]
bigy = rawdata[,2]

# center the data and get the first and the second principals
analyze_principal<-prcomp(bigx, center=TRUE, scale.=FALSE)
select_principal<-analyze_principal$x[,c(1:2)]

#====================part(a) below=========================
speciesnames<-c('Malignant','Benign')
pchr<-c(1, 2, 3)
color<-c('red','green','blue','yellow','orange')
ss<-expand.grid(species=1:3)
parset<-with(ss,simpleTheme(pch = pchr[species],
                            col = color[species]))
splom(select_principal,groups=bigy,
      par.settings=parset,
      varnames=c('principal1','principal2'),
      key = list(text = list(speciesnames),
                 points = list(pch = pchr),columns=3))
#====================part(b) below=========================
plsy<- matrix(nrow = nrow(rawdata), ncol = 2, data=0)
ptr_toB<-which(bigy=='B')
plsy[ptr_toB, 1] <- 1
plsy[-ptr_toB, 2] <- 1
#plsy<-scale(plsy, scale = FALSE)
#nbigx<-data.matrix(bigx)
#pls32<-pls1_nipals(nbigx, bigy, 3)

pls3<-pls2_nipals (bigx,plsy,3)
result <- pls3$T

colr<-vector(length = nrow(rawdata))
colr[ptr_toB]<- "red"
colr[-ptr_toB]<- "green"
plot = scatterplot3d(result[,1],result[,3],result[,2], main="3D Scatterplot",color=colr, xlab = 'pc1',ylab = 'pc3',zlab = 'pc2')
legend(plot$xyz.convert(1300, 22, 500),col= c("red", "green"), bg="white",pch = c(1,1),legend = c("Malignant","Benign"), cex = 1.1)









