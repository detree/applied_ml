rm(list=ls())
workdir <- "~/Documents/cs498_ml/hw7"
setwd(workdir)
library(glmnet)
#================================para====================================
namefile<- "Locations.txt"
locfile<- "Oregon_Met_Data.txt"
#================================para end================================
namedata_raw<- read.csv(namefile, sep=' ', header=TRUE)
locdata_raw<- read.csv(locfile, sep=' ', header=TRUE)
name_out_col<- c('SID', 'East_UTM', 'North_UTM')
loc_out_col<- c('SID', 'Year', 'Tmin_deg_C')
namedata<- namedata_raw[,name_out_col]
locdata<- locdata_raw[,loc_out_col]
loc_filter<- which(locdata[,3]>=9999)
locdata<- locdata[-loc_filter,]
avgtemp<-aggregate(locdata, list(SID=locdata[,1], Year=locdata[,2]), mean)
avgtemp<-avgtemp[,3:5]
tempdata<-matrix(data=0, nrow=nrow(namedata), ncol=3)
for(sid in 1:nrow(namedata)){
  tmpname<- namedata[which(namedata[,1]==sid),]
  tempdata[sid,c(1,2)]<- as.double(tmpname[1,c(2,3)])
  tempdata[sid,3]<- mean(avgtemp[which(avgtemp[,1]==sid), 3])
}
distmat<- as.matrix( dist(tempdata[,c(1,2)]) )
avgdist<- sum(distmat)/length(distmat)
h<-seq(avgdist/2, avgdist*2, length.out = 6)

basepnt<- 
