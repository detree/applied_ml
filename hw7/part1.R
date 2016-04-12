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
temperature_colnum<- 3
avgtemp<-aggregate(locdata, list(SID=locdata[,1], Year=locdata[,2]), mean)
temp2000<-matrix(nrow=nrow(namedata), ncol=temperature_colnum)
temp2001<-matrix(nrow=nrow(namedata), ncol=temperature_colnum)
temp2002<-matrix(nrow=nrow(namedata), ncol=temperature_colnum)
temp2003<-matrix(nrow=nrow(namedata), ncol=temperature_colnum)
temp2004<-matrix(nrow=nrow(namedata), ncol=temperature_colnum)
for(i in 1:nrow(namedata)){
  
  tmpname<- namedata[which(namedata[,1]==i),]
  tmploc<- locdata[which(locdata[,1]==i),]
  year_filter<- which(tmploc[,2]==2000)
  temp2000[i,]<- c( tmpname[,c('East_UTM', 'North_UTM')], mean(tmploc[year_filter,3]) )
  year_filter<- which(tmploc[,2]==2001)
  temp2001[i,]<- c( tmpname[,c('East_UTM', 'North_UTM')], mean(tmploc[year_filter,3]) )
  year_filter<- which(tmploc[,2]==2002)
  temp2002[i,]<- c( tmpname[,c('East_UTM', 'North_UTM')], mean(tmploc[year_filter,3]) )
  year_filter<- which(tmploc[,2]==2003)
  temp2003[i,]<- c( tmpname[,c('East_UTM', 'North_UTM')], mean(tmploc[year_filter,3]) )
  year_filter<- which(tmploc[,2]==2004)
  temp2004[i,]<- c( tmpname[,c('East_UTM', 'North_UTM')], mean(tmploc[year_filter,3]) )
}