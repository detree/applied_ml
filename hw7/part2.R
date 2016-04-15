rm(list=ls())
workdir <- "~/Desktop/applied_ml/hw7"
setwd(workdir)
library(glmnet)
library(MASS)
#================================para====================================
namefile<- "Locations.txt"
locfile<- "Oregon_Met_Data.txt"
scale_index <- 1
#================================para end================================
namedata_raw<- read.csv(namefile, sep=' ', header=TRUE)
locdata_raw<- read.csv(locfile, sep=' ', header=TRUE)
name_out_col<- c('SID', 'East_UTM', 'North_UTM')
loc_out_col<- c('SID', 'Year', 'Tmin_deg_C')
namedata<- namedata_raw[,name_out_col]
namedata[,2:3]<-namedata[,2:3]/1000
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

basepnt<- as.matrix(namedata[,c(2,3)])
train_data<- matrix(nrow = dim(basepnt)[1], ncol = dim(basepnt)[1])
train_data_scale <- matrix(nrow = dim(basepnt)[1], ncol = dim(basepnt)[1])
for(i in 1 : nrow(train_data)){
  for(j in 1 : ncol(train_data)){
    train_data[i,j] <- sqrt((basepnt[i,1] - basepnt[j,1])^2 + (basepnt[i,2] - basepnt[j,2])^2)
  }
}
train_data_scale <- train_data/h[scale_index]
train_data_scale = 1/sqrt(2*pi)*exp((-1)*train_data_scale^2)

#model = glmnet(train_data_scale, tempdata[,3], alpha = 1, lambda = 0)

lasso = cv.glmnet(as.matrix(train_data_scale), tempdata[,3], alpha = 1)
plot(lasso)
new_lambda = lasso$lambda.min

min_east = min(tempdata[,1])
max_east = max(tempdata[,1])
min_north = min(tempdata[,2])
max_north = max(tempdata[,2])

new_east <- seq(min_east, max_east,length.out = 100)
new_north <- seq(min_north, max_north,length.out = 100)
new_pred_x <- matrix(nrow = 10000, ncol = 2)
for(i in 1:100){
  for(j in 1:100){
    new_pred_x[(i-1)*100+j,1] = new_east[i]
    new_pred_x[(i-1)*100+j,2] = new_north[j]
  }
}

new_train_x = matrix(nrow = 10000, ncol = 112)
for (i in 1:10000){
  for (j in 1:112){
    tmp1 = new_pred_x[i,1] - basepnt[j,1]
    tmp2 = new_pred_x[i,2] - basepnt[j,2]
    new_train_x[i,j] = sqrt(tmp1^2+tmp2^2)
  }
}
new_train_x = new_train_x/h[scale_index]
new_train_x = 1/sqrt(2*pi)*exp((-1)*new_train_x^2)
#perd_rsl <- predict(model, new_train_x)
perd_rsl <- predict(lasso, new_train_x , s = new_lambda)


zmat = matrix(0 , nrow=100, ncol=100)
ptr=1
for(i in 1: 100){ 
  for(j in 1: 100){
    zmat[i, j] <- perd_rsl[ptr]
    ptr <- ptr+1
  }
}

totdist<-sum(dist(zmat))/10000
totdist
contour(new_east, new_north, zmat, color = terrain.colors, axes = TRUE,
               xlab ="East",ylab = "North")

