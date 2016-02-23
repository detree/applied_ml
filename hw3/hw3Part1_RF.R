#---------------set up dir, load library and read data from file------------------------------------
setwd('Documents/CS498/HW3')
rm(list=ls())
training_file<-read.csv('data.txt', header=FALSE, sep = '\t')
validation_data_146<-read.csv('validation_3.txt', header=FALSE, sep = '\t')
validation_raw_result<-read.csv('validation_3_solution.txt', header=FALSE, sep = ',')
library(klaR)
library(caret)
library(randomForest)
#---------------------------------------------------------------------------------------------------
training_file$V1 = factor(training_file$V1)
train_data_146 = training_file
#training_result = as.factor(training_file[,1])
validation_result = as.factor(validation_raw_result[,2])


#prepare data for svm
training_data<- train_data_146[,c(1:74)]
training_data[,c(2:74)]<- abs(training_data[,c(2:74)] - train_data_146[,c(75:147)])
validation_data<-validation_data_146[,c(1:73)]
validation_data<-abs(validation_data_146[,c(74:146)]-validation_data)

label_data= training_data[,1]
training_index = createDataPartition(y=label_data, p=.8, list=FALSE)
training_data_par = training_data[training_index,] 
test_data = training_data[-training_index,-1]
answer = training_data[-training_index,1]

training_data_p = training_data[,-1]
names(validation_data) = names(training_data_p)
#start random forest
rf<-randomForest(V1 ~., data = training_data_par , mtry = 5,ntree = 400,type = 'classification')
p<-predict(rf,test_data)
rf_accuracy= sum(p==answer)/(sum(p==answer)+ sum(!(p==answer)))
rf_accuracy
