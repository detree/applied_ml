#---------------set up dir, load library and read data from file------------------------------------
setwd('Documents/CS498/HW3')
rm(list=ls())
training_file<-read.csv('data.txt', header=FALSE, sep = '\t')
validation_data_146<-read.csv('validation_3.txt', header=FALSE, sep = '\t')
validation_raw_result<-read.csv('validation_3_solution.txt', header=FALSE, sep = ',')
library(klaR)
library(caret)
#---------------------------------------------------------------------------------------------------

train_data_146 = training_file[,-c(1)]
training_result = as.factor(training_file[,1])
validation_result = as.factor(validation_raw_result[,2])

#prepare data for svm
training_data = matrix(data = NA, nrow = nrow(train_data_146), ncol = ncol(train_data_146)/2)
validation_data = matrix(data = NA, nrow = nrow(validation_data_146), ncol = ncol(validation_data_146)/2)

for(i in 1:ncol(training_data)){
  training_data[,i] = abs(train_data_146[,i] - train_data_146[,i+73])
  validation_data[,i] = abs(validation_data_146[,i]- validation_data_146[,i+73])
}

training_index = createDataPartition(y=training_result, p=.8, list=FALSE)
label_data = training_result[training_index]
training_data_par = training_data[training_index,] 
test_data = training_data[-training_index,]
answer = training_result[-training_index]

#apply nb now
nb_model<-train(training_data_par, label_data, 'nb', trControl=trainControl(method='cv', number=5))
hahaha<-predict(nb_model,newdata=test_data)
score = confusionMatrix(data=hahaha, answer)

score
