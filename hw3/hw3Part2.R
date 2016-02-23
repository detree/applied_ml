#---------------set up dir, load library and read data from file------------------------------------
setwd('Documents/CS498/HW3')
rm(list=ls())
training_file<-read.csv('data.txt', header=FALSE, sep = '\t')
validation_data_146<-read.csv('validation_1.txt', header=FALSE, sep = '\t')
validation_raw_result<-read.csv('validation_1_solution.txt', header=FALSE, sep = ',')
face_file<-read.csv('pubfig_attr.txt', header=FALSE, sep = '\t')
library(klaR)
library(caret)
library(RANN)
library(gdata)
#---------------------------------------------------------------------------------------------------

train_data_146 = training_file[,-c(1)]
training_result = as.factor(training_file[,1])
validation_result = as.factor(validation_raw_result[,2])
face_data = face_file[,-c(2)]
face_solution = as.factor(face_data[,1])
face_data = face_data[,-c(1)]

#prepare data for knn
firPpl = train_data_146[1:73]
secPpl = train_data_146[74:146]
names <- colnames(firPpl)
colnames(secPpl)<-names
training_data = interleave(firPpl,secPpl)

firPpl = validation_data_146[1:73]
secPpl = validation_data_146[74:146]
names <- colnames(firPpl)
colnames(secPpl)<-names
validation_data = interleave(firPpl,secPpl)

#start knn now
two_nn<-nn2(face_data,query=training_data,k=1)
two_nn_idx <- two_nn$nn.idx
nn_result = vector(mode = 'integer', length = length(training_result))

for(i in 1:length(training_result)){
  if( face_solution[two_nn_idx[2*i-1]] == face_solution[two_nn_idx[2*i]] )
    nn_result[i]<-1
  else
    nn_result[i]<-0
}
result_correcctness <- nn_result == training_result
percentile<-sum(result_correcctness)/length(training_result)

percentile
