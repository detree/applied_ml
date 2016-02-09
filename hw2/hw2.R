setwd("/Users/Lunan/Documents/CS498/HW2")
rm(list=ls())
library(klaR)
library(caret)

#---------------initialize varablie-----------------------
a <- 0.05
b <- 100
epoch <- 50
step <- 300
main_a<-runif(6, -2, 2)
main_b<-runif(1, -2, 2)
answer <- c(TRUE, FALSE)
number <- c(1, -1)
train_accuracy <- 0
graph_size = 50 * 300 / 30
graph <- matrix(data=NA,nrow=4,ncol=graph_size)
#--------------------end------------------------------------

whole_file <- read.csv('data.txt', header = FALSE)
whole_data <- whole_file[, c(1, 3, 5, 11, 12, 13)]
whole_result <- whole_file[, 15]

file_idx<-createDataPartition(y = whole_result,p = 0.9,list =FALSE)
cont_data <- whole_data[file_idx, ]
cont_result <- whole_result[file_idx]

test_data <- whole_data[-file_idx, ]
test_result <- whole_result[-file_idx]
test_data<-scale(test_data)

#-------------------normal distribution----------------------
cont_mean <- sapply(cont_data,mean)
cont_sd <- sapply(cont_data,sd)
cont_offset <- t(t(cont_data)-cont_mean)
cont_data<-t(t(cont_offset)/cont_sd)
#----------------------end------------------------------------

# separate trainning and validation data
train_ind <- createDataPartition(y = cont_result,p = 0.8/0.9,list =FALSE)
train_data <- cont_data[train_ind,]
train_result <- cont_result[train_ind]

valid_data <- cont_data[-train_ind,]
valid_result <- cont_result[-train_ind]

#print(train_data[,1])
for(i in 0:-3){
  lambda = 10^i
  #print(lambda)
  for(j in 1:epoch){
    step_len <- 1/(a*j+b)
    
    #-----------------------drawing graph prepare--------------------------
    p_rate = 50/nrow(train_data)
    graph_idx<-createDataPartition(y = train_result, p = p_rate, list =FALSE)
    graph_data <- train_data[graph_idx,]
    graph_result <- train_result[graph_idx]
    #--------------------------------------------------------------
    
    for(k in 1:step){
      select_idx <- sample(1:length(train_data[,1]), 1)
      select_data <- train_data[select_idx,]
      answer[1] <- train_result[select_idx] == " >50K"
      answer[2] <- train_result[select_idx] == " <=50K"
      predict<-select_data * main_a
      predict <- number[answer] * (sum(predict) + main_b)

      predict <- predict >= 1
      if(predict){
        delta_a <- lambda * main_a
        delta_b <- 0
      }
      else{
        delta_a <- lambda * main_a -  number[answer] * select_data
        delta_b <- -number[answer]
      }
      main_a <- main_a - delta_a * step_len
      main_b <- main_b - delta_b * step_len
      
      
      #-------------------------------drawing graph data----------------------------
      if(k %% 30 == 0)
      {
        
        graph_predict <- t(t(graph_data)*main_a)
        
        graph_predict <- rowSums(graph_predict) + main_b
        graph_predict <- graph_predict > 0
        graph_answer <- graph_result == " >50K"
        
        graph_accuracy <- graph_predict == graph_answer
        graph_accuracy <- sum(graph_accuracy)/length(graph_accuracy)
        
        graph[lambda,(300 * (i - 1) + k)/30] <- graph_accuracy
      }
      #------------------------------end------------------------------------------------
    }#end of 300 steps
    
    valid_predict<-t(t(valid_data)*main_a)
    
    valid_predict<-rowSums(valid_predict) + main_b
    valid_predict<-valid_predict > 0
    valid_answer <- valid_result == " >50K"
    
    val_accuracy <- valid_predict == valid_answer
    val_accuracy <- sum(val_accuracy)/length(val_accuracy)
    if(val_accuracy > train_accuracy)
    {
      train_accuracy <- val_accuracy
      result_a <- main_a
      result_b <- main_b
    }
  }#end of 50 epoches
}#end of 10^i

#---------------------start testing now--------------------------------
test_predict<-t(t(test_data)*result_a)
test_predict<-rowSums(test_predict) + result_b
test_predict<-test_predict > 0
test_answer <- test_result == " >50K"
accuracy <- test_predict == test_answer
accuracy <- sum(accuracy)/length(accuracy)
accuracy
#-----------------------------------------------------------------------

#----------------------drawing graph------------------------------------
plot(1:length(graph[1,]),graph[1,],type = "l",	ylab="accuracy",ylim=c(0,1))
lines(1:length(graph[2,]),graph[2,],type = "l",col="red")
lines(1:length(graph[3,]),graph[3,],type = "l",col="green")
lines(1:length(graph[4,]),graph[4,],type = "l",col="gray")
#------------------------end of the program------------------------------