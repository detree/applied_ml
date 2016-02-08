setwd("/Users/SC/Documents/cs498_ml/hw2")
rm(list=ls())
wdat<-read.csv('adult.data',header = FALSE)
library(klaR)
library(caret)

#initialize data and parameters
x <- 0.05
y <- 100

yi_more <- 1
yi_less <- -1

yi <- c(yi_more,yi_less)
training_Answer <- c(TRUE,FALSE)

a <- c(1, -3, 2, 1, -4, 5) 
b <- 10 
lenta <- 0.01

plot_size = 50 * 300 / 30
plot <-  matrix(data=NA,nrow=4,ncol=plot_size)
#get rid of discrete data
index <- c("V1","V3","V5","V11","V12","V13","V15")
wdat_con <- wdat[index]
pre_accuracy <- 0
#seperate X and Y
wdat_data <-wdat_con[,-c(7)]
wdat_result <-wdat_con[,7]

wdat_mean<-sapply(wdat_data,mean)
wdat_sd<-sapply(wdat_data,sd)
wdat_offsets<-t(t(wdat_data)-wdat_mean)
wdat_data<-t(t(wdat_offsets)/wdat_sd)
#seperate train test and validation data and result

train_ind<-createDataPartition(y = wdat_result,p = 0.8/0.9,list =FALSE)

train_result <- wdat_result [train_ind]
train_data <- wdat_data[train_ind,]

validation_result<-wdat_result[-train_ind]
validation_data <- wdat_data[-train_ind,]

#test_result <- no_train_result[test_ind]
#test_data <- no_train_data[test_ind,]
true_cnt<-0
#beginning training
for(try_lenta in 1:4){
  lenta = 1/(10^(try_lenta - 1)) 
  a <- c(1, -3, 2, 1, -4, 5) 
  b <- 10 
  for(epoch in 1:50){
    the_p = 50/length(train_result)
    plot_ind<-createDataPartition(y = train_result,p = the_p,list =FALSE)
    plot_data <- train_data[plot_ind,]
    plot_result <- train_result[plot_ind]
    steplength <- 1/(x * epoch + y)
    for(step in 1:300){
      selected <- sample(1:length(train_data[,1]),1)
      selected_data <- train_data[selected,]
      training_Answer[1] <-	train_result[selected] == " >50K"
      training_Answer[2] <- train_result[selected] == " <=50K"
      predict<-selected_data * a
      #	print(predict)
      predict <- yi[training_Answer] * (sum(predict) + b)
      #	print(predict)
      predict <- predict >= 1
      
      #	print(predict)
      #	print(is_more_than_50)
      #	print(predict)
      #print(" ")
      
      
      if(predict){
        #true_cnt<-true_cnt+1
        delta_a <- lenta * a
        delta_b <- 0
      }
      else{
        delta_a <- lenta * a -  yi[training_Answer] * selected_data
        # print(yi[training_Answer]) 
        delta_b <- -yi[training_Answer]
      }
      a <- a - delta_a * steplength
      b <- b - delta_b * steplength
      if(step %% 30 == 0)
      {
        
        plot_predict<-t(t(plot_data)*a)
        
        plot_predict<-rowSums(plot_predict) + b
        plot_predict<-plot_predict > 0
        plot_Answer <- plot_result == " >50K"
        
        plot_accuracy <- plot_predict == plot_Answer
        plot_accuracy <- sum(plot_accuracy)/length(plot_accuracy)
        
        plot[try_lenta,(300 * (epoch - 1) + step)/30] <- plot_accuracy
      }
    }
  }
  validation_predict<-t(t(validation_data)*a)
  
  validation_predict<-rowSums(validation_predict) + b
  validation_predict<-validation_predict > 0
  validation_Answer <- validation_result == " >50K"
  
  val_accuracy <- validation_predict == validation_Answer
  val_accuracy <- sum(val_accuracy)/length(val_accuracy)
  if(val_accuracy > pre_accuracy)
  {
    pre_accuracy <- val_accuracy
    result_a <- a
    result_b <- b
  }
  
}
plot(1:length(plot[1,]),plot[1,],type = "l",	ylab="accuracy",ylim=c(0,1))
lines(1:length(plot[2,]),plot[2,],type = "l",col="red")
lines(1:length(plot[3,]),plot[3,],type = "l",col="green")
lines(1:length(plot[4,]),plot[4,],type = "l",col="gray")
#test_data
testdat<-read.csv('test.txt',header = FALSE)
testdat_con <- testdat[index]
test_data <- testdat_con[,-c(7)]
test_result <- testdat_con[,7]
test_data<-scale(test_data)

test_predict<-t(t(test_data)*result_a)
test_predict<-rowSums(test_predict) + result_b
test_predict<-test_predict > 0
test_Answer <- test_result == " >50K."
accuracy <- test_predict == test_Answer
accuracy <- sum(accuracy)/length(accuracy)
accuracy





