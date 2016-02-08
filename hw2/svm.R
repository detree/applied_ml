#several parameters
rm(list=ls())
mydir<-'/Users/SC/Documents/cs498_ml/hw2'
epoch_max<-as.numeric(50)
step_max<-as.numeric(300)
step_a<-as.numeric(0.05)
step_b<-as.numeric(100)
lamda<-as.numeric( 10^(-3) )

#########################################
setwd(mydir)
filedata1<-read.csv('adult.data', header = FALSE)
filedata2<-read.csv('adult.test', header = FALSE)
filedata<-rbind(filedata1, filedata2)
library(caret)
library(klaR)

#start the processing of raw data
cont_attr<- filedata[ , c(1, 3, 5, 11, 12, 13)]

cont_attr_mean<-sapply(cont_attr, mean)
cont_attr_std<-sapply(cont_attr,sd)
cont_attr_offsets<-t(t(cont_attr)-cont_attr_mean)
cont_rawx<-t(t(cont_attr_offsets)/cont_attr_std)

cont_rawy_idx<-grep("<=50K", filedata[,15])
cont_rawy<-filedata[,15] #array(dim=nrow(filedata))
cont_rawy<-as.integer(cont_rawy)
cont_rawy[cont_rawy_idx]<-1
cont_rawy[-cont_rawy_idx]<--1

part_test<-createDataPartition(cont_rawy, p=.1, list=FALSE)
part_valid<-createDataPartition(cont_rawy[-part_test], p=.1111, list=FALSE)
trainx<-cont_rawx[-c(part_test,part_valid), ]
trainy<-cont_rawy[-c(part_test,part_valid)]
testx<-cont_rawx[part_test, ]
testy<-cont_rawy[part_test]
validx<-cont_rawx[part_valid, ]
validy<-cont_rawy[part_valid]

#the evaluation.
main_a<-runif(6, -2, 2)
main_b<-runif(1, -2, 2)
for(epochi in 1:epoch_max)
{
  step_len <- 1/(step_a * epochi + step_b)
  for(stepi in 1:step_max)
  {
    rnd_idx<-sample(1:nrow(trainx), 1)
    #max(0, y_i(a*x_i+b)). if <0: get example wrong; ==0: get right
    mid_aMULx_i<- main_a * trainx[rnd_idx, ] #procedure in the middle: a*x_i
    judge<-max( 0, 1 - trainy[rnd_idx] * ( sum(mid_aMULx_i) + main_b) )
    if(judge==0)
    {
      grad_a<-lamda*main_a
      grad_b<-0
    }
    else if (judge>0)
    {
      grad_a<- -trainy[rnd_idx]*trainx[rnd_idx, ] + lamda* main_a
      grad_b<- -trainy[rnd_idx]
    }
    #update value for a and b
    main_a<-main_a - grad_a * step_len
    main_b<-main_b - grad_b * step_len
  }
}

#test
#test_judge<-main_a * testx
#test_judge<-1 - testy * (rowSums(test_judge) + main_b)
#test_judge<-pmax(0, test_judge)
#sum(test_judge<=0)/length(test_judge)
test_judge<-main_a * testx
test_judge<-rowSums(test_judge) + main_b
crct_cnt<- (test_judge>0)==(testy==1)
sum(crct_cnt)/length(crct_cnt)
