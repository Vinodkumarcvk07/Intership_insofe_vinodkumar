# Internship program at INSOFE
# Project: Congressional Voting Records Data Set
# Team(size 1): C Vinod Kumar
# ------------------------------------------------------------------------------------------------
# Abstract: US Congressmen voting records dataset having 1 dependent,16 independent variables
# Objective: Using given voting patterns classify the house member's party
# -------------------------------------------------------------------------------------------------
#removing the previous existed global environment variables 
rm(list=ls(all=TRUE))

#Setting working directory 
setwd("D:/DATA SCIENCE/Internship/Week3")
getwd()

#installing required libraries
library(mlr)  # it has all required machine learning libraries 
library(DMwR) # for 
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation
library(plyr) #split- apply- combine
library(e1071) #load e1071 library and invoke naiveBayes method
library(sqldf) #to use sql commands
library(MLmetrics) #Machine learning metrics
library(Hmisc)#use the Hmisc package, you can take advantage of some labeling features.
library(caret)

#Reading given txt file into table format 
actual_data <- read.table("house-votes-84.data.txt",sep=",")

#View(actual_data)

#////////////////////////////////
#///  Data Charatcerization   ///
#////////////////////////////////

#checking dimensions of data
dim(actual_data)  #435  17

#checking the class/type of data
data.class(actual_data) 
is.data.frame(actual_data)

#checking the summary of data
summary(actual_data)

#checking the structure of data
str(actual_data)

#head # gives first 6 rows of data
head(actual_data)

#tail # gives last 6 rows of data 
tail(actual_data)

# It shows freqency & proprotions of each variable
describe(actual_data)
describe(actual_data$V2)

#checking for missing values it is showing 0 null values
sum(is.na(actual_data)) 

#replacing "?" with "NA" 
actual_data[actual_data=="?"]<- NA  
head(actual_data) #now ? is raplaced with NA

#now checking of missing values
sum(is.na(actual_data))  #392
#no of missing vaules in each variable
colSums(is.na(actual_data))
#Total percentage of missing values in dataset
(sum(is.na(actual_data))/(nrow(actual_data)*ncol(actual_data)))*100  #I found 5.300879% of data missing
 
#Total percentage of missing values in each variable of given dataset
(colSums(is.na(actual_data))/nrow(actual_data))*100

#checking the column names
names(actual_data)

#replace column names with given names for better undestanding 
lables = c("classname","handicapped_infants","water_project_cost_sharing",
           "adoption_of_the_budget_resolution","physician_fee_freeze",
           "el_salvador_aid","religious_groups_in_schools",
           "anti_satellite_test_ban","aid_to_nicaraguan_contras",
           "mx_missile","immigration","synfuels_corporation_cutback",
           "education_spending","superfund_right-to_sue","crime","duty_free_exports",
           "export_administration_act_south_africa")

#replacing all column names with new lables 
names(actual_data)[1:17]<-lables

#checking each column labled correctly or not
names(actual_data)

#Type of class attributeaa
sapply(actual_data[,1:17], function(x) {class(x)})  # all variables are in "factor" type
#here showing 3 levels inculing "na" so we dropped "na" 
str(actual_data)
droplevels(actual_data)->actual_data


#////////////////////////////
#/////Data Visualization////      
#///////////////////////////

summary(actual_data$classname) 
# democrat republican 
# 267        168
prop.table(table(actual_data$classname))               
# democrat republican 
# 0.6137931  0.3862069 
qplot(x = classname,ylab ='number of memberes voted', data = actual_data, geom = "bar",title="class lable")
qplot(x = crime,ylab ='number of memberes voted',color=classname, data = actual_data, geom = "bar")

#using ggplot we can print Value Freqency and Proportions of each attribute/variable 
ggplot(actual_data, aes(x=classname),colors()) + theme_bw() + geom_bar() + labs(y="Number of Congressmen",x="No of Parties", title= "Members Voted by each party")


par(mfrow=c(4,5))
for(i in 1:17){
  plot(actual_data[,i],xlab = names(actual_data[i]),ylab="no of memers")
  dev.copy(jpeg,filename="final.jpg" )
  dev.off()
  #ggplot(actual_data, aes(x=actual_data[,i]),colours()) + theme_classic() +geom_bar() + labs(y=y="Number of Congressmen",x="No of Parties", title= "Members Voted by each party")
}

# View(data)
plot(as.factor(actual_data[,2]))
par(mfrow=c(3,6))
for(i in 2:17){
  plot(actual_data[,i],xlab = names(actual_data[i]),ylab="no of members", title= "voting table")
  dev.copy(jpeg,filename="final4.jpg" )
  dev.off()
}

write.csv(actual_data,"housevotes.csv")

#------------------------------------------------------------------------------------
setwd("D:/DATA SCIENCE/Internship/Week3")
congress_data<-read.csv("housevotes.csv",sep=',')

str(congress_data)
#ploting using plot
par(mfrow=c(3,6))
for(i in 2:17){
  plot(congress_data[,i],col=congress_data$classname,xlab = names(congress_data[i]),ylab = "number of members voted")
  dev.copy(jpeg,filename="final.jpg" )
  dev.off()
}
 
# #ploting using ggplot
# par(mfrow=c(3,3))
# for(i in 1:17){
# #ggplot(congress_data,aes(x=congress_data[,i]),colour = 'classname')+theme_classic() +geom_bar() +labs(y="number of members voted", title="voting by each member")
# ggplot(congress_data, aes(x= congress_data[,i]),colors()) + theme_bw() + geom_bar() + labs(y="Number of Congressmen",x= names(congress_data[i]), title= "Members Voted by each party")
# dev.copy(jpeg,filename="final3_ggplot.jpg")
# dev.off()
# }

#### Omiting missing values###

omited_mis_congress_data<-na.omit(congress_data)
str(omited_mis_congress_data)
dim(omited_mis_congress_data)


republic <- congress_data[which(congress_data$classname == 'republican'),]
democrat <- congress_data[which(congress_data$classname == 'democrat'),]

#### Central Imputation #######

sum(is.na(republic))  #131
sum(is.na(democrat))  #261

republic <- centralImputation(republic)
democrat <- centralImputation(democrat)
summary(republic)
summary(democrat)
  
imputed_congress_data <- rbind(republic,democrat)
dim(imputed_congress_data)
write.csv(imputed_congress_data,"imputed_congress_data.csv")

#divide into data in to training,test 80,20 proportions
set.seed(5000)
proportion_data <- sample(seq(1,2),size = nrow(imputed_congress_data),replace = TRUE, prob = c(.8, .2))
train_congress_data <- imputed_congress_data[proportion_data == 1,]
test_congress_data <- imputed_congress_data[proportion_data == 2,]
#
dim(train_congress_data)
dim(test_congress_data)

##///////////////######
###Model Building######
#/////////////////////#

library(e1071)
nb_model_congress_data<- naiveBayes(classname~.,data = train_congress_data)
nb_model_congress_data

pred_train_congress_data<-predict(nb_model_congress_data,train_congress_data[,-1])

pred_test_congress_data<-predict(nb_model_congress_data,test_congress_data[,-1])

summary(pred_test_congress_data)

summary(pred_train_congress_data)

conf_test_congress_data<-confusionMatrix(pred_test_congress_data,test_congress_data$classname)
conf_test_congress_data

library(MLmetrics)

Accuracy(train_congress_data$classname,pred_train_congress_data)
Recall(train_congress_data$classname,pred_train_congress_data)

Accuracy(test_congress_data$classname,pred_test_congress_data)
Recall(test_congress_data$classname,pred_test_congress_data)

##/////////////////###
## logistic  model####
##///////////////#####

#install.packages("ROCR")
library(ROCR)
logistic_model<-glm(classname~.,data = train_congress_data,'binomial')
logistic_model
summary(logistic_model)

pred_train_log <- predict(logistic_model,type = "response")
pred <- prediction(pred_train_log,train_congress_data$classname)

pred_log_congress_data<- predict(logistic_model,test_congress_data[,-1],type="response")

pref<-performance(pred, measure ="tpr", x.measure = "fpr")
pref

###plot Roc curve
plot(pref,col =rainbow(10),colorize = T,print.cutoffs.at=seq(0,1),title="Roc curve")

perf_auc <- performance(pred,measure = "auc")

perf_auc@y.values[[1]]

#prediction of test data

pred_log_test <- predict(logistic_model,test_congress_data,type = "response")

preds_log_test <- ifelse(pred_log_test>0.9,"republican","democrat")
library(caret)

conf_log_test <- confusionMatrix(preds_log_test,test_congress_data$classname)
conf_log_test


#/////////////////#
###Random forest###
#/////////////////#

install.packages("gplots")
library("gplots")
install.packages("randomForest")
library("randomForest")

Modelforest <- randomForest(classname~.,data = train_congress_data)
Modelforest

summary(Modelforest)
prob_train <- predict(Modelforest, train_congress_data,type="class")
prob_test <- predict(Modelforest, test_congress_data, type = "class")

prob_train

train_data_labs <- train_congress_data$classname 
 
test_data_labs <- test_congress_data$classname

conf_matrix <- table(train_data_labs,prob_train)
conf_matrix
conf_matrix_test<- table(test_data_labs,prob_test)
conf_matrix_test
specificity <- conf_matrix[1, 1]/sum(conf_matrix[1, ])
specificity
sensitivity <- conf_matrix[2, 2]/sum(conf_matrix[2, ])
sensitivity
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
accuracy 

resultrain <-c(accuracy,specificity,sensitivity)
resultrain 
conf_matrix_test <- table(test_data_labs, prob_test)
specificity_test <- conf_matrix_test[1, 1]/sum(conf_matrix_test[1, ])
sensitivity_test <- conf_matrix_test[2, 2]/sum(conf_matrix_test[2, ])
accuracy_test <- sum(diag(conf_matrix_test))/sum(conf_matrix_test)
resultest <- c(accuracy_test,specificity_test,sensitivity_test)
resultest
resultRF <- data.frame(rbind(resultrain,resultest))
resultRF


#//////////////////////#
# ///decisiontrees/////# 
#//////////////////////#

#install.packages("caret")
library(caret)
#install.packages(c('rpart','rpart.plot'))
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)

model_rpart <-rpart(classname~.,data=train_congress_data)
summary(model_rpart)

rpart.plot(model_rpart)
pred_rpart<-predict(model_rpart,test_congress_data[,-1],'class')

conf_rpart<-table(pred_rpart,test_congress_data$classname)
conf_rpart
summary(conf_rpart)


#---------------------------------------------------------------
