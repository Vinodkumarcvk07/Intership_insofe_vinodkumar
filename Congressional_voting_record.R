# Internship program at INSOFE
# Project: Congressional Voting Records Data Set
# Team(size 1): C Vinod Kumar
# ------------------------------------------------------------------------------------------------
# Abstract: "Congresssional voting records data"  
# Objective: Using given voting patterns classify the house member's party
# -------------------------------------------------------------------------------------------------

#removing the previous existed global environment variables 
rm(list=ls(all=T))

#installing required libraries
library(mlr) #This library all required machine learning libraries
library(e1071) #Invoke naiveBayes method,SVM and other models
library(MLmetrics) #It will help to calulate all machine learning metrics
library(DMwR) #This package includes functions "DataMiningwithR" Central Imputation 
library(ggplot2) # Create Elegant Data Visulalisations Using the Grammar of Graphics
library(rpart)  # "Recursive Partitioning and Regression Trees"
library(caret)  # "Classification and Regression Training" library that contains xgboost models 
library(randomForest) #RandomForest implements Breiman's random forest algorithm for classification and regression
library(rpart.plot) #Plot an rpart model, automatically tailoring the plot for the model's response type. 
library(glmnet) #These functions provide the base mechanisms for defining new functions in the R language
library(ROCR) #To plot ROC CURVE 
library(rminer) #To caluculate metrics
library(neuralnet) #neuralnet is used to train neural networks using backpropagation, resilient backpropagation
library(dummies) #contains functions,methods to creat,manipulate dummy variables
library(dummy) #Automatic Dummy Variable Creation with Support for Predictive Contexts
library(dplyr) #dplyr provides a flexible grammar of data manipulation.
library(Hmisc)#use the Hmisc package, you can take advantage of some labeling features.
library(lattice) #The lattice package is based on the Grid graphics engine, provides its own interface for querying
library(grid) #grid adds an nx by ny rectangular grid to an existing plot
library(MASS)
#install.packages("GoodmanKruskal")
library(GoodmanKruskal) #to check the corelation between Categorical variables
#install.packages("ggthemes")
library(ggthemes)
library(gridExtra)
library(xgboost)
#install.packages("C50")
library(C50)
#install.packages("Amelia")
#library(Amelia)
#setting working directory
setwd("G:/DATA SCIENCE/Internship/Final")

#Reading given txt file into table format 
actual_data<-read.table("house-votes-84.data.txt",sep=',')

View(actual_data)

#////////////////////////////////
#///  Data Charatcerization   ///
#////////////////////////////////


#checking dimentions of data
dim(actual_data)
#checking data set colmn and row names 
dimnames(actual_data)
#checking the structre of actual data
str(actual_data)
#summary of each column
summarizeColumns(actual_data)
#To know the freqency of each level
summarizeLevels(actual_data)
#Checking for missing values
sum(is.na(actual_data))
#Checking the type of class 
class(actual_data)
is.data.frame(actual_data)
#Type of class attribute
sapply(actual_data[,1:17], function(x) {class(x)})  # all variables are in "factor" type
#to view first 6 rows in actual data
head(actual_data)
#To View last 6 rows in actual data
tail(actual_data)
#Plot class lable to know freqency of memebers in each party
summary(actual_data$V1)
plot(actual_data$V1,xlab="Number of parties",ylab="Number of members",main="Total Number of members of each party")
#Given data set having "?" which represents Not voted,we consider them as missing values
#For that replacing "?" with "NA" 
actual_data_na<-actual_data
actual_data_na[actual_data_na=="?"]<- NA  
head(actual_data_na) #now ? is replaced with NA
#To Know structure of data after replacing with NA
str(actual_data_na)
#In structre we noticed that "?" replaced with NA but Levels did't changes still it is showing 3 levels 
#so we droping false levels
droplevels(actual_data_na)->actual_data_na
#Now checking for missing values
sum(is.na(actual_data_na))  #392
#Number of missing values in each column
number_miss_val<-colSums(is.na(actual_data_na))
number_miss_val

#ploting number of missing values in each bill
plot(number_miss_val,type="h",main="Number of missing values in each bill",xlab="Number of Bills")
#Checking percentage of Total missing values
sum(is.na(actual_data_na))/(nrow(actual_data_na)*ncol(actual_data_na))*100  #I found 5.300879% of data missing
#Calculating percentage of missing values in each bill
per_miss_val<-(colSums(is.na(actual_data_na))/nrow(actual_data_na))*100
per_miss_val
#ploting percentage of missing values in each bill
plot(per_miss_val,type="h",main="Percentage of missing values",xlab="Number of bills",ylab="Percentage of missing values")
plot(per_miss_val,type="l",main="Percentage of missing values",xlab="Number of bills",ylab="Percentage of missing values")
#Combing missing values along with its percentage  
miss_val_report<-rbind(number_miss_val,per_miss_val)

write.csv(miss_val_report,"missingvalues_report.csv")

#Given data set doesn't have any labels for better understanding giving lables
lables = c("classname","handicapped_infants","water_project_cost_sharing",
           "adoption_of_the_budget_resolution","physician_fee_freeze",
           "el_salvador_aid","religious_groups_in_schools",
           "anti_satellite_test_ban","aid_to_nicaraguan_contras",
           "mx_missile","immigration","synfuels_corporation_cutback",
           "education_spending","superfund_right_to_sue","crime","duty_free_exports",
           "export_administration_act_south_africa")

#lables to actual data
actual_data_na_lb<-actual_data_na
names(actual_data_na_lb)[1:17]<-lables
names(actual_data_na_lb)
dim(actual_data_na_lb)


#////////////////////////////
#///// Data Visualization////      
#///////////////////////////

library(ggplot2)
#Plot class lable to know freqency of memebers in each party
summary(actual_data$V1)
names(actual_data)
plot(actual_data$V1,xlab="Number of parties",ylab="Number of members",main="Total Number of members of each party")
ggplot(data = actual_data,mapping=aes(x=(V1),fill=V1),colors())+geom_bar()

g <- ggplot(actual_data, aes(V1))
g + geom_bar()

sum_handicapped<-summary(actual_data_na_lb$handicapped_infants)

qplot(x = actual_data_na_lb$handicapped_infants, col=classname,fill = classname,xlab="n-Voted No,y-Voted YES,NA-NOT Voted",ylab ='Number of members voted', data = actual_data_na_lb, geom = "auto",main ="Plot of Bill handicapped_infants")
qplot(x = actual_data_na_lb$physician_fee_freeze, col=classname,fill = classname,xlab="n-Voted No,y-Voted YES,NA-NOT Voted",ylab ='Number of members voted', data = actual_data_na_lb, geom = "auto",main ="Plot of Bill physician_fee_freeze")

# par(mfrow=c(3,6))
# for(i in 1:17){
#   plot(actual_data_na_lb[,i],xlab="n-Voted No,y-Voted YES,NA-NOT Voted",ylab="Number of members",main=names(actual_data_na_lb[i]))
#   dev.copy(jpeg,filename="Plot_bills1.jpg" )
#   dev.off()
# }  

#/////////////////////////////
#/// Data Preprocessing //////
#/////////////////////////////

#To know the corelation between the each varible

#Using Chisq test
actual_data_colnames<- c(colnames(actual_data_na_lb[,-1]))
for (i in actual_data_colnames){
  chi_sq_result<-chisq.test(actual_data_na_lb$classname,actual_data_na_lb[,i])
  print(i)  
  print(chi_sq_result)
} 

#Using GKtau function on which the GoodmanKruskal package is built
actual_data_na_lb_GK1<-GKtau(actual_data_na_lb$classname,actual_data_na_lb$classname) 
plot(actual_data_na_lb_GK1)

actual_data_na_lb_GK2<-GKtauDataframe(actual_data_na_lb) 
plot(actual_data_na_lb_GK2)

actual_data_GK3<-GKtauDataframe(actual_data) 
plot(actual_data_GK3)

actual_data_na_GK4<-GKtauDataframe(actual_data_na) 
plot(actual_data_na_GK4)

########## IMPUTING MISSING VALUES #########

#spliting data into republic and democratic
republic <- actual_data_na_lb[which(actual_data_na_lb$classname == 'republican'),]
democrat <- actual_data_na_lb[which(actual_data_na_lb$classname == 'democrat'),]

sum(is.na(republic))  #131
sum(is.na(democrat))  #261

dim(republic)
dim(democrat)

#### Central Imputation #######
republic <- centralImputation(republic)
democrat <- centralImputation(democrat)

summary(republic)
summary(democrat)

imp_data <- rbind(republic,democrat)
dim(imp_data)
str(imp_data)

#Ploting after missing values treatment 
#using ggplot we can print Value Freqency and Proportions of each attribute/variable 
p1_classname = ggplot(data = imp_data,mapping=aes(x=(classname),fill=classname))+geom_bar()
p2_handicapped_infants = ggplot(data = imp_data,mapping = aes(x=(handicapped_infants),fill = classname))+geom_bar()
p3_water_project_cost_sharing = ggplot(data = imp_data,mapping = aes(x=(water_project_cost_sharing),fill = classname))+geom_bar()
P4_physician_fee_freeze = ggplot(data = imp_data,mapping = aes(x=(physician_fee_freeze),fill = classname))+geom_bar()


p1_classname
p2_handicapped_infants
p3_water_project_cost_sharing
P4_physician_fee_freeze

library(ggplot2)
library(gridExtra)

# par(mfrow=c(3,6))
# for(i in 1:17){
#    qplot(x = imp_data[,i], col=classname,fill = classname,xlab="n-Voted No,y-Voted YES",ylab ="Number of members voted", data =imp_data, geom = "auto",main ="Plot of Bills")
#    dev.copy(jpeg,filename=paste(names(imp_data[i]),"plot.jpeg",sep ="_"))
#    dev.off()
# }
# 
# 
# par(mfrow=c(3,6))
# for(i in 1:17){
#   ggplot(data = imp_data,mapping = aes(x=lables[i],fill = classname))+geom_bar()
#   dev.copy(jpeg,filename="plot.jpeg")
#   dev.off()
# }

write.csv(imp_data,"housevotes_imputed.csv")

#/////////////////////#
#// Model Building  //#
#/////////////////////#
set.seed(1234)

#divide into data in to training,test 80,20 proportions
proportion_data <- sample(seq(1,2),size = nrow(imp_data),replace = TRUE, prob = c(.8, .2))
train_imp_data <- imp_data[proportion_data == 1,]
test_imp_data <- imp_data[proportion_data == 2,]


###############  Naivebayes ###################

#Naviebayes model building using all variables
naivebayes_model1<-naiveBayes(classname~.,train_imp_data)

#Naviebayes model building using one variable
naivebayes_model2<-naiveBayes(classname~ physician_fee_freeze,train_imp_data)

#Naviebayes model building using three important variables
naivebayes_model5<-naiveBayes(classname~ physician_fee_freeze+education_spending+adoption_of_the_budget_resolution,train_imp_data)

#Naivebayes model building with tuning parameteres laplace=0
naivebayes_model3<-naiveBayes(classname~.,train_imp_data,laplace=0)

#Naivebayes model building with tuning parameteres laplace=1
naivebayes_model4<-naiveBayes(classname~.,train_imp_data,laplace=1)

#all naivebayes models 
naivebayes_model1
naivebayes_model2
naivebayes_model3
naivebayes_model4
naivebayes_model5

#predicting naivebayes_model1
pd_train_naivebayes1<-predict(naivebayes_model1,train_imp_data)
pd_test_naivebayes1<-predict(naivebayes_model1,test_imp_data)

pd_train_naivebayes2<-predict(naivebayes_model2,train_imp_data)
pd_test_naivebayes2<-predict(naivebayes_model2,test_imp_data)

pd_train_naivebayes3<-predict(naivebayes_model3,train_imp_data)
pd_test_naivebayes3<-predict(naivebayes_model3,test_imp_data)

pd_train_naivebayes4<-predict(naivebayes_model4,train_imp_data)
pd_test_naivebayes4<-predict(naivebayes_model4,test_imp_data)

pd_train_naivebayes5<-predict(naivebayes_model5,train_imp_data)
pd_test_naivebayes5<-predict(naivebayes_model5,test_imp_data)


pd_train_naivebayes1
pd_test_naivebayes1

pd_train_naivebayes2
pd_test_naivebayes2

pd_train_naivebayes3
pd_test_naivebayes3

pd_train_naivebayes4
pd_test_naivebayes4

pd_train_naivebayes5
pd_test_naivebayes5

table(pd_train_naivebayes1,train_imp_data$classname)
table(pd_test_naivebayes1,test_imp_data$classname)

table(pd_train_naivebayes2,train_imp_data$classname)
table(pd_test_naivebayes2,test_imp_data$classname)

table(pd_train_naivebayes3,train_imp_data$classname)
table(pd_test_naivebayes3,test_imp_data$classname)

table(pd_train_naivebayes4,train_imp_data$classname)
table(pd_test_naivebayes4,test_imp_data$classname)

table(pd_train_naivebayes5,train_imp_data$classname)
table(pd_test_naivebayes5,test_imp_data$classname)

confusionMatrix(pd_train_naivebayes1,train_imp_data$classname)
confusionMatrix(pd_test_naivebayes1,test_imp_data$classname)

confusionMatrix(pd_train_naivebayes2,train_imp_data$classname)
confusionMatrix(pd_test_naivebayes2,test_imp_data$classname)

confusionMatrix(pd_train_naivebayes3,train_imp_data$classname)
confusionMatrix(pd_test_naivebayes3,test_imp_data$classname)

confusionMatrix(pd_train_naivebayes4,train_imp_data$classname)
confusionMatrix(pd_test_naivebayes4,test_imp_data$classname)

confusionMatrix(pd_train_naivebayes5,train_imp_data$classname)
confusionMatrix(pd_test_naivebayes5,test_imp_data$classname)


mmetric(train_imp_data$classname,pd_train_naivebayes1,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_naivebayes1,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pd_train_naivebayes2,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_naivebayes2,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pd_train_naivebayes3,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_naivebayes3,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pd_train_naivebayes4,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_naivebayes4,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pd_train_naivebayes5,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_naivebayes5,c("ACC","PRECISION","TPR","F1"))


Accuracy_nb_train1<-Accuracy(pd_train_naivebayes1,train_imp_data$classname)
Recall_nb_train1<-Recall(pd_train_naivebayes1,train_imp_data$classname)
Precision_nb_train1<-Precision(pd_train_naivebayes1,train_imp_data$classname)
F1_score_nb_train1<-F1_Score(pd_train_naivebayes1,train_imp_data$classname)
Train_results_nb1<-c("Accuracy"=Accuracy_nb_train1,"Recall"=Recall_nb_train1,"Precision"=Precision_nb_train1,"F1_score"=F1_score_nb_train1)

Accuracy_nb_test1<-Accuracy(pd_test_naivebayes1,test_imp_data$classname)
Recall_nb_test1<-Recall(pd_test_naivebayes1,test_imp_data$classname)
Precision_nb_test1<-Precision(pd_test_naivebayes1,test_imp_data$classname)
F1_nb_test1<-F1_Score(pd_test_naivebayes1,test_imp_data$classname)
Test_results_nb1<-c("Accuracy"=Accuracy_nb_test1,"Recall"=Recall_nb_test1,"Precision"=Precision_nb_test1,"F1_score"=F1_nb_test1)

Accuracy_nb_train2<-Accuracy(pd_train_naivebayes2,train_imp_data$classname)
Recall_nb_train2<-Recall(pd_train_naivebayes2,train_imp_data$classname)
Precision_nb_train2<-Precision(pd_train_naivebayes2,train_imp_data$classname)
F1_score_nb_train2<-F1_Score(pd_train_naivebayes2,train_imp_data$classname)
Train_results_nb2<-c("Accuracy"=Accuracy_nb_train2,"Recall"=Recall_nb_train2,"Precision"=Precision_nb_train2,"F1_score"=F1_score_nb_train2)

Accuracy_nb_test2<-Accuracy(pd_test_naivebayes2,test_imp_data$classname)
Recall_nb_test2<-Recall(pd_test_naivebayes2,test_imp_data$classname)
Precision_nb_test2<-Precision(pd_test_naivebayes2,test_imp_data$classname)
F1_nb_test2<-F1_Score(pd_test_naivebayes2,test_imp_data$classname)
Test_results_nb2<-c("Accuracy"=Accuracy_nb_test2,"Recall"=Recall_nb_test2,"Precision"=Precision_nb_test2,"F1_score"=F1_nb_test2)

Accuracy_nb_train3<-Accuracy(pd_train_naivebayes3,train_imp_data$classname)
Recall_nb_train3<-Recall(pd_train_naivebayes3,train_imp_data$classname)
Precision_nb_train3<-Precision(pd_train_naivebayes3,train_imp_data$classname)
F1_score_nb_train3<-F1_Score(pd_train_naivebayes3,train_imp_data$classname)
Train_results_nb3<-c("Accuracy"=Accuracy_nb_train3,"Recall"=Recall_nb_train3,"Precision"=Precision_nb_train3,"F1_score"=F1_score_nb_train3)

Accuracy_nb_test3<-Accuracy(pd_test_naivebayes3,test_imp_data$classname)
Recall_nb_test3<-Recall(pd_test_naivebayes3,test_imp_data$classname)
Precision_nb_test3<-Precision(pd_test_naivebayes3,test_imp_data$classname)
F1_nb_test3<-F1_Score(pd_test_naivebayes3,test_imp_data$classname)
Test_results_nb3<-c("Accuracy"=Accuracy_nb_test3,"Recall"=Recall_nb_test3,"Precision"=Precision_nb_test3,"F1_score"=F1_nb_test3)

Accuracy_nb_train4<-Accuracy(pd_train_naivebayes4,train_imp_data$classname)
Recall_nb_train4<-Recall(pd_train_naivebayes4,train_imp_data$classname)
Precision_nb_train4<-Precision(pd_train_naivebayes4,train_imp_data$classname)
F1_score_nb_train4<-F1_Score(pd_train_naivebayes4,train_imp_data$classname)
Train_results_nb4<-c("Accuracy"=Accuracy_nb_train4,"Recall"=Recall_nb_train4,"Precision"=Precision_nb_train4,"F1_score"=F1_score_nb_train4)

Accuracy_nb_test4<-Accuracy(pd_test_naivebayes4,test_imp_data$classname)
Recall_nb_test4<-Recall(pd_test_naivebayes4,test_imp_data$classname)
Precision_nb_test4<-Precision(pd_test_naivebayes4,test_imp_data$classname)
F1_nb_test4<-F1_Score(pd_test_naivebayes4,test_imp_data$classname)
Test_results_nb4<-c("Accuracy"=Accuracy_nb_test4,"Recall"=Recall_nb_test4,"Precision"=Precision_nb_test4,"F1_score"=F1_nb_test4)

Accuracy_nb_train5<-Accuracy(pd_train_naivebayes5,train_imp_data$classname)
Recall_nb_train5<-Recall(pd_train_naivebayes5,train_imp_data$classname)
Precision_nb_train5<-Precision(pd_train_naivebayes5,train_imp_data$classname)
F1_score_nb_train5<-F1_Score(pd_train_naivebayes5,train_imp_data$classname)
Train_results_nb5<-c("Accuracy"=Accuracy_nb_train5,"Recall"=Recall_nb_train5,"Precision"=Precision_nb_train5,"F1_score"=F1_score_nb_train5)

Accuracy_nb_test5<-Accuracy(pd_test_naivebayes5,test_imp_data$classname)
Recall_nb_test5<-Recall(pd_test_naivebayes5,test_imp_data$classname)
Precision_nb_test5<-Precision(pd_test_naivebayes5,test_imp_data$classname)
F1_nb_test5<-F1_Score(pd_test_naivebayes5,test_imp_data$classname)
Test_results_nb5<-c("Accuracy"=Accuracy_nb_test5,"Recall"=Recall_nb_test5,"Precision"=Precision_nb_test5,"F1_score"=F1_nb_test5)


Train_results_nb1
Test_results_nb1

Train_results_nb2
Test_results_nb2

Train_results_nb3
Test_results_nb3

Train_results_nb4
Test_results_nb4

Train_results_nb5
Test_results_nb5


########################################Logistic regression###########################################################3
library(ROCR)
library(glmnet)
#----------
library(glmnet)
logistic_model<-glm(classname~.,data = train_imp_data,family="binomial")
logistic_model
summary(logistic_model)
pred_train_log <- predict(logistic_model,type = "response")
pred <- prediction(pred_train_log,train_imp_data$classname)


pred_log_congress_data<- predict(logistic_model,test_congress_data[,-1],type="response")

#---------
logistic_model1<-glm(classname~.,data=train_imp_data,family="binomial")
# printing the summary of the model 
summary(logistic_model1)
#confidence interval
confint(logistic_model1)
#to know the significanc of the feartures
anova(logistic_model1, test="Chisq")

# logistic_model_summary<-capture.output(summary(logistic_model))
# cat(logistic_model_summary,file="logistic_model_summary.txt",sep="\n",append=TRUE)

logistic_model2<-glm(classname~ physician_fee_freeze+education_spending+adoption_of_the_budget_resolution,data=train_imp_data,family="binomial")

#cooks.distance(logistic_model1)

#Assessing the predictive ability of the model


#converting log of odd into probalilities
pred_log_train1 <- predict(logistic_model1,train_imp_data,type = "response")
pd_log_train1 <- ifelse(pred_log_train1>0.5,"republican","democrat")

pr <- prediction(pd_log_train1,train_imp_data[c("classname")])
perf <- performance(pr,measure = "tpr",x.measure = "fpr") 

#prediction of test data
pred_log_test1 <- predict(logistic_model1,test_imp_data,type = "response")
pd_log_test1 <- ifelse(pred_log_test1>0.5,"republican","democrat")

pred_log_train2 <- predict(logistic_model2,train_imp_data,type = "response")
pd_log_train2 <- ifelse(pred_log_train2>0.5,"republican","democrat")

pred_log_test2 <- predict(logistic_model2,test_imp_data,type = "response")
pd_log_test2 <- ifelse(pred_log_test2>0.5,"republican","democrat")


table(pd_log_train1,train_imp_data$classname)
table(pd_log_test1,test_imp_data$classname)

table(pd_log_train2,train_imp_data$classname)
table(pd_log_test2,test_imp_data$classname)


conf_log_train1 <- confusionMatrix(pd_log_train1,train_imp_data$classname)
conf_log_train1

conf_log_test1 <- confusionMatrix(pd_log_test1,test_imp_data$classname)
conf_log_test1

conf_log_train2 <- confusionMatrix(pd_log_train2,train_imp_data$classname)
conf_log_train2

conf_log_test2 <- confusionMatrix(pd_log_test2,test_imp_data$classname)
conf_log_test2


mmetric(train_imp_data$classname,pd_log_train1,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_log_test1,c("ACC","PRECISIOn","TPR","F1"))


##############################Support Vector Machine(SVM)######################################################33 
svmfit1<-svm(train_imp_data$classname~.,train_imp_data,kernal="linear",cost=10,scale = FALSE)
svmfit1

#

svmfit2<-svm(train_imp_data$classname~.,train_imp_data,kernal="radial",cost=10,scale = FALSE)
svmfit2
#
svmfit3<-svm(train_imp_data$classname~physician_fee_freeze,train_imp_data,kernal="linear",cost=10,scale = FALSE)
svmfit3
#
svmfit4<-svm(train_imp_data$classname~physician_fee_freeze+education_spending+adoption_of_the_budget_resolution,train_imp_data,kernal="linear",scale = FALSE)
svmfit4

#
pd_train_svmfit1<-predict(svmfit1,train_imp_data)
pd_train_svmfit1

pd_test_svmfit1<-predict(svmfit1,test_imp_data)
pd_test_svmfit1

confusionMatrix(pd_train_svmfit1,train_imp_data$classname)
confusionMatrix(pd_test_svmfit1,test_imp_data$classname)

#
pd_train_svmfit2<-predict(svmfit2,train_imp_data)
pd_train_svmfit2

pd_test_svmfit2<-predict(svmfit2,test_imp_data)
pd_test_svmfit2

confusionMatrix(pd_train_svmfit2,train_imp_data$classname)
confusionMatrix(pd_test_svmfit2,test_imp_data$classname)

#
pd_train_svmfit3<-predict(svmfit3,train_imp_data)
pd_train_svmfit3

pd_test_svmfit3<-predict(svmfit3,test_imp_data)
pd_test_svmfit3

confusionMatrix(pd_train_svmfit3,train_imp_data$classname)
confusionMatrix(pd_test_svmfit3,test_imp_data$classname)

#
pd_train_svmfit4<-predict(svmfit4,train_imp_data)
pd_train_svmfit4

pd_test_svmfit4<-predict(svmfit4,test_imp_data)
pd_test_svmfit4

confusionMatrix(pd_train_svmfit4,train_imp_data$classname)
confusionMatrix(pd_test_svmfit4,test_imp_data$classname)

#svmfit1
mmetric(train_imp_data$classname,pd_train_svmfit1,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_svmfit1,c("ACC","PRECISION","TPR","F1"))
#svmfit1

mmetric(train_imp_data$classname,pd_train_svmfit2,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_svmfit2,c("ACC","PRECISION","TPR","F1"))

#svmfit3
mmetric(train_imp_data$classname,pd_train_svmfit3,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_svmfit3,c("ACC","PRECISION","TPR","F1"))

#svmfit4
mmetric(train_imp_data$classname,pd_train_svmfit4,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_svmfit4,c("ACC","PRECISION","TPR","F1"))



#tuned_svm<- tune(classname~.,data=train_imp_data,kernel="linear",ranges= list(cost=0.001,0.01,0.1,1,10,100))

#------------------------------------------------------------
#rpart

rpart_model1<-rpart(classname~.,train_imp_data)
rpart_model1
printcp(rpart_model1)
plotcp(rpart_model1)
rpart.plot(rpart_model1)
pd_train_rpart1<-predict(rpart_model1,train_imp_data,"class")
pd_train_rpart1
pd_test_rpart1<-predict(rpart_model1,test_imp_data,"class")
pd_test_rpart1
confusionMatrix(pd_train_rpart1,train_imp_data$classname)
confusionMatrix(pd_test_rpart1,test_imp_data$classname)

mmetric(train_imp_data$classname,pd_train_rpart1,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pd_test_rpart1,c("ACC","PRECISION","TPR","F1"))


Accuracy_rpart<-Accuracy(pd_test_rpart1,test_imp_data$classname)
Recall_rpart<-Recall(pd_test_rpart1,test_imp_data$classname)
Precision_rpart<-Precision(pd_test_rpart1,test_imp_data$classname)
F1_score_rpart<-F1_Score(pd_test_rpart1,test_imp_data$classname)

evalution_metrics<-c("Accuracy_rpart"=Accuracy_rpart,"Recall_rpart"=Recall_rpart,"Precision_rpart"=Precision_rpart,"F1_score_rpart"=F1_score_rpart)
evalution_metrics

rpart_model2<-rpart(classname~physician_fee_freeze,train_imp_data)
printcp(rpart_model2)
plotcp(rpart_model2)
rpart.plot(rpart_model2)

pd_train_rpart2<-predict(rpart_model2,train_imp_data,"class")
pd_train_rpart2
pd_test_rpart2<-predict(rpart_model2,test_imp_data,"class")
pd_test_rpart2

confusionMatrix(pd_train_rpart2,train_imp_data$classname)
confusionMatrix(pd_test_rpart2,test_imp_data$classname)

Accuracy_rpart2<-Accuracy(pd_test_rpart2,test_imp_data$classname)
Recall_rpart2<-Recall(pd_test_rpart2,test_imp_data$classname)
Precision_rpart2<-Precision(pd_test_rpart2,test_imp_data$classname)
F1_score_rpart2<-F1_Score(pd_test_rpart2,test_imp_data$classname)

evalution_metrics<-c("Accuracy_rpart"=Accuracy_rpart2,"Recall_rpart"=Recall_rpart2,"Precision_rpart"=Precision_rpart2,"F1_score_rpart2"=F1_score_rpart)
evalution_metrics

#random forest
randomforest_model<-randomForest(classname~.,train_imp_data,ntree=50)
summary(randomforest_model)

var_imortance<-varImp(randomforest_model)

write.csv(var_imortance,"var_importance1.csv")

varImpPlot(randomforest_model)
#
randomforest_model2<-randomForest(classname~ adoption_of_the_budget_resolution+physician_fee_freeze+el_salvador_aid,train_imp_data)
randomforest_model2
#
randomforest_model3<-randomForest(classname~physician_fee_freeze,train_imp_data)
randomforest_model3
#
pred_train_random1<-predict(randomforest_model,train_imp_data)
pred_train_random2<-predict(randomforest_model2,train_imp_data)
pred_train_random3<-predict(randomforest_model3,train_imp_data)
#
pred_test_random1<-predict(randomforest_model,test_imp_data)
pred_test_random2<-predict(randomforest_model2,test_imp_data)
pred_test_random3<-predict(randomforest_model3,test_imp_data)
#
confusionMatrix(pred_train_random1,train_imp_data$classname)
confusionMatrix(pred_train_random2,train_imp_data$classname)
confusionMatrix(pred_train_random3,train_imp_data$classname)
#
confusionMatrix(pred_test_random1,test_imp_data$classname)
confusionMatrix(pred_test_random2,test_imp_data$classname)
confusionMatrix(pred_test_random3,test_imp_data$classname)


mmetric(train_imp_data$classname,pred_train_random1,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pred_test_random1,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pred_train_random2,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pred_test_random2,c("ACC","PRECISION","TPR","F1"))

mmetric(train_imp_data$classname,pred_train_random3,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,pred_test_random3,c("ACC","PRECISION","TPR","F1"))

# adoption_of_the_budget_resolution      25.0584554
# physician_fee_freeze                   59.2563413
# el_salvador_aid                        14.7614638

#------------------------------neuralnet-----------------------------------
library(neuralnet)
names(train_imp_data[1])
nn_train_data<-data.frame(ifelse(train_imp_data[,2:17]=='y',1,0))
nn2_train_data<-data.frame(ifelse(train_imp_data[1]=='democrat',1,0))
dim(nn_train_data)
dim(nn2_train_data)
train_nn_data<-cbind(nn2_train_data,nn_train_data)
dim(train_nn_data)
View(train_nn_data)
str(train_nn_data)
names(train_nn_data)
neuralnet_model<-neuralnet(classname~ handicapped_infants+ water_project_cost_sharing
                         +physician_fee_freeze,train_nn_data,hidden=5,err.fct='ce',linear.output=FALSE)
neuralnet_model
str(neuralnet_model)
plot(neuralnet_model)
allvariables<-colnames(train_nn_data[])
predvar<-allvariables[!allvariables%in%"classname"]
predvar<-paste(predvar,collapse="+")
form_train<-as.formula(paste("classname~",predvar,collapse ="+"))
form_train
neuralnet_train_model<-neuralnet(form_train,train_nn_data,hidden=10,err.fct='ce',linear.output=FALSE)
plot(neuralnet_train_model)

#KNN
model_knn <- knn3(classname~.,train_imp_data,k = 3)
preds_k <- predict(model_knn, train_imp_data)
preds_knn <- ifelse(preds_k[, 1] > preds_k[, 2],"democratic","republic")
table(preds_knn,train_imp_data$classname)

preds_test_k<-predict(model_knn, test_imp_data)
preds_test_knn<-ifelse(preds_test_k[, 1]>preds_test_k[, 2], "democratic","republic")

table(preds_test_knn,test_imp_data$classname)

mmetric(train_imp_data$classname,preds_knn,c("ACC","PRECISION","TPR","F1"))
mmetric(test_imp_data$classname,preds_test_knn,c("ACC","PRECISION","TPR","F1"))

summary(model_knn)
#

library(xgboost)
transformation <- preProcess(train_imp_data, method = c("range"))
trainingData <- predict(transformation, train_imp_data) 
testData <- predict(transformation, test_imp_data)

xgb.ctrl <- trainControl(method = "repeatedcv", repeats = 3, number = 3, 
                         search='random', allowParallel=T)

xgb.tune <-train(classname~., data = trainingData, method="xgbTree", 
                 trControl=xgb.ctrl, 
                 tuneLength=30, 
                 verbose=T, 
                 metric="Accuracy", 
                 nthread=3)
xgb.tune 
View(xgb.tune$results)

preds_train_xgboost <- predict(xgb.tune, trainingData) 

preds_test_xgboost <- predict(xgb.tune, testData) 

confusionMatrix(trainingData$classname, preds_train_xgboost)

confusionMatrix(testData$classname, preds_test_xgboost)

mmetric(trainingData$classname, preds_train_xgboost,c("ACC","PRECISION","TPR","F1"))
mmetric(testData$classname, preds_test_xgboost,c("ACC","PRECISION","TPR","F1"))

#
#Building a stacked ensemble
train_preds_df <- data.frame(svm = pd_train_svmfit1, naivebayes = pd_train_naivebayes1 ,rpart=pd_train_rpart1,
                             random = pred_train_random1, classname = train_imp_data$classname)
test_preds_df <- data.frame(svm = pd_test_svmfit1, naivebayes = pd_test_naivebayes1 ,rpart=pd_test_rpart1,
                            random = pred_test_random1, classname = test_imp_data$classname)

#combine those two dataframes together and convert target variable into factor
stack_df <- rbind(train_preds_df, test_preds_df)
stack_df$classname <- as.factor(stack_df$classname)

#View(stack_df)
str(stack_df)

#
stacking_nb_model<-naiveBayes(classname~.,train_preds_df)
stacking_nb_model

pred_stacking_train_nb<-predict(stacking_nb_model,train_preds_df)
pred_stacking_test_nb<-predict(stacking_nb_model,test_preds_df)

confusionMatrix(pred_stacking_train_nb,train_preds_df$classname)
confusionMatrix(pred_stacking_test_nb,test_preds_df$classname)



