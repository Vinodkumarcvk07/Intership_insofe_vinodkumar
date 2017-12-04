#Internship programe at INSOFE
#Week 3 working on building model on given data set
#Project: US Congressional Voting Records Data Set
#Team(size 1): C Vinod Kumar
#----------------------------------------------------------------------------------------------
setwd("D:/DATA SCIENCE/Internship")

#install.packages("mlr") # it has all required machine learning libraries 
library(mlr)  # it has all required machine learning libraries 
#install.packages("ggplot2")
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation
library(plyr) #split- apply- combine
library(e1071) #load e1071 library and invoke naiveBayes method

#Reading data set 
data<-read.table("house-votes-84.data.txt",sep=',')
View(data)
#labeling the column names
lables = c("classname","handicapped-infants","water-project-cost-sharing",
           "adoption-of-the-budget-resolution","physician-fee-freeze",
           "el-salvador-aid","religious-groups-in-schools",
           "anti-satellite-test-ban","aid-to-nicaraguan-contras",
           "mx-missile","immigration","synfuels-corporation-cutback",
           "education-spending","superfund-right-to-sue","crime","duty-free-exports",
           "export-administration-act-south-africa")

#assinging each variable with lables
names(data)[1:17]<-lables
data[data=="?"]<-NA

#missing values verifying
sum(is.na(data))            #392
write.csv(data,"housevotes.csv")
dim(data)
str(data)
summary(data)

#spliting data in to train and test and validation
# idx <- sample(seq(1, 3), size = nrow(data), replace = TRUE, prob = c(.6, .2, .2))
# train <- data[idx == 1,]
# test <- data[idx == 2,]
# val <- data[idx == 3,]

#head(train)
#View(train)
#View(test)
#View(val)
#head(val)
#install.packages("e1071")
# library(e1071)
# navbase_model1 <- naiveBayes(data$classname~.,data = train,na.pass(data))
# navebase_model
# navbase_model2 <- naiveBayes(data$classname~.,data = train,na.omit(data))
# navbase_model2

#
par(mfrow=c(1,1))
barplot(table(data$classname), col="green", xlab="parties", ylab ="no of members",main="Number of memberes in each class")
library(DMwR)
#spliting data with respecitive to no of parties
republic <-data$classname=="republican"
democrat <- data$classname=="democrat"

#ploting votes with respective each party and each bill
par(mfrow=c(4,2))
for(i in 2:ncol(data)){
                       plot(as.factor(data[republic,i]))
                       title(main="Republican votes cast for each bill",col="green", xlab="casted votes", ylab="Number of members")
                      }
par(mfrow=c(4,2))
for(i in 2:ncol(data)){
                       plot(as.factor(data[democrat,i]))
                       title(main="Democrat votes cast for issue",col="blue", xlab="casted vote", ylab="Number of members")
                      } 

# of missing values after replacing ? with NA
sum(is.na(data))             # 392 null values
colSums(is.na(data))
rowSums(is.na(data))

#% na's missing in each variable  
per_mis<-(colSums(is.na(data))/nrow(is.na(data)))*100
per_mis                                           
sort(per_mis)
write.csv(per_mis,"results.csv")
?write.csv

# We have a lot of NA. We are going to impute the values
# Functions needed for imputation
# function to return number of NAs by vote and class (democrat or republican)
na_by_col_class <- function (col,cls){return(sum(is.na(data[,col]) & data$classname==cls))}

#function to compute the conditional probability that a member of a party will cast a "yes" vote for a particular issue. 
#The probability is based on all members of the party who #actually cast a vote on the issue (ignores NAs).
p_y_col_class <- function(col,cls){
  sum_y<-sum(data[,col]=="y" & data$classname==cls,na.rm = TRUE)
  sum_n<-sum(data[,col]=="n" & data$classname==cls,na.rm = TRUE)
  return(sum_y/(sum_y+sum_n))
}


#Check the prob of yes vote by a democrat in issue 5
p_y_col_class(5,"democrat")

#impute missing values.

# If the republican congresman didn't vote, then we are allocating 'y' or 'n' based on if their
# party voted 'y' or 'n'
for (i in 2:ncol(data)) 
{
  if(sum(is.na(data[,i])>0)) 
  {
    c1 <- which(is.na(data[,i])& data$classname=="democrat",arr.ind = TRUE)
    c2 <- which(is.na(data[,i])& data$classname=="republican",arr.ind = TRUE)
    data[c1,i] <- ifelse(runif(na_by_col_class(i,"democrat"))<p_y_col_class(i,"democrat"),"y","n")
    data[c2,i] <- ifelse(runif(na_by_col_class(i,"republican"))<p_y_col_class(i,"republican"),"y","n")}
}

#divide into test and training sets
#create new col "train" and assign 1 or 0 in 80/20 proportion via random uniform dist
data[,"train"] <- ifelse(runif(nrow(data))<0.80,1,0)

#get col number of train / test indicator column (needed later)
trainColNum <- grep("train",names(data))
#separate training and test sets and remove training column before modeling
traindata <- data[data$train==1,-trainColNum]
testdata <- data[data$train==0,-trainColNum]

head(data)
idx <- sample(seq(1, 3), size = nrow(data), replace = TRUE, prob = c(.6, .2, .2))
# train <- data[idx == 1,]
# test <- data[idx == 2,]
# val <- data[idx == 3,]



#load e1071 library and invoke naiveBayes method
library(e1071)
nb_model <- naiveBayes(classname~.,data = traindata)

nb_model

#Lets test the model
nb_test_predict <- predict(nb_model,testdata[,-1])

#fraction of correct predictions
mean(nb_test_predict==testdata$classname)

#confusion matrix
confusion_data<-table(pred=nb_test_predict,true=testdata$classname)


confusion_data

par(mfrow=c(1,1))
plot(confusion_data,main="Voting actual and predicted", xlab="parties",ylab="parties")





