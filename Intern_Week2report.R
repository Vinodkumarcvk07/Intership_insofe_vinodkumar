rm(list=ls(all=TRUE))

#Intership programe at INSOFE
#Project: US Congressional Voting Records Data Set
#Team(size 1): C Vinod Kumar
#-----------------------------------------------------------------------------------------------------------------
#Abstract: 
#US Congressmen voting records datset for 16 bills of previous year 
#Objective: To bulid a model and predict ---which party bill will-----?
------------------------------------------------------------------------------------------
#setwd("D:/DATA SCIENCE/Internship")
#getwd()
#rm(list=ls(all=TRUE))

#install.packages("mlr") # it has all required machine learning libraries 
library(mlr)  # it has all required machine learning libraries 
#install.packages("ggplot2")
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation
library(plyr) #split- apply- combine
library(e1071) #load e1071 library and invoke naiveBayes method

#reading data in to table format 
data<-read.table("house-votes-84.data.txt",sep=",")
#write.csv(data,"uscongress_voting.csv")
head(data)

#Viewing the data to understand the data set 
View(data)

#////////////////////////////////
#     Data Charatcerization   ///
#////////////////////////////////

#checking the summary of data
summary(data)

#checking the structure of data
str(data)

#checking the class of data
data.class(data)

#head  # gives first 6 rows of data
head(data)

#tail # gives last 6 rows of data 
tail(data)

#names
colnames(data)

#replace column names with domain knowledge 
lables = c("classname","handicapped-infants","water-project-cost-sharing",
           "adoption-of-the-budget-resolution","physician-fee-freeze",
           "el-salvador-aid","religious-groups-in-schools",
           "anti-satellite-test-ban","aid-to-nicaraguan-contras",
           "mx-missile","immigration","synfuels-corporation-cutback",
           "education-spending","superfund-right-to-sue","crime","duty-free-exports",
           "export-administration-act-south-africa")


library(Hmisc)#use the Hmisc package, you can take advantage of some labeling features.

describe(data)
describe(data$V1)

#replacing all names with new lables 
names(data)[1:17]<-lables

#checking new labes of data set
names(data)
head(data)

describe(data)
describe(data$classname)

#checking for the data dimenstions 
data_dim<-dim(data)                 #435 rows #17 columns
ncol(data)                          #17
nrow(data)                          #435

#checking # of records
data_dim[1]       # 435 row/records

#checking # of attributes
data_dim[2]       # 17  cols/attributes
length(data)      # 17 varibles 

#Type of class attribute
sapply(data[,1:17], function(x) {class(x)})

# of missing values with out replacing ?
sum(is.na(data))              # 0 null values

# Recoding Values to Missing
# replacing "?" with "ng" # Recoding Values to Missing 
data[data=="?"]<-NA

# of missing values after replacing ? with NA
sum(is.na(data))             # 392 null values

colSums(is.na(data))
rowSums(is.na(data))
sum(is.na(data$crime))

sapply(data, function(x) sum(is.na(x)))
ndata<-data_dim[1]*data_dim[2]

#total %of missing vaules in data set
per_total<-((sum(is.na(data))/ndata)*100)
per_total                                            #5.300879

#% of each variable percentage of missing values 
per_mis<-(colSums(is.na(data))/nrow(is.na(data)))*100
per_mis                                           

#sorting data from highest null value variable
sort(colSums(is.na(data)),decreasing = TRUE)
sort(per_mis,decreasing =TRUE)

#of Levels in Taget Variable
str(data$classname)      #Factor w/ 2 levels "democrat","republican": 2 2 1 1 1 1 1 2 2 1 .

#of categorical attributes 
ncol(data)       #17
# of numeric attributes


#converting y into 1 and n to 0 
data1<-data.frame(ifelse(data[,2:17]=='n',0,ifelse(data[,2:17]=='y',1,NA)))

data1<-data$classnames
#View(data1)

#writing new data in csv file
write.csv(data,"newdata.csv")

#////////////////////////////
#/////Data Visualization////      
#///////////////////////////

#ploting percentage of missing values 
#plot(data)

#barplots for specific issue
plot(as.factor(data[,2]))
title(main='Votes cast for issue 1', xlab="vote", ylab= "Num reps" )

data_class_length<-data$classname.length
plot(data_class)

head(data)
voting_data<-data
#by party
Republican <-  voting_data$classname=="republican"
Democrat <- voting_data$classname=="democrat"

votingsummary<-summary(voting_data)
votingsummary

head(voting_data)
describe(voting_data)

qplot(voting_data)
qplot(x = classname, data = voting_data, geom = "bar")
qplot(x = immigration,data = voting_data, geom = "bar")

#using ggplot we can print Value Freqency and Proportions of each attribute/variable 
ggplot(voting_data,aes(x=classname),colorspaces) + geom_bar() + labs(y="no of bills",title="congress voting")
ggplot(voting_data, aes(x=classname),colors(distinct=T)) + theme_bw() +geom_bar() + labs(y="no of bills",title="congress voting")
ggplot(voting_data, aes(x=classname),colours()) + theme_bw() +geom_bar() + labs(y="no of bills",title="congress voting")
ggplot(voting_data, aes(x=classname),colours()) + theme_classic() +geom_bar() + labs(y="no of bills",title="congress voting")

prop.table(table(voting_data$classname))               

ggplot(voting_data,aes(x=voting_data$immigration),colorspaces) + geom_bar() + labs(y="no of bills",title="congress voting")
prop.table(table(voting_data$'immigration'))

data1<-data.frame(apply(data, 2, function(x) as.numeric(x)))
str(data1)
data<-data.frame(apply(data, 2, 'factor'))
View(data)

for(i in 1:16){
  plot(data[,i])
  dev.copy(jpeg,filename=paste(names(data[i]),"plot.jpeg",sep=" " ))
    dev.off()
}

n_class <- function (col,cls){return(sum(is.na(data[,col]) & data$classname==cls))}
n_class

p_y_class <- function(col,cls){
sum_y<-sum(data[,col]=="y" & data$classname==cls,na.rm = TRUE)
sum_n<-sum(data[,col]=="n" & data$classname==cls,na.rm = TRUE)
return(sum_y/(sum_y+sum_n))
}

p_y_class
p_y_class(5,"democrat")

# for (i in 2:ncol(data)) 
# {
#   if(sum(is.na(data[,i])>0)) 
#    {
#     c1 <- which(is.na(data[,i])& data$Class=="democrat",arr.ind = TRUE)
#     c2 <- which(is.na(data[,i])& data$Class=="republican",arr.ind = TRUE)
#     data[c1,i] <- ifelse(runif(n_class(i,"democrat"))<p_y_class(i,"democrat"),"y","n")
#     data[c2,i] <- ifelse(runif(n_class(i,"republican"))<p_y_class(i,"republican"),"y","n")}
# }

#View(data)

#removing target variable from data 
data_without_target <- data[,-c(1)]
head(data_without_target)
dim(data_without_target)

#reading target variable in another data frame
data_targetvarible<- data.frame(data[,c(1)])
dim(data_targetvarible)


library(DMwR)
#Imputing missing value with Central Imputation(Mean)
Imp_data_without_target<-centralImputation(data_without_target)

#View(Imp_data_without_target)
summary(Imp_data_without_target)
str(Imp_data_without_target)


#Imputing missing valus with Knn imputation
#Imp_knn_data_without_target<- knnImputation(data_without_target,k = 2)
#(knn does't work because here hamming distance doen't work for chategorical)
#spliting data in to train and test data


#strandarize data
library(MASS)
library(vegan)
# 
# data_stand<-decostand(Imp_data_without_target,"max", method = "log",na.rm = F ) 
# data_stand<- decostand(Imp_data_without_target, "chi.square")

#spting data into train and test
data_train
names(voting_data)

#dummify
#install.packages("dummies")
library(dummies)

data_dummies<-dummy.data.frame(data_without_target,dummy.classes = c("integer","factor"))
class(data_dummies)
str(data_dummies)

library(vegan)
data_stand<-decostand(data_dummies,method='range')
#View(data_stand)


#normalize
data_normalize<-decostand(data_dummies,method='normalize')
#View(data_normalize)
str(data_normalize)

names(data_normalize)



#////////////////////////////////////////




