#Intership programe at INSOFE
#Project: US Congressional Voting Records Data Set
#Team(size 1): C Vinod Kumar
#-----------------------------------------------------------------------------------------------------------------
#Abstract: 
#US Congressmen voting records datset for 16 bills of previous year 

#Objective: To bulid a model and predict ---which party bill will-----?
------------------------------------------------------------------------------------------
setwd("D:/DATA SCIENCE/Internship")
getwd()

rm(list=ls(all=TRUE))

install.packages("mlr") # it has all required machine learning libraries 
library(mlr)  # it has all required machine learning libraries 
install.packages("ggplot2")
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation
library(plyr) #split- apply- combine
library(e1071) #load e1071 library and invoke naiveBayes method

#reading data in to table format 
data<-read.table("house-votes-84.data.txt",sep=",")
#write.csv(data,"uscongress_voting.csv")

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
lables = c("class Name","handicapped-infants","water-project-cost-sharing",
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

#checking for the data dimenstions 
data_dim<-dim(data)                 #435 rows #17 columns

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
# replacing "?" with "NA" # Recoding Values to Missing 
data[data=="?"]<-NA

# of missing values after replacing ? with NA
sum(is.na(data))             # 392 null values

#% missing values
sum(is.na(data))
colSums(is.na(data))
rowSums(is.na(data))
sum(is.na(data$crime))
sapply(data, function(x) sum(is.na(x)))
ndata<-data_dim[1]*data_dim[2]

#total %of missing vaules in data set
per_total<-((sum(is.na(data))/ndata)*100)
per_total

#% of each variable percentage of missing values 
per_mis<-(colSums(is.na(data))/nrow(is.na(data)))*100
per_mis

#sorting data from highest null value variable
#sort(colSums(is.na(data)),decreasing = TRUE)
#sort(per_mis,decreasing =TRUE)

###of Levels in Taget Variable

#of categorical attributes to 

# of numeric attributes

#converting y into 1 and n to 0 
#data1<-data.frame(ifelse(data[,2:17]=='n',0,ifelse(data[,2:17]=='y',1,NA)))
#data1$<-data$V1

#View(data1)

#writing new data in csv file
write.csv(data,"newdata.csv")

#////////////////////////////
#/////Data Visualization////      
#///////////////////////////

#ploting percentage of missing values 
plot(data)

#barplots for specific issue
plot(as.factor(data[,2]))
title(main='Votes cast for issue 1', xlab="vote", ylab= "Num reps" )

str(data)
summary(data)

data1<-data.frame(apply(data, 2, function(x) as.numeric(x)))
str(data1)
data<-data.frame(apply(data, 2, 'factor'))

for(i in 1:16){
  dev.copy(jpeg,filename=paste(names(data[i]),"plot.jpeg",sep=" " ))
  plot(data[,i])
  dev.off()
}




