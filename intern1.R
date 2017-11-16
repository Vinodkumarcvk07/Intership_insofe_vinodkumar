
#Intership programe at INSOFE
#Project: Congressional Voting Records Data Set
#Team(size 1): C Vinod Kumar
#-----------------------------------------------------------------------------------------------------------------
#Abstract: US Congressmen voting records datset for 16 random/important 16 bills of previous Quater/year 


#Objective: To bulid a model and predict ---which congress bill pass -----?

#-------------------------------------------------------------------------------------------------------------------
#removing the previous existed global environment variables 
rm(list=ls(all=TRUE))
#-------------------------#
#Seting working directory #
#-------------------------#
setwd("D:/DATA SCIENCE/Internship")
getwd()
#-------------------------------------------------------------------------------------------------------------------------
#
#install.packages("mlr") # it has all required machine learning libraries 
library(mlr)  # it has all required machine learning libraries 
#install.packages("ggplot2")
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation
library(plyr) #split- apply- combine

#-------------------------------------------------------------------------------------------------------------------------
#reading the data in txt to table format 
data2 <- read.table("house-votes-84.data.txt",sep=",")#witout header #we got actual data

#-------------------------------------------------------------------------------------------------------------------------
#Viewing the data to understand the data set 
View(data2)
#--------------------------------------------------------------------------------------------------------------------------
#checking for the data dimenstions 
dim(data2) #435 rows #17 columns  (with out header)

#435 USCongress men and 16 varibles represent 16 types of bills voted for previous year 

#--------------------------------------------------------------------------------------------------------------------------
#summary(data)# read.table data set
summary(data2) # I can see 

   #V1              V2      V3      V4      V5      V6      V7      V8      V9      V10     V11     V12     V13     V14    
#democrat  :267   ?: 12   ?: 48   ?: 11   ?: 11   ?: 15   ?: 11   ?: 14   ?: 15   ?: 22   ?:  7   ?: 21   ?: 31   ?: 25  
#republican:168   n:236   n:192   n:171   n:247   n:208   n:152   n:182   n:178   n:206   n:212   n:264   n:233   n:201  
#                 y:187   y:195   y:253   y:177   y:212   y:272   y:239   y:242   y:207   y:216   y:150   y:171   y:209  

#V15     V16     V17    
#?: 17   ?: 28   ?:104  
#n:170   n:233   n: 62  
#y:248   y:174   y:269

#It is a classification problem V1 is the target vairable 

#from V2 to V17 shows how many voted yes or no or not voted 

#--------------------------------------------------------------------------------------------------------------------------

#view the structure of data set, attributes and data types
str(data2)
#It is a Data frame having 435 rows/observations and 17 columns/variables
#V1 to V17 all are Factor, having 3 levels expect V1 has (2 levels)-- Y or N ,Democratic or Republican
#----------------------------------------------------------------------------------------------------------------------------
is.data.frame(data2)
#TRUE
#--------------------------------------------------------------------------------------------------------------------------
#vaiable names
names(data2)

 #[1] "V1"  "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11" "V12" "V13" "V14"
#[15] "V15" "V16" "V17"
#-----------------------------------------------------------------------------------------
head(data2)
#          V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14 V15 V16 V17
#1 republican  n  y  n  y  y  y  n  n   n   y   ?   y   y   y   n   y
#2 republican  n  y  n  y  y  y  n  n   n   n   n   y   y   y   n   ?
#3   democrat  ?  y  y  ?  y  y  n  n   n   n   y   n   y   y   n   n
#4   democrat  n  y  y  n  ?  y  n  n   n   n   y   n   y   n   n   y
#5   democrat  y  y  y  n  y  y  n  n   n   n   y   ?   y   y   y   y
#6   democrat  n  y  y  n  y  y  n  n   n   n   n   n   y   y   y   y
#--------------------------------------------------------------------------------
sum(is.na(data2)) 
# it is showing 0 null values
#---------------------------------------------------------------------------------
data2[data2=="?"]<- NA  # assign "?"  values with NA
head(data2) #now ? is raplaced with NA
#---------------------------------------------------------------------------------
sum(is.na(data2)) #it is showing #392 null values
#--------------------------------------------------------------------------------
#checkig how much percentage of missing values are there in dataset
count(data2$V1) 
#          x freq 
#   democrat  267
# republican  168
data2[count(frequency(data2$V1,exclude = "democrat"))]
count(data2$V2)
sum(is.na(data2$V1))
sum(is.na(data2$V2))
count(data2$V2)         
colSums(~.,data2)         

unique(data2$V1)#e="democrat")
unique(is.na((data2$V2))
unique(data2$V10)

summarizeColumns(data2)
summarizeColumns(data2$V)
summarizeCo

head(data2)

sum(is.na(data2$V1))
sum(is.na(data2$V3=="n")) #48
sum(is.na(data2$V16=="n")) #28
sum(is.na(data2$V1,]))
sum(is.na(data2))
sum(is.na(data2))
#----------------------------------------------
for (i in 1:16)
{
  sum((is.na(data2[,i]))
}


#checking each column and each row hou much percentage value are missing values 


#-------------------------------------------------------------------------------

plot(data2$V1,data2$V2)
plot(data2$V2,data2$V3)
plot(data2$V3)

#----------------------------------------
for (i in 1:16)
{
  plot(data2[,i])
}
#================
for(i in 1:16)
{
  dev.copy(jpeg,filename=paste(names(data2[i]),"plot.jpeg",sep="@" ))
  plot(data2[,i])
  dev.off()
}

barplot(as.matrix(data2), main="congressvotes", xlab= "type of votes",ylab= "no of votes polled",
        ,col=rainbow())



