
#removing the previous existed global environment variables 
rm(list=ls(all=TRUE))
setwd("D:/DATA SCIENCE/Internship")
getwd()
#
#-------------------------------------------------------------------------------------------------------------------------
#
#install.packages("mlr") # it has all required machine learning libraries 
library(mlr)  # it has all required machine learning libraries 
#install.packages("ggplot2")
library(ggplot2) # create elgant data Visualisation using the grammer of graphics
library(dplyr) #A grammer of data manipulation

#-------------------------------------------------------------------------------------------------------------------------
#reading the data in txt to table format 
#data <- read.table("house-votes-84.data.txt",header=TRUE,sep=",")#here first row is treetd as header 
data2 <- read.table("house-votes-84.data.txt",sep=",")#witout header #we got actual data
#-------------------------------------------------------------------------------------------------------------------------
#Viewing the data to understand the data set 
#View(data)
View(data2)
#--------------------------------------------------------------------------------------------------------------------------
#checking for the data dimenstions 
#dim(data)  #434 rows #17 columns (with header)
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

#V1 to V7 all are Categorical, having 3 levels expect V1(2 levels)
#----------------------------------------------------------------------------------------------------------------------------














