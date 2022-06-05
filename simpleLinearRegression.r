# This modle to create Linear Regression model
#set the work dirctory
setwd('G:/Data Science/Assignment')
#first read and load the data from my account github
df<-read.csv('https://raw.githubusercontent.com/AliHumadi2000/LinearRegression/main/delivery_time.csv')
head(df)
View(df)
# see the structure of the data
str(df)

#columns name 
colnames(df)

#see if there is duplicated or missing value 
sum(is.na(df)) #no missing value 

sum(duplicated(df))#no duplicated value

#shape of data
nrow(df) # 21 rows
ncol(df) # 2 columns

# visulization 

library(ggplot2)
#Predict delivery time using sorting time
scatter<-ggplot(df,aes(Sorting.Time,Delivery.Time))+geom_point(size=1,color="blue")+geom_smooth(method=lm, se=FALSE,col='red')
scatter

#we can observ there is residual error we can make it lease by using transformation or scalling 
#use min max scaaler
#build the model before scaling 
x<-df$Sorting.Time
y<-df$Delivery.Time
model<-lm(y~x,df)
model
summary(model)

#predict
pre<-predict(model)
pre

data.frame(ActualValue=df$Delivery,predictValue=pre)

MinMax<- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
df$Delivery.Time<-MinMax(df$Delivery.Time)
df$Sorting.Time<-MinMax(df$Sorting.Time)
View(df)

#see the plot after scaling

scatter1<-ggplot(df,aes(Sorting.Time,Delivery.Time))+geom_point(size=1,color="blue")+geom_smooth(method=lm, se=FALSE,col='black')
scatter1

#check if there is outlier

boxplot(df$Delivery.Time,col = 'red')
boxplot(df$Sorting.Time,col = 'black')
#no outlier

#build the model

library(caret)
x<-df$Sorting.Time
y<-df$Delivery.Time
model<-lm(y~x,df)
model
summary(model)

#predict
pre<-predict(model)
pre

data.frame(ActualValue=df$Delivery,predictValue=pre)

predict(10)
