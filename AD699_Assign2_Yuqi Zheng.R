library(dplyr) 
library(tidyr)
library(ggplot2) 
library(GGally)
library(corrplot)
library(caret)
library(ggmosaic)


#Simple Linear Regression
#1
lendingclub<-read.csv("lendingclub.csv")
dim(lendingclub)
View(lendingclub)

#2
lendingclub<-filter(lendingclub,revol_util <=100)
range(lendingclub$revol_util)

#3
ggplot(lendingclub,aes(x=revol_util,y=int_rate))+geom_hex(bins=12)+scale_fill_gradient(low="yellow",high="red")+geom_smooth(method="lm")

#4
cor(lendingclub$revol_util,lendingclub$int_rate)
    
#5
dim(lendingclub)
set.seed(480)
club <- sample_n(lendingclub, 2131401)
View(club)
2131401*0.6
train <- slice(club, 1:1278841)
valid <- slice(club, 1278842:2131401)

#6
options(scipen=999)
linearClubTrain<-lm(int_rate~revol_util, data=train)
linearClubTrain
summary(linearClubTrain)



#8
range(train$revol_util)
df_pred<-predict(linearClubTrain,data.frame(revol_util=25))
df_pred


#9
library(forecast)
predTrain <- predict(linearClubTrain, train)
accuracy(predTrain, train$int_rate)

predValid <- predict(linearClubTrain, valid)
accuracy(predValid, valid$int_rate)


#Multiple Linear Regression
str(train)
#1
library(corrplot)
names(train)
numbers<-train[,c(2:4,7,10,13:22)]
  
traincor <- cor(numbers,use='complete.obs')
traincor
corrplot(traincor)

df_new<-train[,c(2:4,6,7,18,22)]
cor(df_new,use='complete.obs')


train2<-train[,-c(1,3,4,7,18)]
names(train2)


#2
Club1<-lm(int_rate~.,train2)
summary(Club1)


#3
Club1Step <- step(Club1, direction = "backward")
summary(Club1Step)
View(train2)


#5
train3<-train2[,-c(4,5)]

Club2<-lm(int_rate~.,train3)
summary(Club2)
Club2Step2 <- step(Club2, direction = "backward")
summary(Club2Step2)



#6
train4<-na.omit(train3)
ratemove<-train4$int_rate-mean(train4$int_rate)
ratemovesq<-ratemove^2
SST<-sum(ratemovesq)
SST

modelexp<-Club2Step2$fitted.values-mean(train4$int_rate)  
modelexpsq<-modelexp^2
SSR<-sum(modelexpsq)
SSR

SSR/SST


#7
library(visualize)
visualize.t(stat=c(-1.676,1.676),df=95904,section = "bounded")
visualize.t(stat=c(-1.676,1.676),df=95904,section = "tails")


#8 
temp<-train2[,c(1,6,9:17)]
sapply(temp,range,na.rm=TRUE)

table(train2$dti)

Tina <- data.frame(loan_amnt =30000,term=" 60 months",emp_length="7 years",home_ownership="RENT",annual_inc=777777,verification_status="Verified", purpose="small_business",dti=2,delinq_2yrs=32,inq_last_6mths =15,mths_since_last_delinq =160,mths_since_last_record=77,pub_rec =5,revol_bal=500000,revol_util=17,total_acc=60)
predTina <-predict(Club2Step2,Tina)
predTina


#9
predTrain2 <- predict(Club2Step2, train2)
accuracy(predTrain2, train2$int_rate)


predValid2 <- predict(Club2Step2, valid)
accuracy(predValid2, valid$int_rate)


