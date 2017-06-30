library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)

getwd()
setwd("C:/Users/t_yanju/Documents/DSStudyGroup/ConversionRate/ConversionRate")
conversion_data=read.csv('conversion_data.csv')
str(conversion_data)
summary(conversion_data)

sort(unique(conversion_data$age),decreasing =TRUE) 

subset(conversion_data,age>79)

data=subset(conversion_data,age<80)


data%>%
  group_by(country)%>%
  summarise(conversion_rate=mean(converted))%>%
  ggplot(aes(x=reorder(country,-conversion_rate),y=conversion_rate))+
  geom_bar(stat='identity',fill='light blue')


data%>%
  group_by(total_pages_visited)%>%
  summarise(conversion_rate=mean(converted))%>%
  ggplot(aes(x=total_pages_visited,y=conversion_rate))+geom_line()

data%>%
  group_by(source)%>%
  summarise(conversion_rate=mean(converted))%>%
  ggplot(aes(x=reorder(source,-conversion_rate),y=conversion_rate))+
  geom_bar(stat='identity',fill='light blue')

newuser_country=data%>%
  group_by(country,new_user)%>%
  summarise(conversion_rate=mean(converted))

ggplot(newuser_country,aes(x=factor(country),y=conversion_rate, fill=factor(new_user)))+
  geom_bar(position='dodge',stat='identity')

#++++++++++++++++++++++++++++++++++++++++++++++++

data$converted=as.factor(data$converted)
data$new_user=as.factor(data$new_user)
data$country=as.factor(data$country)

#++++++++Logistic Regression

LR_train=data[sample(nrow(data),size=nrow(data)*0.66),]
LR_test=data[-sample(nrow(data),size=nrow(data)*0.66),]
glm.fit1=glm(converted~.,data=LR_train,family='binomial')
summary(glm.fit1)
glm.prob1=predict(glm.fit1,type='response')
glm.predict1=ifelse(glm.prob1>0.5,'1','0')
table(glm.predict1,LR_train$converted)
mean(glm.predict1!=LR_train$converted)




