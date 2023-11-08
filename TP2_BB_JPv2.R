#install.packages('tidyverse')
library(tidyverse)

#load the data set
dataset=read.csv("ds_salaries_2023.csv")

#missing data
paste('number of missing data',sum(is.na(dataset)))

#explore the data
view(dataset)
glimpse(dataset)
head(dataset) 
tail(dataset)
length(dataset) 
names(dataset) 
str(dataset)
summary(dataset)
paste('standard deviation',sd(dataset$salary_in_usd))
paste('standard deviation',sd(dataset$remote_ratio))
paste('standard deviation',sd(dataset$work_year))

#install.packages('psych')
library(psych)
describe(dataset)

#calculation frequency of categorical data
table(dataset$experience_level)
table(dataset$employment_type)
table(dataset$job_title)
table(dataset$salary_currency)
table(dataset$employee_residence)
table(dataset$company_location)
table(dataset$company_size)

#barplots
ggplot(data=dataset,
       aes(x=experience_level))+
  geom_bar()+
  labs(title = 'Barplot of Experience Level',
       x='Experience Level',
       y='Count')

ggplot(data=dataset,
       aes(x=employment_type))+
  geom_bar()+
  labs(title = 'Barplot of Employment Type',
       x='Employment Type',
       y='Count')

ggplot(data=dataset,
       aes(x=company_size))+
  geom_bar()+
  labs(title = 'Barplot of Company Size',
       x='Company Size',
       y='Count')

ggplot(data=dataset,
       aes(x=work_year))+
  geom_bar()+
  labs(title = 'Barplot of Work Year',
       x='Work Year',
       y='Count')

ggplot(data=dataset,
       aes(x=remote_ratio))+
  geom_bar()+
  labs(title = 'Barplot of Employees Working Remote',
       x='Experience Level',
       y='Count')

#Turning off scientific notation
options(scipen = 999)

#histogram
ggplot(data=dataset,
       aes(x=salary_in_usd))+
  geom_histogram()+
  labs(title = 'Histogram of Salary',
       x='Salary in USD',
       y='Count')

#boxplot
ggplot(data=dataset,
       aes(x=salary_in_usd))+
  geom_boxplot(fill='lightblue')+
  labs(title='Boxplot of height',
       x='Salary in USD')







# MLR on Salary 
names(dataset)
dataset2=dataset[sapply(dataset,is.numeric)]
names(dataset2)

library(caTools)
split=sample.split(dataset2$salary_in_usd,SplitRatio = .8)

training_set=subset(dataset2,split==TRUE)
test_set=subset(dataset2,split==FALSE)
MLR=lm(formula = salary_in_usd~.,
       data=training_set)
summary(MLR)
#Prediction on test set
y_pred=predict(MLR,newdata = test_set)
library(Metrics)

rmse(test_set$salary_in_usd,y_pred)
mae(test_set$salary_in_usd,y_pred)

saved=data.frame(test_set$salary_in_usd,y_pred)
head(saved)




# MLR on Remote Ratio
split=sample.split(dataset$remote_ratio,SplitRatio = .8)

training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
MLR=lm(formula = remote_ratio~.,
       data=training_set)
summary(MLR)
#Prediction on test set
y_pred=predict(MLR,newdata = test_set)
library(Metrics)

rmse(test_set$remote_ratio,y_pred)
mae(test_set$remote_ratio,y_pred)

saved=data.frame(test_set$remote_ratio,y_pred)
head(saved)


summary(data[sapply(data,is.numeric)])








#simple linear regression between rooms and values
dataset_new=dataset[c(1,7)]

#splitting the data into training and test set
library(caTools)

split=sample.split(dataset_new$salary_in_usd,SplitRatio = 2/3)
#2/3 data for training and 1/3 for testing
training_set=subset(dataset_new,split==TRUE)
test_set=subset(dataset_new,split==FALSE)

#training the model using training set
regressor_SLR=lm(formula = salary_in_usd~work_year,training_set)
summary(regressor_SLR)

#accuracy on test set
y_pred=predict(regressor_SLR,newdata = test_set)
result=data.frame(test_set$salary_in_usd,y_pred)
head(result)

#accuracy checkby MAE, MSE, RMSE
library(Metrics)
mae(test_set$salary_in_usd,y_pred)
mse(test_set$salary_in_usd,y_pred)
rmse(test_set$salary_in_usd,y_pred)

#single prediction
new=data.frame(work_year=2023)
#value of the house
predict(regressor_SLR,newdata = new)

#single prediction
new=data.frame(work_year=2022)
#value of the house
predict(regressor_SLR,newdata = new)

#single prediction
new=data.frame(work_year=2021)
#value of the house
predict(regressor_SLR,newdata = new)

#single prediction
new=data.frame(work_year=2020)
#value of the house
predict(regressor_SLR,newdata = new)






