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








