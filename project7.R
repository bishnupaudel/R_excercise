# Project 7: Hospital cost analysis

library(readxl)
library(ggplot2)
library(plotly)


# set working directory
setwd("C:/Users/ribis/Documents/Simplilearn_Data Scientist/Projects/Porject 7")

data_1 = read_xlsx("1555054100_hospitalcosts.xlsx") # read the xlsx file

View(data_1) # viewing data

summary(data_1) # summarizes each columns

names(data_1) # read name of headings


# AIM 1: To record the patient statistics, the agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure
is.na(data_1["AGE"]) # checking NA if exist

data_2 = na.omit(data_1["AGE"]) # not require in this case as NA not exist in AGE data

hist(data_1$AGE)  # create distribtution of AGE, based on histogram infant of age group 0-1 has higest fequency of visiting hospital



plot(data_1$AGE, data_1$TOTCHG) # scatter plot showing expenditure by different age group


infant <- as.factor(data_1$AGE) # create a fator

summary(infant) #  here 0 age group has maximum 307 times discharge rate meaning frequently visiting hospital


tapply(data_1$TOTCHG, data_1$AGE, sum) # finds total expenditure related to particular age group

which.max(tapply(data_1$TOTCHG,data_1$AGE,sum)) 

#AIM 2: In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure

diagnosis_expense <-as.factor(data_1$APRDRG) # converted into a factor
View(diagnosis_expense)
is.na(diagnosis_expense)

summary(diagnosis_expense) # summarize the data labelleing frequency of occurence

which.max(summary(diagnosis_expense)) # find maxium value and number of iterations. It shows 640 is the maximum value repeated for 44 times

tapply(data_1$TOTCHG, diagnosis_expense, sum) # divide the data set into groups and apply sum for the data set into each group

which.max(tapply(data_1$TOTCHG, diagnosis_expense, sum)) # provides maximum value 

max(tapply(data_1$TOTCHG, diagnosis_expense, sum)) # max value of tapply function 

library(plot3D)
ggplot(data=data_1, aes(x=data_1$LOS, y=data_1$APRDRG)) + geom_density2d()

xyplot(data_1$APRDRG~data_1$TOTCHG | data_1$LOS) # this plot shows that 640 entries, longest stay and maximum cost on hospitalization


# Aim 3: To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.

# Linear regression model is applicable to address this aim


malpractice <- as.factor(data_1$RACE)
View(malpractice)
summary(malpractice)

# Here NA's are present so need to remode those data
malpracticena <- na.omit(data_1$RACE)

# Now fitting to a regression model using RACE as independent variable
# Null hypothesis: Racism exist during medical practice leading high expenditure
# Alternative hypothesis (Ha): Racism does not exit during medical practice
fit1 <- lm(data_1$TOTCHG~data_1$RACE)
summary(fit1) 
plot(fit1)
# Here p-value is observe 0.68 which is greater than 0.5, so fail to accept Null hypothsis, which mean there is no reacism
# exist during medical treatment which lead to high hopital cost

#Aim 4: To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.

fit2 <- lm(data_1$TOTCHG~data_1$AGE + data_1$FEMALE)
summary(fit2)
fit2$fitted.values
plot(fit2)

fit3 = lm(data_1$TOTCHG~ data_1$AGE, data = data_1)
fit3$fitted.values
summary(fit3)

fit4 = lm(data_1$TOTCHG~data_1$FEMALE, data=data_1)
fit4$fitted.values
summary(fit4) 

#Aim 5: Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
fit5 <- lm(data_1$LOS~data_1$AGE + data_1$FEMALE + data_1$RACE, data=data_1)
fit5$fitted.values
summary(fit5)
plot(fit5)

# Aim 6: To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.
#data_1na <- na.omit(data_1)
library(fastDummies)

# dummy_race <- dummy_cols(data_1, select_columns = 'RACE', remove_selected_columns = TRUE)
# View(dummy_race)
library(dplyr)
df1 = data.frame(data_1na)
View(df1)
names(df1)

summary(df1)
fit6 <- lm(df1$TOTCHG~. , data=data_1na)
summary(fit6)
plot(fit6)


library(caret)
fit7 <- lm(df1$TOTCHG~. , data=data_1na)
varImp(fit7, scale=FALSE)
