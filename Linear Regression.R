data=read.csv(file.choose())

str(data)



#--missing value---#
data<- na.omit(data)
data=data[c('BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2')]
data <- subset(data, VEHICLE.TYPE.CODE.2 =!"")
data <- subset(data,VEHICLE.TYPE.CODE.1  != "")
data <- subset(data,CONTRIBUTING.FACTOR.VEHICLE.2  != "")
data <- subset(data,CONTRIBUTING.FACTOR.VEHICLE.1  != "")
data <- subset(data,NUMBER.OF.PERSONS.KILLED   != "")
data <- subset(data,NUMBER.OF.PERSONS.INJURED    != "")
data <- subset(data,ON.STREET.NAME     != "")
data <- subset(data,ZIP.CODE     != "")
data <- subset(data,BOROUGH     != "")
data <- subset(data,CRASH.TIME      != "")
#---Regression---#
rand_data <- data[sample(nrow(data), size=3000), ]
rand_data <- na.omit(rand_data)
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
head(dat.d)
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
str(data)
l=lm(NUMBER.OF.PERSONS.INJURED~ .  ,data=train.data)
summary(l)

y_pred=predict(l, new_data=test.data)
head(y_pred)

#backward elimination
#do each varibale
l=lm(NUMBER.OF.PERSONS.INJURED~ .  ,data=train.data)
summary(l)
#load car package
library(car)

#produce added variable plots
avPlots(l)




