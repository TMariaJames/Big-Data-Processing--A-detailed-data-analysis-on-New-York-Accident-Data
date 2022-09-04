data=read.csv(file.choose())
data=data[c('COLLISION_ID','CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
            'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
tail(names(sort(table(data$CONTRIBUTING.FACTOR.VEHICLE.1))), 3)
d1=subset(data,(data$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction" |  data$CONTRIBUTING.FACTOR.VEHICLE.1=="Failure to Yield Right-of-Way"))


#data preprocessing
d1=na.omit(d1)
d1 <- subset(d1, VEHICLE.TYPE.CODE.2 =!"")
d1 <- subset(d1,VEHICLE.TYPE.CODE.1  != "")
d1 <- subset(d1,CONTRIBUTING.FACTOR.VEHICLE.2  != "")
d1 <- subset(d1,CONTRIBUTING.FACTOR.VEHICLE.1  != "")
d1 <- subset(d1,NUMBER.OF.PERSONS.KILLED   != "")
d1 <- subset(d1,NUMBER.OF.PERSONS.INJURED    != "")
d1 <- subset(d1,ON.STREET.NAME     != "")
d1 <- subset(d1,ZIP.CODE     != "")
d1 <- subset(d1,BOROUGH     != "")
d1 <- subset(d1,CRASH.TIME      != "")
d1 <- subset(d1,NUMBER.OF.PEDESTRIANS.INJURED != "")
d1 <- subset(d1,NUMBER.OF.PEDESTRIANS.KILLED!= "")
d1 <- subset(d1,LOCATION      != "")
str(d1)

#Number of persons killed is the dependent variable
data <- subset(data, VEHICLE.TYPE.CODE.2 =!"")
data <- subset(data,VEHICLE.TYPE.CODE.1  != "")
data <- subset(data,CONTRIBUTING.FACTOR.VEHICLE.2  != "")
data <- subset(data,CONTRIBUTING.FACTOR.VEHICLE.1  != "")
data <- subset(data,NUMBER.OF.PERSONS.KILLED   != "")
data <- subset(data,NUMBER.OF.PERSONS.KILLED   != 0)
data <- subset(data,NUMBER.OF.PERSONS.INJURED    != "")
data <- subset(data,NUMBER.OF.PERSONS.INJURED    != 0)
data <- subset(data,ON.STREET.NAME     != "")
data <- subset(data,ZIP.CODE     != "")
data <- subset(data,BOROUGH     != "")
data <- subset(data,CRASH.TIME      != "")

data<- subset(data,LOCATION!="")
data <- subset(data, ZIP.COE  =!"")
data <- subset(data, NUMBER.OF.PEDESTRIANS.INJURED  !="")
data <- subset(data, NUMBER.OF.PEDESTRIANS.INJURED  !=0)
data <- subset(data,  NUMBER.OF.PEDESTRIANS.KILLED   !="")
data <- subset(data,  NUMBER.OF.PEDESTRIANS.KILLED   !=0)
data <- subset(data,   NUMBER.OF.CYCLIST.INJURED   1="")
data <- subset(data,   NUMBER.OF.CYCLIST.INJURED   !=0)
data <- subset(data,   NUMBER.OF.CYCLIST.KILLED   !="")
data <- subset(data,   NUMBER.OF.CYCLIST.KILLED   =!0)
data <- subset(data,   NUMBER.OF.MOTORIST.INJURED   =!"")
data <- subset(data,   NUMBER.OF.MOTORIST.INJURED   =!0)
data <- subset(data,   NUMBER.OF.MOTORIST.KILLED    =!"")
data <- subset(data,   NUMBER.OF.MOTORIST.KILLED    =!0)
data <- subset(data, COLLISION_ID  =!"")
data=na.omit(data)
str(data)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)



#splitting
rand_data <- d1[sample(nrow(d1), size=20000), ]
rand_data$CRASH.TIME  <- as.numeric(as.factor(rand_data$CRASH.TIME))
rand_data$BOROUGH  <- as.numeric(as.factor(rand_data$BOROUGH))
rand_data$CONTRIBUTING.FACTOR.VEHICLE.1 <- as.numeric(as.factor(rand_data$CONTRIBUTING.FACTOR.VEHICLE.1))
rand_data$VEHICLE.TYPE.CODE.1  <- as.numeric(as.factor(rand_data$VEHICLE.TYPE.CODE.1))
rand_data=rand_data[c('CRASH.TIME','BOROUGH','NUMBER.OF.PERSONS.INJURED',
                      'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1',
                      'VEHICLE.TYPE.CODE.1','NUMBER.OF.PEDESTRIANS.INJURED',
                      'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)              
train.data[-5]=scale(train.data[-5])
test.data[-5]=scale(test.data[-5])
#model
library(e1071)
classifier = svm(formula =  CONTRIBUTING.FACTOR.VEHICLE.1~ .,
                 data = train.data,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test.data[-5])
head(y_pred)

# Making the Confusion Matrix
cm = table(test.data[, 5], y_pred)






