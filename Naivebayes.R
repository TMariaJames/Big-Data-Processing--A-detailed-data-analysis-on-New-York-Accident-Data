data=read.csv(file.choose())
head(data)
str(data)

data=data[c('COLLISION_ID','CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
#getting the most frequent vehicle to cause an accident
tail(names(sort(table(data$VEHICLE.TYPE.CODE.1))), 3)
#subsetting the data for the most frequent vehicle in accident
d2=subset(data,data$VEHICLE.TYPE.CODE.1=="Station Wagon/Sport Utility Vehicle" | data$VEHICLE.TYPE.CODE.1=="PASSENGER VEHICLE")
str(d2)

#data preprocessing
d2=na.omit(d2)
d2 <- subset(d2, VEHICLE.TYPE.CODE.2 =!"")
d2 <- subset(d2,VEHICLE.TYPE.CODE.1  != "")
d2 <- subset(d2,CONTRIBUTING.FACTOR.VEHICLE.2  != "")
d2 <- subset(d2,CONTRIBUTING.FACTOR.VEHICLE.1  != "")
d2 <- subset(d2,NUMBER.OF.PERSONS.KILLED   != "")
d2 <- subset(d2,NUMBER.OF.PERSONS.INJURED    != "")
d2 <- subset(d2,ON.STREET.NAME     != "")
d2 <- subset(d2,ZIP.CODE     != "")
d2 <- subset(d2,BOROUGH     != "")
d2 <- subset(d2,CRASH.TIME      != "")
d2 <- subset(d2,NUMBER.OF.PEDESTRIANS.INJURED != "")
d2 <- subset(d2,NUMBER.OF.PEDESTRIANS.KILLED!= "")
d2 <- subset(d2,LOCATION      != "")
str(d2)

#splitting
rand_data <- d2[sample(nrow(d2), size=300), ]
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)
#NaiveBayes
library(e1071)
model2=naiveBayes(VEHICLE.TYPE.CODE.1~.,data=train.data)
predictions=predict(model2,train.data,type="raw")
head(predictions)
pred=ifelse(predictions[,1]>=.5,"PASSENGER VEHICLE","Station Wagon/Sport Utility Vehicle")
prop.table(table(pred,train.data$VEHICLE.TYPE.CODE.1))

           