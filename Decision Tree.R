data=read.csv(file.choose())
head(data)
str(data)
data=data[c('COLLISION_ID','CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
            'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
#getting the most frequent factor for an accident
tail(names(sort(table(data$CONTRIBUTING.FACTOR.VEHICLE.1))), 3)
#subsetting the data for the most frequent factor
d1=subset(data,data$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction" | data$CONTRIBUTING.FACTOR.VEHICLE.1=="Failure to Yield Right-of-Way")
str(d1)


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

#splitting
rand_data <- d1[sample(nrow(d1), size=300), ]
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)
#decision tree
library(rpart)

model1=rpart(CONTRIBUTING.FACTOR.VEHICLE.1~NUMBER.OF.PERSONS.INJURED+NUMBER.OF.PERSONS.KILLED+
               VEHICLE.TYPE.CODE.1+VEHICLE.TYPE.CODE.2+NUMBER.OF.PEDESTRIANS.INJURED+NUMBER.OF.PEDESTRIANS.KILLED,
             data=train.data,cp=0)
plot(model1)
text(model1)

model2=rpart(CONTRIBUTING.FACTOR.VEHICLE.1~.
             ,data=train.data,cp=0)


#testing the  model performance
pred=predict(model1,train.data,type='class')
head(pred)
head(test.data $CONTRIBUTING.FACTOR.VEHICLE.1)
tbl1<-table(pred, train.data $CONTRIBUTING.FACTOR.VEHICLE.1)
print(tbl1)
pred=predict(model2,train.data,type='class')
head(pred)
head(test.data $CONTRIBUTING.FACTOR.VEHICLE.1)
tbl2<-table(pred, train.data $CONTRIBUTING.FACTOR.VEHICLE.1)
print(tbl2)

