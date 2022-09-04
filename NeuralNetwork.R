data=read.csv(file.choose())
data=data[c('COLLISION_ID','CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
            'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
#getting the most frequent vehicle to cause an accident
tail(names(sort(table(data$CRASH.TIME))), 3)
tail(names(sort(table(data$BOROUGH))), 3)
tail(names(sort(table(data$CONTRIBUTING.FACTOR.VEHICLE.1))), 3)
tail(names(sort(table(data$VEHICLE.TYPE.CODE.1))), 3)


d1=subset(data,(data$VEHICLE.TYPE.CODE.1=="Station Wagon/Sport Utility Vehicle" | data$VEHICLE.TYPE.CODE.1=="PASSENGER VEHICLE" ) &
            (data$CRASH.TIME=="16:00" | data$CRASH.TIME=="17:00") &
            (data$BOROUGH=="BROOKLYN" | data$BOROUGH=="QUEENS") &
            (data$CONTRIBUTING.FACTOR.VEHICLE.1=="Driver Inattention/Distraction" |  data$CONTRIBUTING.FACTOR.VEHICLE.1=="Failure to Yield Right-of-Way"))
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
rand_data <- d1[sample(nrow(d1), size=1852), ]
rand_data$CRASH.TIME  <- as.numeric(as.factor(rand_data$CRASH.TIME))
rand_data$BOROUGH  <- as.numeric(as.factor(rand_data$BOROUGH))
rand_data$CONTRIBUTING.FACTOR.VEHICLE.1 <- as.numeric(as.factor(rand_data$CONTRIBUTING.FACTOR.VEHICLE.1))
rand_data$VEHICLE.TYPE.CODE.1  <- as.numeric(as.factor(rand_data$VEHICLE.TYPE.CODE.1))
rand_data=rand_data[c('CRASH.TIME','BOROUGH','NUMBER.OF.PERSONS.INJURED',
                      'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1',
                      'VEHICLE.TYPE.CODE.1','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
                      'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)              
train.data[-2]=scale(train.data[-2])
test.data[-2]=scale(test.data[-2])
#building the model
install.packages("neuralnet")
library(neuralnet)
nn=neuralnet(BOROUGH~CRASH.TIME+NUMBER.OF.PERSONS.INJURED+CONTRIBUTING.FACTOR.VEHICLE.1+
               VEHICLE.TYPE.CODE.1+ NUMBER.OF.CYCLIST.INJURED+NUMBER.OF.MOTORIST.INJURED, data=train.data,
             hidden=15,act.fct="logistic",
             linear.output = FALSE)
plot(nn)
pred=compute(nn,test.data)
pred$net.result
prob=pred$net.result
pred=ifelse(prob[,1]>0.5,"BROOKLYN","QUEENS")
new=data.frame(pred,test.data$BOROUGH)
cm = table(test.data[, 2], pred)
cm
