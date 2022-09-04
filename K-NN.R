#K Nearest neigbourhood knn
data=read.csv(file.choose())
d1=data[c('COLLISION_ID','CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME','NUMBER.OF.PERSONS.INJURED',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PEDESTRIANS.INJURED','NUMBER.OF.PEDESTRIANS.KILLED',
            'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.CYCLIST.KILLED','NUMBER.OF.MOTORIST.INJURED','NUMBER.OF.MOTORIST.KILLED')]

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
tail(names(sort(table(d1$CRASH.TIME))), 3)
d1=subset(data,(data$CRASH.TIME=="16:00" | data$CRASH.TIME=="17:00"))
d1=d1[c('CRASH.TIME','BOROUGH','VEHICLE.TYPE.CODE.1'
                      )]
d1$CRASH.TIME  <- as.numeric(as.factor(d1$CRASH.TIME))
d1$BOROUGH  <- as.numeric(as.factor(d1$BOROUGH))
d1$CONTRIBUTING.FACTOR.VEHICLE.1 <- as.numeric(as.factor(d1$CONTRIBUTING.FACTOR.VEHICLE.1))
d1$VEHICLE.TYPE.CODE.1  <- as.numeric(as.factor(d1$VEHICLE.TYPE.CODE.1))

#splitting
rand_data <- d1[sample(nrow(d1), size=100), ]
rand_data=rand_data[,-10]
set.seed(123)
dat.d <- sample(1:nrow(rand_data),size=nrow(rand_data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- rand_data[dat.d,] # 70% training data
test.data <- rand_data[-dat.d,] # remaining 30% test data
str(test.data)              
train.data[-1]=scale(train.data[-1])
test.data[-1]=scale(test.data[-1])
train.data=na.omit(train.data)
test.data=na.omit(test.data)
#model building
install.packages('caTools')
library(class)
y_pred=knn(train=train.data[,-1],
           test=test.data[,-1],
           cl=train.data[,1],
           k=5)


head(y_pred)

cm=table(test.data[,1] , y_pred)
cm

#visualization
install.packages('ElemStatLearn')
library(ElemStatLearn)
set=train.data
x1=seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
x2=seq(min(set[,3])-1, max(set[,3])+1, by=0.01)
grid_set=expand.grid(x1,x2)
colnames(grid_set)=c('BOROUGH','VEHICLE.TYPE.CODE.1')
y_grid=y_pred=knn(train=train.data[,-1],
                  test=grid_set,
                  cl=train.data[,1],
                  k=5)
plot(set[, -1],
     main = 'K-NN (Training set)',
     xlab = 'BOROUGH', ylab = 'VEHICLE.TYPE.CODE.1',
     xlim = range(x1), ylim = range(x2))

contour(x1, x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 1] == 1, 'green4', 'red3'))











