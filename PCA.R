# PCA

# Importing the dataset
data=read.csv(file.choose())
data=data[c('CRASH.TIME','BOROUGH','ZIP.CODE','ON.STREET.NAME',
            'NUMBER.OF.PERSONS.KILLED','CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
            'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','LOCATION','NUMBER.OF.PERSONS.INJURED')]

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
data <- subset(data, NUMBER.OF.PEDESTRIANS.INJURED  =!"")
data <- subset(data, NUMBER.OF.PEDESTRIANS.INJURED  =!0)
data <- subset(data,  NUMBER.OF.PEDESTRIANS.KILLED   =!"")
data <- subset(data,  NUMBER.OF.PEDESTRIANS.KILLED   =!0)
data <- subset(data,   NUMBER.OF.CYCLIST.INJURED   =!"")
data <- subset(data,   NUMBER.OF.CYCLIST.INJURED   =!0)
data <- subset(data,   NUMBER.OF.CYCLIST.KILLED   =!"")
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


#building the model

# Feature Scaling


data$VEHICLE.TYPE.CODE.2  <- as.numeric(as.factor(data$VEHICLE.TYPE.CODE.2))
data$VEHICLE.TYPE.CODE.1  <- as.numeric(as.factor(data$VEHICLE.TYPE.CODE.1))
data$CONTRIBUTING.FACTOR.VEHICLE.2  <- as.numeric(as.factor(data$CONTRIBUTING.FACTOR.VEHICLE.2))
data$CONTRIBUTING.FACTOR.VEHICLE.1  <- as.numeric(as.factor(data$CONTRIBUTING.FACTOR.VEHICLE.1))
data$ON.STREET.NAME   <- as.numeric(as.factor(data$ON.STREET.NAME ))
data$ZIP.CODE   <- as.numeric(as.factor(data$ZIP.CODE ))
data$BOROUGH    <- as.numeric(as.factor(data$BOROUGH  ))
data$CRASH.TIME   <- as.numeric(as.factor(data$CRASH.TIME) )
data$LOCATION   <- as.numeric(as.factor(data$LOCATION) )
data$NUMBER.OF.PERSONS.KILLED=as.numeric(as.factor(data$NUMBER.OF.PERSONS.KILLED))
data$NUMBER.OF.CYCLIST.INJURED=as.numeric(data$NUMBER.OF.CYCLIST.INJURED)
data$NUMBER.OF.CYCLIST.KILLED=as.numeric(data$NUMBER.OF.CYCLIST.KILLED)
data$NUMBER.OF.MOTORIST.INJURED=as.numeric(data$NUMBER.OF.MOTORIST.INJURED)
data$NUMBER.OF.MOTORIST.KILLED =as.numeric(data$NUMBER.OF.MOTORIST.KILLED )
data$NUMBER.OF.PEDESTRIANS.INJURED=as.numeric(data$NUMBER.OF.PEDESTRIANS.INJURED)
#data$ NUMBER.OF.PERSONS.INJURED=as.factor(data$ NUMBER.OF.PERSONS.INJURED)
str(data)
set.seed(123)
dat.d <- sample(1:nrow(data),size=nrow(data)*0.7,replace = FALSE) #random selection of 70% data.
train.data <- data[dat.d,] # 70% training data
test.data <-data[-dat.d,] # remaining 30% test data
str(test.data)              

train.data[-11] = scale(train.data[-11])
test.data[-11] = scale(test.data[-11])

# Applying PCA
#install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pca = preProcess(x = train.data[-11], method = 'pca', pcaComp = 9)
train.data = predict(pca, train.data)
train.data = train.data[c(2,3,4,5,6,7,8,9 ,1)]
test.data = predict(pca, test.data)
test.data = test.data[c(2,3,4,5,6,7,8,9,  1)]
str(train.data)

l=lm(NUMBER.OF.PERSONS.INJURED~.  ,data=train.data)
summary(l)
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 2, 'deepskyblue', ifelse(y_grid == 1, 'springgreen3', 'tomato')))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, 'blue3', ifelse(set[, 3] == 1, 'green4', 'red3')))
