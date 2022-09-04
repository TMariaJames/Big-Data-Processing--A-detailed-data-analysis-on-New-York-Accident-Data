# K-Means Clustering

# Importing the dataset
data=read.csv(file.choose())


d1=data[c('CONTRIBUTING.FACTOR.VEHICLE.1','NUMBER.OF.PERSONS.INJURED')]
d1=subset(d1,
          (d1$BOROUGH=="BROOKLYN" | d1$BOROUGH=="QUEENS") 
)
str(d1)
d1 <- data[sample(nrow(data), size=1000), ]
#data preprocessing

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
d1=na.omit(d1)

str(d1)            
d=data[c('CRASH.TIME','BOROUGH','ZIP.CODE','NUMBER.OF.PERSONS.INJURED',
                'CONTRIBUTING.FACTOR.VEHICLE.1','CONTRIBUTING.FACTOR.VEHICLE.2',
                 'VEHICLE.TYPE.CODE.1','VEHICLE.TYPE.CODE.2','NUMBER.OF.PEDESTRIANS.INJURED',
      
                 'NUMBER.OF.CYCLIST.INJURED','NUMBER.OF.MOTORIST.INJURED')]
#getting the most frequent vehicle to cause an accident
d$CONTRIBUTING.FACTOR.VEHICLE.1  <- as.numeric(as.factor(d$CONTRIBUTING.FACTOR.VEHICLE.1))
d$CONTRIBUTING.FACTOR.VEHICLE.2 <- as.numeric(as.factor(d$CONTRIBUTING.FACTOR.VEHICLE.2))
d$VEHICLE.TYPE.CODE.1   <- as.numeric(as.factor(d$VEHICLE.TYPE.CODE.1 ))
d$VEHICLE.TYPE.CODE.2  <- as.numeric(as.factor(d$VEHICLE.TYPE.CODE.2))
d$LOCATION <- as.numeric(as.factor(d$LOCATION))
d$CRASH.TIME  <- as.numeric(as.factor(d$CRASH.TIME))
d$BOROUGH <- as.numeric(as.factor(d$BOROUGH))
d$ZIP.CODE  <- as.numeric(as.factor(d$ZIP.CODE))
d$ON.STREET.NAME   <- as.numeric(as.factor(d$ON.STREET.NAME ))
d=scale(d)
library(factoextra)
library(cluster)
fviz_nbclust(d, kmeans, method = "wss")

# Using the elbow method to find the optimal number of clusters
set.seed(9)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(d, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
#calculate gap statistic based on number of clusters
gap_stat <- clusGap(d,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)
#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(d, centers = 3, nstart = 25)

#view results
km
fviz_cluster(km, data = d)
# Fitting K-Means to the dataset
set.seed(2)
kmeans = kmeans(x = d, centers = 7)
# Visualising the clusters
fviz_cluster(kmeans, data = d)

aggregate(d, by=list(cluster=kmeans$cluster), mean)


final_data <- cbind(d, cluster = kmeans$cluster)

#view final data
head(final_data)
