library(tidyverse)
str(DM)
View(DM)
#46 variables & 600 observations

#Looking for missing values
sum(is.na(DM)) #no missing values


#Data Cleaning
#Removing % sign from the data
DM[] <- lapply(DM, gsub, pattern='%', replacement='')
DM

#Converting Brand wise purchase variables to numeric
DM[,23:31] <- data.frame(apply(DM[23:31], 2, as.numeric))
str(DM) #Done
#Computed a new column as Maximum Brand Loyalty which is basically maximum value from all the Brand wise purchase variables
DM$Max_Brand <- apply(DM[23:30], 1, max)

#Performing k-means clustering to identify clusters of households based on customer purchase behavior
#Variables taken into consideration - no of brands, brand runs, total volume, no of transactions, value, Avg price, share to other brands & max to one bran
#Converting data types of customer purchase behavior variables
DM[,12:16] <- data.frame(apply(DM[12:16], 2, as.numeric))
DM$Avg_Price <- as.numeric(DM$Avg_Price)
DM$Others_999 <- as.numeric(DM$Others_999)
DM$Max_Brand <- as.numeric(DM$Max_Brand)

#Normalizing i.e. performing scaling on all numeric variables before applying k-means
DataNum <- DM[, c(12:16,19,31,47)]
mins <- apply(DataNum, 2, min)
maxs <- apply(DataNum, 2, max)

Data.scaled <- scale(DataNum, center = mins, scale = maxs - mins)
summary(Data.scaled)
View(Data.scaled)


install.packages("factoextra")
library(factoextra)
distance <- get_dist(Data.scaled)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Performing k-means clustering
set.seed(7)
km2 <- kmeans(Data.scaled, 2, nstart = 100)
km2
km3 <- kmeans(Data.scaled, 3, nstart = 100)
km3
km4 <- kmeans(Data.scaled, 4, nstart = 100)
km4
km5 <- kmeans(Data.scaled, 5, nstart = 100)
km5

p1 <- fviz_cluster(km2, geom = "point", data = Data.scaled) + ggtitle("k = 2")
p2 <- fviz_cluster(km3, geom = "point",  data = Data.scaled) + ggtitle("k = 3")
p3 <- fviz_cluster(km4, geom = "point",  data = Data.scaled) + ggtitle("k = 4")
p4 <- fviz_cluster(km5, geom = "point",  data = Data.scaled) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


#Elbow method
set.seed(123)
fviz_nbclust(Data.scaled, kmeans, method = "wss")

#Silhoutte measure
fviz_nbclust(Data.scaled, kmeans, method = "silhouette")
#From Silhoutte measure, 2 seems to be comparatively better with Silhoutte measure equal to be 0.36.


#Converting data types of selling proposition variables to numeric for further analysis
DM[,36:46] <- data.frame(apply(DM[36:46], 2, as.numeric))
str(DM)
#Visualizing Selling propositions variables for getting insights into the data
library(purrr)
library(tidyr)
library(ggplot2)

DM[,36:46] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

DM[,36:46] %>%
  keep(is.numeric) %>%                     
  gather() %>%                             
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_density()  

#Considering Selling propositions catergory from 5-9, 14 & 15.
#Ignoring all other selling propositions variable because they have very less values comparatively


#Converting data type of basis of purchase variable
DM[,20:22] <- data.frame(apply(DM[20:22], 2, as.numeric))
DM[,32:35] <- data.frame(apply(DM[32:35], 2, as.numeric))

#Performing normalization on the scaled data
#Normalizing i.e. performing scaling on all numeric variables before applying k-means
DataNum1 <- DM[, c(20:22,32:40,45:46)]
mins1 <- apply(DataNum1, 2, min)
maxs1 <- apply(DataNum1, 2, max)

Data.scaled1 <- scale(DataNum1, center = mins1, scale = maxs1 - mins1)
summary(Data.scaled1)
View(Data.scaled1)


#Performing k-means clustering to identify clusters of basis for purchase.
#Variables taken into consideration - Pur-vol-no-promo, Pur-vol-promo-6, Pur-vol-other, all price categories, selling propositions category from 5-9 & 14-15.
distance1 <- get_dist(Data.scaled1)
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#Performing k-means clustering
set.seed(123)
km22 <- kmeans(Data.scaled1, 2, nstart = 100)
km22
km33 <- kmeans(Data.scaled1, 3, nstart = 100)
km33
km44 <- kmeans(Data.scaled1, 4, nstart = 100)
km44
km55 <- kmeans(Data.scaled1, 5, nstart = 100)
km55

p11 <- fviz_cluster(km22, geom = "point", data = Data.scaled1) + ggtitle("k = 2")
p22 <- fviz_cluster(km33, geom = "point",  data = Data.scaled1) + ggtitle("k = 3")
p33 <- fviz_cluster(km44, geom = "point",  data = Data.scaled1) + ggtitle("k = 4")
p44 <- fviz_cluster(km55, geom = "point",  data = Data.scaled1) + ggtitle("k = 5")
#Plotting clusters
library(gridExtra)
grid.arrange(p11, p22, p33, p44, nrow = 2)

#Elbow method
set.seed(123)
fviz_nbclust(Data.scaled1, kmeans, method = "wss")

#Silhoutte measure
fviz_nbclust(Data.scaled1, kmeans, method = "silhouette")
#From Silhoutte measure, 2 seems to be comparatively better with Silhoutte measure equal to be 0.36.

#K-means clustering based on both purchase behavior & basis of purchase
#Clubbing both the purchase behavior & basis of purchase data in NewData
NewData <- data.frame(Data.scaled, Data.scaled1)

#Performing k-means clustering
set.seed(7)
km222 <- kmeans(NewData, 2, nstart = 100)
km222
km333 <- kmeans(NewData, 3, nstart = 100)
km333
km444 <- kmeans(NewData, 4, nstart = 100)
km444
km555 <- kmeans(NewData, 5, nstart = 100)
km555

p111 <- fviz_cluster(km222, geom = "point", data = NewData) + ggtitle("k = 2")
p222 <- fviz_cluster(km333, geom = "point",  data = NewData) + ggtitle("k = 3")
p333 <- fviz_cluster(km444, geom = "point",  data = NewData) + ggtitle("k = 4")
p444 <- fviz_cluster(km555, geom = "point",  data = NewData) + ggtitle("k = 5")
#Plotting different clusters
library(gridExtra)
grid.arrange(p111, p222, p333, p444, nrow = 2)


#Elbow method
set.seed(123)
fviz_nbclust(NewData, kmeans, method = "wss")

#Silhoutte measure
fviz_nbclust(NewData, kmeans, method = "silhouette")
#2 cluster size seems to be optimal

#Clustering including above variables & demographic information
#Converting the variables data types
DM$HS <- as.numeric(DM$HS)
DM$Affluence_Index <- as.numeric(DM$Affluence_Index)
DM[,c(2:7,9:10)] <- data.frame(apply(DM[2:7,9:10], 2, as.factor))
str(DM)
# Identify numerical variables
DataNum2 <- DM[, c(8,11)]
mins2 <- apply(DataNum2, 2, min)
maxs2 <- apply(DataNum2, 2, max)

Data.scaled2 <- scale(DataNum2, center = mins2, scale = maxs2 - mins2)
summary(Data.scaled2)
View(Data.scaled2)

 #Looking into categorical variables
fac <- sapply(DM, is.factor)
fac

ND <- data.frame(NewData, Data.scaled2,DM[,fac])
View(ND)
installed.packages('clustMixType')
library(clustMixType)

Kcn2 <- kproto(x=ND, k=2, nstrt=1, iter.max = 20)
b<-clprofiles(Kcn2, ND)
Kcn3 <- kproto(x=ND, k=3, nstrt=1, iter.max = 20)
c<-clprofiles(Kcn3, ND)
Kcn4 <- kproto(x=ND, k=4, nstrt=1, iter.max = 20)
d<-clprofiles(Kcn4, ND)
Kcn5 <- kproto(x=ND, k=5, nstrt=1, iter.max = 20)
e<-clprofiles(Kcn5, ND)

#Computing silhouette measure of each clustering
silhouette2 <- validation_kproto(method = "silhouette", object = Kcn2)
silhouette2
silhouette3 <- validation_kproto(method = "silhouette", object = Kcn3)
silhouette3
silhouette4 <- validation_kproto(method = "silhouette", object = Kcn4)
silhouette4
silhouette5 <- validation_kproto(method = "silhouette", object = Kcn5)
silhouette5
#Cluster 2 has the best silhouette measure of 0.4604392, thereby making it the optimal clusters to work with.

#Since silhouette measure of 0.4604392 for 2 clusters that include information about basis of purchase, purchase behavior & demographics is maximum among all the silhouette values,
#this segmentation seems to be the best.  
#Using final data to construct Decision tree
Final<- data.frame(ND,Kcn2$cluster)
View(Final)
Final$Kcn2.cluster <- as.factor(Final$Kcn2.cluster)

#Test TRain split
index <- sample(2, nrow(Final), replace = T, prob = c(0.75,0.25))
TrainData <- Final[index == 1, ]
TestData <- Final[index == 2, ]

#Plotting decision tree
library(party)

iris_ctree = ctree(Kcn2.cluster ~ ., data = TrainData)
iris_ctree

# To draw the ctree we can use plot function
plot(iris_ctree)

library(rpart)
iris_rpart <- rpart(Kcn2.cluster ~ ., data = TrainData, parms = list(split = "gini"),control = rpart.control(minsplit = 0, minbucket = 0, cp = -1))

# We can use the rpart.plot to plot the decision tree
library(rpart.plot)
rpart.plot(iris_rpart)

#Predict on test data
pred_Test_class <- predict(iris_rpart, newdata = TestData, type = "class")
mean(pred_Test_class != TestData$Kcn2.cluster)

