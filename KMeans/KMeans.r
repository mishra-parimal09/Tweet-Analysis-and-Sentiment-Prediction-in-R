#importing the dataset

dataset <- read.csv("sentiment.csv")

#selecting the columns of concern  from the dataset in another variable

preprocessed_data <- dataset %>%
  select(AS = airline_sentiment, ASC = airline_sentiment_confidence, NRC = negativereason_confidence)

#substituting null values with the mean values of the column

MeanNRC<-mean(preprocessed_data$NRC, na.rm=TRUE)
preprocessed_data$NRC[which(is.na(preprocessed_data$NRC))] <- MeanNRC

#creating a CSV file to store the columns of concern

write.csv(preprocessed_data,"preprocessed_dataset.csv",row.names = F)

#importing the refined dataset

KMeans<-read.csv("preprocessed_dataset.csv")
KMeans_2<-KMeans[-1]
KMeans_3<-as.data.frame(scale(KMeans_2))
KMeans_3

#finding the mean and standard deviation

sapply(KMeans_2,mean)
sapply(KMeans_2,sd)
sapply(KMeans_3,mean)
sapply(KMeans_3,sd)

#applying KMeans to the datasets

library(NbClust)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#plotting the graph with the clusters

wssplot(KMeans_3,nc=30,seed=1234)

#visualising the results

base_kmeans<-kmeans(KMeans_3,30)
base_kmeans
base_kmeans$centers
base_kmeans$size