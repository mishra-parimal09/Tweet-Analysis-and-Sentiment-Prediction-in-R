#importing the libraries

library(caret)
library(rpart.plot)
library(knitr)
library(rpart)
library(rpart.plot)

#importing the dataset

dataset<-read.csv("Airline_Reviews.csv",stringsAsFactors = FALSE)

#viewing summary of the dataset

summary(dataset)
new <- new[, c("Response", 'ASC', 'Airline', 'Timezone')]

#verifying the existence of any null values

mapply(function(x) sum(is.na(x)), new)
mapply(function(x) sum(!is.na(x)), new)
new <- new[complete.cases(new),]

#setting up the sample size and seed 

smp_size <- floor(0.75 * nrow(new))
set.seed(2)

#creating sepatate CSV files for training and test set datasets

train_ind <- sample(seq_len(nrow(new)), size = smp_size)
train <- new[train_ind, ]
write.csv(train,"dtreetraining.csv",row.names = F)

test <- new[-train_ind, ]
write.csv(test,"dtreetesting.csv",row.names = F)

#importing the training and test set data

dtreeTrainingSet<-read.csv("dtreetraining.csv",stringsAsFactors = FALSE)
dtreeTrainingSet <- dtreeTrainingSet[, c("Response", 'ASC', 'Airline', 'Timezone')]
dtreeTrainingSet <- dtreeTrainingSet[complete.cases(dtreeTrainingSet),]

dtreeTestingSet <-read.csv("dtreetesting.csv",stringsAsFactors = FALSE)

#fitting the decision tree classifier

fit <- rpart(Response ~ ASC + Airline + Timezone , data = dtreeTrainingSet, method = "class")

#visualisiing the results

plot(fit)
text(fit)
set.seed(3)
rpart.plot(fit)

#predicted the values for the test set data

predicted= predict(fit,dtreeTestingSet)
predicted

