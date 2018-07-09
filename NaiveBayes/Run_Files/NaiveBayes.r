#including the libraries

library(RTextTools)
library(e1071)
library(dplyr) 
library(tidytext) 

#importing the dataset

dataset<-read.csv("sentiment.csv",stringsAsFactors = F)

#selecting the columns of concern from the imported dataset

responses<-dataset[[2]]
tweet_text<-dataset[[11]]

#removing the special characters from the selected column

removeSpecialChars <- function(tweet_text) gsub("[^a-zA-Z0-9 ]", " ", tweet_text)
tweet_text <- sapply(tweet_text, removeSpecialChars)

#converting the text data into lower case

tweet_text<-sapply(tweet_text,tolower)

#substitution of several words

fix.contractions<-function(tweet_text)
{
  tweet_text <- gsub("won't", "will not", tweet_text)
  tweet_text <- gsub("can't", "can not", tweet_text)
  tweet_text <- gsub("n't", " not", tweet_text)
  tweet_text <- gsub("'ll", " will", tweet_text)
  tweet_text <- gsub("'re", " are", tweet_text)
  tweet_text <- gsub("'ve", " have", tweet_text)
  tweet_text <- gsub("'m", " am", tweet_text)
  tweet_text <- gsub("'d", " would", tweet_text)
  tweet_text <- gsub("'s", "", tweet_text)
  return(tweet_text)}

#updating the imported dataset with the changes made in the column selected  

dataset$text<-tweet_text

#selecting the columns of concern  from the dataset in another variable

datasetrefined <- dataset %>% 
  select(Response = airline_sentiment,tweets = text)
datasetrefined

#creating a CSV file to store the columns of concern

write.csv(datasetrefined,"preprocessed_dataset.csv",row.names = F)

#importing the refined dataset

application<-read.csv("preprocessed_dataset.csv")

#setting up the sample size

smp_size <- floor(0.75 * nrow(application))

#setting seed

set.seed(123)

#preparing the training set and storing the training set data in a separate CSV file

train_ind <- sample(seq_len(nrow(application)), size = smp_size)
train <- application[train_ind, ]
write.csv(train,"training_dataset.csv",row.names = F)

#preparing the test set and storing the test set data in a separate CSV file

test <- application[-train_ind, ]
write.csv(test,"testing_dataset.csv",row.names = F)

#creating the sparse matrix of the training set

mat_train= create_matrix(train$tweets, language="english", 
                      removeStopwords=FALSE, removeNumbers=FALSE, 
                      stemWords=FALSE) 
matrix_train = as.matrix(mat_train)

#fitting the NaiveBayes classifier to the training set data

classifier = naiveBayes(mat, as.factor(train$Response))

#creating the sparse matrix of the test set data

mat_test= create_matrix(test$Response, language="english", 
                      removeStopwords=TRUE, removeNumbers=FALSE, 
                      stemWords=FALSE) 
matrix_test = as.matrix(mat_test)

#predicting the results of the test set data

predicted = predict(classifier, mat1[1:10,])
predicted

#generating results in tabular form

predicted_table<-table(test[1:10,1], predicted)
predicted_table

#calculating the accuracy

predicted_accuracy<-recall_accuracy(test[1:10,1], predicted)
predicted_accuracy