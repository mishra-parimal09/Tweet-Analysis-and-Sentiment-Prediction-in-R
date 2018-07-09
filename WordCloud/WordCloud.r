#importing the libraries

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

#importing the dataset

dataset<-read.csv("sentiment.csv")

#selecting the column of concern

required<-dataset[[11]]

#corpus

dataset.Corpus<-Corpus(VectorSource(required))

dataset.Clean<-tm_map(dataset.Corpus, PlainTextDocument)
dataset.Clean<-tm_map(dataset.Corpus,tolower)
dataset.Clean<-tm_map(dataset.Clean,removeNumbers)
dataset.Clean<-tm_map(dataset.Clean,removeWords,stopwords("english"))
dataset.Clean<-tm_map(dataset.Clean,removePunctuation)
dataset.Clean<-tm_map(dataset.Clean,stripWhitespace)
dataset.Clean<-tm_map(dataset.Clean,stemDocument)

#generating the WordCloud

wordcloud(words = dataset.Clean, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
wordcloud(dataset.Clean,max.words = 200,random.color = TRUE,random.order=FALSE)