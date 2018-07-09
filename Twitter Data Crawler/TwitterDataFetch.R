library(httr)
library(twitteR)

#Using OAuth authentication handshake functions from the httr package for a twitteR session
setup_twitter_oauth('4T4HL518fn0AFR1giSLKisY5U', 'GCBxvj0W3b7NomDauIMfbjx6VFhr45IIjTnp0JrO6nQN5TymwK', '4567773974-roQtQdQwg5hRO6Th5sVfOlMg5YqDCa0oJD1yMZL', 'vsQLqzMqbGyDi0yvoiUqJPwhdjPk9r41bYO1Sf8yNWqnh')

#Using function searchTwitter to search the given hashtag with given data entries and saving them into a variable
tweets <- searchTwitter('#demonetization', n=50) 

## all elements of tweets will be collapsed into
## a data.frame and returned into tweet.df
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))

#Writing the tweets data frame into a new file and naming it as Minor13.csv
write.csv(tweets.df,file = "Minor13.csv",row.names=FALSE)
