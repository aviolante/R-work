rm(list=ls())

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#### CLOUD 1 ####
# What is a Data Scientist
text <- readLines('/Users/anviol/Desktop/dataPersonaPlan/who_is_data_scientist.txt')

docs <- Corpus(VectorSource(text))

# Preview Doc
inspect(docs)

# Space COnversaion Function
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Remove Characters
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\(")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "\\.")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# term-document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Cloud
set.seed(1234)
par(bg=NA)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"))

# Word Association
findAssocs(dtm, terms = "problem", corlimit = 0.1)

# Word Frequency Plot
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#### CLOUD 2 ####
# Word Cloud and Stats from What a Data Scientist Wants
text <- readLines('/Users/anviol/Desktop/dataPersonaPlan/what_data_scientist_want.txt')

docs <- Corpus(VectorSource(text))

# Preview Doc
inspect(docs)

# Space COnversaion Function
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Remove Characters
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "\\(")
docs <- tm_map(docs, toSpace, ")")
docs <- tm_map(docs, toSpace, "\\.")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# term-document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Word Cloud
set.seed(1234)
par(bg=NA)
wordcloud(words = d$word, freq = d$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"))

# Word Association
findAssocs(dtm, terms = "tools", corlimit = 0.1)

# Word Frequency Plot
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")




