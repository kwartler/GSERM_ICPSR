#' Purpose: Inner join sentiment lexicons to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 15, 2021
#'

# Wd
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# Libs
library(tm)
library(lexicon)
library(tidytext)
library(dplyr)
library(qdap)
library(radarchart)
library(tidyr)

# Bring in our supporting functions
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'))

# Clean and Organize
txt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/Weeknd.csv')
# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(txt$text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make a DTM & convert for simplicity
txtDTM <- DocumentTermMatrix(txtCorpus)

# Examine original & Compare
as.matrix(txtDTM)[,1:10]
dim(as.matrix(txtDTM))

# Examine Tidy & Compare
tidyCorp <- tidy(txtDTM)
tidyCorp
dim(tidyCorp)

# Get bing lexicon
# "afinn", "bing", "nrc", "loughran"
bing <- get_sentiments(lexicon = c("bing"))
head(bing)

# Perform Inner Join
bingSent <- inner_join(tidyCorp, bing, by=c('term' = 'word'))
bingSent

# Quick Analysis
aggregate(count~sentiment,bingSent, sum)

# Compare original with qdap::Polarity
polarity(txt$text)
# avg. polarity  -0.409

# Get afinn lexicon
afinn<-get_sentiments(lexicon = c("afinn"))
head(afinn)

# Perform Inner Join
afinnSent <- inner_join(tidyCorp,afinn, by=c('term' = 'word'))
afinnSent

# Quick Analysis
weeknd <- txt$text
weekndWords <- data.frame(word = unlist(strsplit(weeknd,' ')))
weekndWords$word <- tolower(weekndWords$word )
weekndWords <- left_join(weekndWords,afinn, by=c('word' = 'word'))
weekndWords[is.na(weekndWords$value),2] <- 0
plot(weekndWords$value, type="l", main="Quick Timeline of Identified Words") 

# Get nrc lexicon; deprecated in tidytext, use library(lexicon)
nrc <- nrc_emotions
head(nrc)

# Pivot the data for joining
nrcLex <- pivot_longer(nrc, c(-term))
nrcLex <- subset(nrcLex, nrcLex$value>0)
head(nrcLex)
nrcLex$value <- NULL

# Perform Inner Join
nrcSent <- inner_join(tidyCorp,nrcLex, by=c('term' = 'term'))
nrcSent

# Quick Analysis
table(nrcSent$name)
emos <- data.frame(table(nrcSent$name))
chartJSRadar(scores = emos, labelSize = 10, showLegend = F)

# End