#' Purpose: Given two corpora find disjoint words and visualize
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# Data Input, locally you can use list.files()
chardonnay <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/chardonnay.csv'
coffee     <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/coffee.csv'
txtFiles <- c(chardonnay, coffee)

# Topic names
topicNames <- c('chardonnay','coffee')

# Options
options(scipen = 999)

# Libs
library(tm)
library(qdap)
library(wordcloud)
library(RColorBrewer)

# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay', 'coffee')

# Read in the files
for (i in 1:length(txtFiles)){
  assign(topicNames[i], read.csv(txtFiles[i]))
  cat(paste('read completed:',txtFiles[i],'\n'))
}


# Vector Corpus; omit the meta data
coffee       <- VCorpus(VectorSource(coffee$text))
chardonnay <- VCorpus(VectorSource(chardonnay$text))

# Clean up the data
coffee       <- cleanCorpus(coffee, stops)
chardonnay <- cleanCorpus(chardonnay, stops)

# Another way to extract the cleaned text 
coffee       <- unlist(lapply(coffee, content))
chardonnay <- unlist(lapply(chardonnay, content))

# Instead of 1000 individual documents, collapse each into a single "subject" ie a single document
coffee       <- paste(coffee, collapse = ' ')
chardonnay <- paste(chardonnay, collapse = ' ')

# Combine the subject documents into a corpus of *2* documents
allDrinks <- c(coffee, chardonnay)
allDrinks <- VCorpus((VectorSource(allDrinks)))

# Make TDM with a different control parameter
# Tokenization `control=list(tokenize=bigramTokens)`
# You can have more than 1 ie `control=list(tokenize=bigramTokens, weighting = weightTfIdf)`
ctrl      <- list(weighting = weightTfIdf)
drinkTDM  <- TermDocumentMatrix(allDrinks, control = ctrl)
drinkTDMm <- as.matrix(drinkTDM)

# Make sure order is the same as the c(objA, objB) on line ~80
colnames(drinkTDMm) <- c('coffee', 'chardonnay')

# Examine
head(drinkTDMm)

# Make comparison cloud
comparison.cloud(drinkTDMm, 
                 max.words=75, 
                 random.order=FALSE,
                 title.size=0.5,
                 colors=brewer.pal(ncol(drinkTDMm),"Dark2"),
                 scale=c(3,0.1))

# End