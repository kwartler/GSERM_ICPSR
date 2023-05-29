#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# File path input
filePath <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/coffeeVector.csv'

# Libs
library(tm)

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
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'smh')

# Data
text <- read.csv(filePath)

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$x))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Compare a single tweet
text$x[4]
content(txtCorpus[[4]])

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)
txtTdmM <- as.matrix(txtTdm)

# Examine
txtDtmM[1:4,grep('cute|mug|^coffee$', colnames(txtDtmM))]
txtTdmM[grep('cute|mug|^coffee$', colnames(txtDtmM)),1:4]

# Get the most frequent terms
topTermsA <- colSums(txtDtmM)
topTermsB <- rowSums(txtTdmM)

# Add the terms
topTermsA <- data.frame(terms = colnames(txtDtmM), freq = topTermsA, row.names = NULL)
topTermsB <- data.frame(terms = rownames(txtTdmM), freq = topTermsB, row.names = NULL)

# Review
head(topTermsA)
head(topTermsB)

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]
head(exampleReOrder)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# End