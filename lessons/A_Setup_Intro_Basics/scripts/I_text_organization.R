#' Purpose: Learn some basic cleaning functions & term frequency
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: May 28, 2023
#'

# Libs
library(tm)

# Get the data path
filePath <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/coffeeVector.csv'

# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
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
stops <- c(stopwords('english'), 'lol', 'smh', 'GSERM')

# Data
text <- read.csv(filePath)
head(text)

# Make a volatile corpus
# DataframeSource captures meta data (other columns like author and date) but requires doc_id as the 1st column
txtCorpus <- VCorpus(VectorSource(text$x))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# When you have a lot of text cleaning takes a long time.
# You can extract and save the cleaned text with content()
# This method lets you column bind the meta columns if you need too
df <- data.frame(text = unlist(sapply(txtCorpus, `[`, "content")),
                 stringsAsFactors=F)

# Sapply will let you get the clean vector only for saving if you have no meta information
cleanVector <- sapply(txtCorpus, content)

# Compare a single tweet
text$x[4]
df[4,]
cleanVector[4]

## YOu only need to make one of these since they are the same data!
# Make a Document Term Matrix 
txtDtm  <- DocumentTermMatrix(txtCorpus)
txtDtmM <- as.matrix(txtDtm)

# Make a Term Document Matrix
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtTdmM <- as.matrix(txtTdm)

# If you have a lot of data you may not want to make them inefficient "simple matrices" with as.matrix()
# DocumentTermMatrix & TermDocumentMatrix are "simple_triplet_matrix" objects so you can work with 
# large data using library(slam) functions

# Examine
# Find the mug
idx <- grep('mug', colnames(txtDtmM))
txtDtmM[1:4,idx]
txtTdmM[idx,1:4]


#### Go back to PPT ####

# Get the most frequent terms
topTermsA <- colSums(txtDtmM)

# Add the terms
topTermsA <- data.frame(terms = colnames(txtDtmM), freq = topTermsA, row.names = NULL)

# Review
head(topTermsA)
head(topTermsB)
topTermsB <- data.frame(terms = rownames(txtTdmM), freq = topTermsB, row.names = NULL)
topTermsB <- rowSums(txtTdmM)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# End
