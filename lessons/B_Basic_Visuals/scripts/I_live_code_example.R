# Author: TK
# date: May 28
# purpose: live code a text mining project

# Choices
# What are people complaining about?
# '2020-12-18_dellForum_k1_k5540.fst' #requires the FST package to load
# What are people talking about at a moment in time when mentioning beer? 
# "beer.csv"  
# Any differences among carrefour and tesco mentions?
# Compare: "carrefour.csv" "tesco.csv"
# What words are used to describe 130k wines, can be done by country, region, points or price
# *large-ish* data: "winemag-data-130k-v2.csv" 

# Data Path

# Libs

# Options & Functions
Sys.setlocale('LC_ALL','C')

# Lowercase
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Corpus processing
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Stops


# Corpus Clean DTM

# Association?

# Dendrogram?

# Simple word cloud?

# Comparison cloud?

# End