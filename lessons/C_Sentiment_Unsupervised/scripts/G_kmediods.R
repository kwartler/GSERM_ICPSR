#' Purpose: apply k Mediod clustering to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 5, 2023
#' https://cran.r-project.org/web/packages/kmed/vignettes/kmedoid.html

# Wd
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# Libs
library(kmed)
library(tm)
library(clue)
library(cluster)

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

# Stopwords
stops  <- c(stopwords('SMART'), 'work')

# Read
txt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/basicResumes.csv')
# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(txt$text))

# Clean & Organize
txtMat <- cleanCorpus(txtCorpus, stops)
txtMat <- DocumentTermMatrix(txtMat, control = list(weighting = 'weightTfIdf'))
txtMat <- as.matrix(txtMat)

# Remove empty docs w/TF-Idf, or very short docs made of stop words like tweets 
txtMat <- subset(txtMat, rowSums(txtMat) > 0)

# Scale
txtMat <- scale(txtMat)

# Use a manhattan distance matrix; default for kmed
manhattanDist <- distNumeric(txtMat, txtMat, method = "mrw")

# Calculate the k-mediod
txtKMeds <- pam(manhattanDist, k = 2, mediods = T)

# Number of docs per cluster
table(txtKMeds$cluster)
barplot(table(txtKMeds$cluster), main = 'k-mediod')

# Silhouette, looks like one is very well defined and the other two have overlap.  Maybe try cluster 2?
silPlot          <- silhouette(txtKMeds$cluster, manhattanDist)
plot(silPlot, col=1:max(txtKMeds$cluster), border=NA)

# Median centroid documents:
names(txtKMeds$medoid[,1])

# End