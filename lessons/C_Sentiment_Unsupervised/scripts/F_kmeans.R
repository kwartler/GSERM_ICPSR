#' Purpose: apply k means clustering to text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 15, 2021

# Wd
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# Libs
library(tm)
library(clue)
library(cluster)
library(dplyr)

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
txtMat <- DocumentTermMatrix(txtMat)
txtMat <- as.matrix(txtMat)

# Scale the matrix
txtMat    <- scale(txtMat) #subtract mean  & divide by stDev
txtKMeans <- kmeans(txtMat, 3)
txtKMeans$size
barplot(txtKMeans$size, main = 'k-means')

# Looks like 1 cluster dominates, not surprising since this isn't a great method for text.
# silhouette measures a distance from where the observation sits in the cluster to the next nearest cluster 
dissimilarityMat <- dist(txtMat)
silPlot          <- silhouette(txtKMeans$cluster, dissimilarityMat)
head(silPlot, 15) #1=well matched, 0=similar to next cluster, -1=misclassified

# The sihouette plot needs to have k cluster "shadows" that are tall and distinct.
plot(silPlot, col=1:max(txtKMeans$cluster), border=NA)


#calculate indices of closest document to each centroid
idx <- vector()
for (i in 1:max(txtKMeans$cluster)){
  
  # Calculate the absolute distance between doc & cluster center
  absDist <- abs(txtMat[which(txtKMeans$cluster==i),] -  txtKMeans$centers[i,])
  
  # Check for single doc clusters
  if(is.null(nrow(absDist))==F){
    absDist <- rowSums(absDist)
    minDist <- subset(absDist, absDist==min(absDist))
  } else {
    minDist <- txtKMeans$cluster[txtKMeans$cluster==i]
  }
  idx[i] <- as.numeric(names(minDist))
}

# Notification of closest doc to centroid
cat(paste('cluster',1:max(txtKMeans$cluster),': centroid doc is ', idx,'\n'))

# End
