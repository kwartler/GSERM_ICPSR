#' Purpose: apply cosine similarity kmeans clustering
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: June 5, 2023

# Wd
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# Libs
library(skmeans)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(fst)

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
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Stopwords
stops  <- c(stopwords('SMART'), 'news')

# Read & Preprocess
txt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/exampleNews.csv')
txtVec <- paste(txt$title, 
                txt$description, 
                txt$content)
txt <- VCorpus(VectorSource(txtVec))
txt <- cleanCorpus(txt, stops)
txtDTM <- DocumentTermMatrix(txt, control = list(weighting = 'weightTfIdf'))
txtMat <- as.matrix(txtDTM)

# Remove any empty docs w/TF-Idf
txtMat <- subset(txtMat, rowSums(txtMat) > 0)

# Apply Spherical K-Means
txtSKMeans <- skmeans(txtMat, # data
                      4, #clusters
                      m = 1, #"fuzziness of cluster" 1 = hard partition, >1 increases "softness"
                      control = list(nruns = 5, verbose = T))
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')
dev.off()

# Silhouette plot
sk <- silhouette(txtSKMeans)
plot(sk, col=1:4, border=NA)
dev.off()

# ID protypical terms
protoTypical           <- t(cl_prototypes(txtSKMeans))
colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
protoTypical[99:101,]

# To improve the aesthetics save directly to disk
pdf(file = "~/Desktop/GSERM_ICPSR/personalFiles/exampleNews_cluster_topics.pdf", 
    width  = 6, 
    height = 6) 
comparison.cloud(protoTypical, title.size=1.1, scale=c(1,.5))
dev.off()
# End