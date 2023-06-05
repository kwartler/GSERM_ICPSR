#' Purpose: Apply NRC to get news source sentiment & cluster to get news topics
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 15, 2021

# wd
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# options
Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # this is unicode text

# Libs
library(skmeans)
library(tidytext)
library(tm)
library(clue)
library(cluster)
library(wordcloud)
library(lexicon)
library(dplyr)
library(plyr)
library(radarchart)
library(ggplot2)
library(ggthemes)

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

# Examine Raw Text
rawTxt <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/C_Sentiment_Unsupervised/data/exampleNews.csv')

# Examine the meta
t(rawTxt[1,])

# Organize into a DF for TM
allInfo <- data.frame(doc_id = 1:nrow(rawTxt),
                      text   = paste0(rawTxt$title, 
                                      rawTxt$description, 
                                      rawTxt$content),
                      source = rawTxt$id)

# Now the TM
stops  <- c(stopwords('SMART'),'chars') # API truncation "[+3394 chars]"

# Process
allInfo    <- VCorpus(DataframeSource(allInfo))
allInfo    <- cleanCorpus(allInfo, stops) 
allInfoDTM <-  DocumentTermMatrix(allInfo)
allInfoDTM <- as.matrix(allInfoDTM)
allInfoDTM <- subset(allInfoDTM, rowSums(allInfoDTM) > 0)
dim(allInfoDTM)

#### Perform a Spherical K Means Clustering
set.seed(1234)
txtSKMeans <- skmeans(allInfoDTM, 
                      4, 
                      m = 1, #1=hard partition
                      control = list(nruns = 5, verbose = T))

# Examine Separation
barplot(table(txtSKMeans$cluster), main = 'spherical k-means')
plot(silhouette(txtSKMeans), col=1:2, border=NULL)

# What are the terms of our 2 clusters
# ID protypical terms
protoTypical           <- t(cl_prototypes(txtSKMeans))
colnames(protoTypical) <- paste0('cluster_',1:ncol(protoTypical))
head(protoTypical)


# Make a comparison cloud of word clusters
pdf(file = "~/Desktop/GSERM_ICPSR/personalFiles/news_cluster_topics.pdf", 
    width  = 6, 
    height = 6) 
comparison.cloud(protoTypical, title.size=1.1, scale=c(1,.5))
dev.off()

#### Perform an NRC Sentiment Inner Join
tidyCorp <- tidy(DocumentTermMatrix(allInfo))
tidyCorp

# Let's understand the meta data of new source
sourceID <- unique(meta(allInfo))
sourceID

# Cut documents into the 5 sources
seq(0,500,100) 
tidyCorp <- as.data.frame(tidyCorp)
tidyCorp$source <- cut(as.numeric(tidyCorp$document), 
                       breaks = seq(0,500,100), 
                       labels = sourceID[,1])
head(tidyCorp[grep('msnbc',tidyCorp$source)-2,])

# Previously we reshaped the NRC now we just load it
nrc <- read.csv('tidy_nrcLex.csv')

# Perform the inner join
nrcSent <- inner_join(tidyCorp,
                      nrc, by=c('term' = 'term'), relationship = "many-to-many")
head(nrcSent)

# Adjust for quick analysis
table(nrcSent$sentiment, nrcSent$source)
emos <- as.data.frame.matrix(table(nrcSent$sentiment,nrcSent$source))
emos

# as a proportion of the channel's effort
prop.table(as.matrix(emos), margin = 2)

# Make a radarChart; more disgust in brietbart, more anger in washignton post
chartJSRadar(scores = as.data.frame(prop.table(as.matrix(emos), margin = 2)), 
             labs = rownames(emos),
             labelSize = 10, showLegend = F)
             
# Intersect the Clusters and Sentiment; ID outlier sources
clusterProp <- table(data.frame(txtSKMeans$cluster,
                       clusterSource = cut(1:length(txtSKMeans$cluster), 
                       breaks = seq(0,500,100), 
                       labels = sourceID[,1])))
clusterProp <- prop.table(clusterProp, margin = 1)

# Cluster is row, proportion of the cluster from the source (row wise)
clusterProp

# Intersect the Clusters and Sentiment; join the clusters
docCluster <- data.frame(document = names(txtSKMeans$cluster), 
                clusterAssignment = txtSKMeans$cluster)
head(docCluster)
head(nrcSent)
combinedData <- left_join(nrcSent, 
                          docCluster, 
                          by = c("document" = "document"))

# Bring it all together, doc 1 has x sentiment and was assigned to cluster one, and is from the washignton post
head(combinedData)

# Intersect the Clusters and Sentiment; subset to the cluster of interest
oneTopic <- subset(combinedData, combinedData$clusterAssignment == 1)

# Adjust for quick analysis
table(oneTopic$sentiment, oneTopic$source)
oneEmo <- as.data.frame.matrix(table(oneTopic$sentiment, oneTopic$source))
oneEmo

# Make a radarChart, in cluster 1, the majority of sadness words came from brietbart news
chartJSRadar(scores = oneEmo, 
             labs = rownames(oneEmo),
             labelSize = 10, showLegend = F)
             
# Intersect the Clusters and Sentiment; subset to one source
# Revisit the combined data
head(combinedData)

# Subset to a single source
oneSource <- subset(combinedData, combinedData$source== 'the-washington-post') 

# Sum count the words by sentiment emotion and cluster assignment
oneSource <- aggregate(count~sentiment+clusterAssignment, oneSource, sum)
oneSource

# Recode the topics from 1-4 to the most frequent words in each cluster like "trump" etc
levelKey <- rownames(protoTypical)[apply(protoTypical,2,which.max)] # Get the max words in each cluster
names(levelKey) <- c("1","2","3","4") # Assign the corresponding 1-4; just done in order 
levelKey

# Now apply the recode; !!! "unquotes" the inputs and uses the raw values so "1" or "4" are passed directly as 1, 4 etc.
oneSource$clusterAssignment <- recode(as.character(oneSource$clusterAssignment), !!!levelKey)

# Examine the work
head(oneSource)

# Now plot a single source, x-axis is the emotion, y-axis is the top term for each cluster, dot size and alpha is emotional term frequency
ggplot(oneSource, 
       aes(sentiment, as.factor(clusterAssignment), 
                      size = count, alpha = count)) +
  geom_point() +
  ggtitle(singleSourceID, 
          sub = "Emotion by Topic Cluster") + 
  ylab("") +
  theme_tufte()
# End