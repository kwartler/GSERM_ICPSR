#' Purpose: use LSA to reduce dimensions and create a model
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: May 28, 2023

# Data Folder
folderPaths <- list.dirs(path = '~/Desktop/GSERM_ICPSR/lessons/D_Supervised/data/AutoAndElectronics')
savePath <- '~/Desktop/GSERM_ICPSR/lessons/D_Supervised/data/'

# Libs
library(tm)
library(lsa)
library(yardstick)
library(ggplot2)

# Custom helper functions
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


# Options & Functions
options(stringsAsFactors = FALSE, scipen = 999)
Sys.setlocale('LC_ALL','C')

# Create custom stop words
stops <- c(stopwords('SMART'), 'car', 'electronic')

# Bring in some data
carCorp <- VCorpus(DirSource(folderPaths[2]))
electronicCorp <- VCorpus(DirSource(folderPaths[3]))

# Clean each one
carCorp        <- cleanCorpus(carCorp, stops)
electronicCorp <- cleanCorpus(electronicCorp, stops)

# Combine the corpora
allPosts <-  c(carCorp,electronicCorp)

# Remove objects we don't need cluttering up the RAM (for students with small computers)
rm(carCorp)
rm(electronicCorp)
gc()

# Construct the Target
yTarget <- c(rep(1,1000), rep(0,1000)) #1= about cars, 0 = electronics

# Make TDM; lsa docs save DTM w/"documents in columns, terms in rows and occurrence frequencies in the cells."!
allTDM <- TermDocumentMatrix(allPosts, 
                             control = list(weighting = weightTfIdf))
allTDM

# Get 20 latent topics
##### Takes awhile, may crash small RAM computers, so saved a copy
#lsaTDM <- lsa(allTDM, 20)
#saveRDS(lsaTDM, paste0(savePath, Sys.Date(),'_lsaTDM_tfidf_.rds'))#be sure to declare the right wd!
lsaTDM <- readRDS(paste0(savePath, '2023-05-28_lsaTDM_tfidf_.rds'))

### If you have new data/docs and have a model built using the original lsa() you need to treat the new values like this:
# Transform new, unseen data into the lower-dimensional LSA space
# testDTM <- create.matrix(c("This is a bird for document 1.", "I am an alien for document 2."), type = "content")
# test_lsa <- testDTM %*% lsaTDM$tk
###


# Extract the document LSA values
docVectors <- as.data.frame(lsaTDM$dk)
head(docVectors)

# Append the target var
docVectors$yTarget <- yTarget

# Sample (avoid overfitting)
set.seed(1234)
idx <- sample(1:nrow(docVectors),.6*nrow(docVectors))
training   <- docVectors[idx,]
validation <- docVectors[-idx,]

# Fit the model
fit <- glm(yTarget~., training, family = 'binomial')

# Predict in sample
predTraining <- predict(fit, training, type = 'response')
head(predTraining)

# Predict on validation
predValidation <- predict(fit, validation, type = 'response')
head(predValidation)

# Simple Accuracy Eval
yHat <- ifelse(predValidation >= 0.5,1,0)
(confMat <- table(yHat, validation$yTarget))
autoplot(conf_mat(confMat))

# End
