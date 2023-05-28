#' Purpose: Mix data types to improve a model
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# Data Path
filePath <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/D_Supervised/data/diabetes_subset_8500.csv'

# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(pROC)


# Custom cleaning function
diagnosisClean<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}

# Read
diabetes <- read.csv(filePath)

# Concantenate texts in 3 columns
diabetes$diagnosisText <- as.character(paste(diabetes$diag_1_desc,
                                         diabetes$diag_2_desc,
                                         diabetes$diag_3_desc, sep=' '))

### SAMPLE : Patritioning
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F)
trainDiabetesTxt <- diabetes[idx,]
testDiabetesTxt  <- diabetes[-idx,]

### EXPLORE
head(trainDiabetesTxt$diagnosisText,2)

table(trainDiabetesTxt$readmitted)

### MODIFY
trainDiabetesTxt$diagnosisText <- diagnosisClean(trainDiabetesTxt$diagnosisText)

# Initial iterator to make vocabulary
iterMaker <- itoken(trainDiabetesTxt$diagnosisText, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords('SMART'))
head(textVocab)
tail(textVocab)
nrow(textVocab)

#prune vocab to make DTM smaller
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM 
diabetesDTM <- create_dtm(iterMaker, vectorizer)
dim(diabetesDTM)

# Default is TF but if you want TF-IDF
#idf         <- get_idf(diabetesDTM)
#diabetesDTM <- transform_tfidf(diabetesDTM,idf)

### MODEL(s)
#train text only model
textFit <- cv.glmnet(diabetesDTM,
                     y=as.factor(trainDiabetesTxt$readmitted),
                    alpha=0.9,
                    family='binomial',
                    type.measure='auc',
                    nfolds=5,
                    intercept=F)


# Examine - lots of terms thrown out!
head(coefficients(textFit),10)

# Fit without text
noText    <- as.matrix(trainDiabetesTxt[,1:132])
noTextFit <- cv.glmnet(noText,
                       y=as.factor(trainDiabetesTxt$readmitted),
                       alpha=0.9,
                       family='binomial',
                       type.measure='auc', 
                       nfolds=5, 
                       intercept=F)

# Fit with text and the other data
allFit <- cv.glmnet(cbind(diabetesDTM, noText),
                    y=as.factor(trainDiabetesTxt$readmitted),
                    alpha=0.9,
                    family='binomial',
                    type.measure='auc',
                    nfolds=5,
                    intercept=F)
### ANALYZE
plot(textFit)
title("GLMNET Only Text")

plot(noTextFit)
title("GLMNET No Text")

plot(allFit)
title("GLMNET All Info")

# Let's get training predictions for each model
textPreds   <-as.logical(predict(textFit,
                                 diabetesDTM,
                                 type = 'class',
                                 s    = textFit$lambda.min))
noTextPreds <- as.logical(predict(noTextFit,
                                  as.matrix(trainDiabetesTxt[,1:132]),
                                  type = 'class',
                                  s    = noTextFit$lambda.min))


allPreds    <- as.logical(predict(allFit,cbind(diabetesDTM,
                                               as.matrix(trainDiabetesTxt[,1:132])),
                                  type = 'class',
                                  s    = allFit$lambda.min))

MLmetrics::Accuracy(textPreds,trainDiabetesTxt$readmitted*1 )
MLmetrics::Accuracy(noTextPreds,trainDiabetesTxt$readmitted*1)
MLmetrics::Accuracy(allPreds, trainDiabetesTxt$readmitted*1)

### Go to PPTX

#compare AUC
textROC     <- roc((trainDiabetesTxt$readmitted*1), textPreds*1)
noTextROC   <- roc((trainDiabetesTxt$readmitted*1), noTextPreds*1)
allROC      <- roc((trainDiabetesTxt$readmitted*1), allPreds*1)

plot(textROC,col="blue",main="BLUE = Text, RED = No Text, GREEN=All",adj=0)
plot(noTextROC, add=TRUE,col="red", lty=2)
plot(allROC,add=TRUE,col="darkgreen", lty=3)

### Apply to new patients requires the construction of the new patient DTM exactly as the training set
testIT   <- itoken(testDiabetesTxt$diagnosisText, 
                   tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator
testDTM <-create_dtm(testIT,vectorizer)
# not needed here but you can xfer a tfidf too: testDTMtfidf <- transform_tfidf(testDTM, idf)

# Append the DTM to the test patient data
newPatients <- cbind(testDTM, as.matrix(testDiabetesTxt[,1:132]))

testPreds   <- predict(allFit,
                       newPatients,
                       type = 'class',
                       s    = allFit$lambda.min) #cv$lambda.1se;lambda.min

# Confusion Matrix
confMat <- table(testPreds, testDiabetesTxt$readmitted)
confMat
# Accuracy
sum(diag(confMat))/sum(confMat)

#End
