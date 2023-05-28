#' Purpose: Build an elastic net for classification 
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

### SAMPLE : Partitioning
idx              <- createDataPartition(diabetes$readmitted,p=.7,list=F)
trainDiabetesTxt <- diabetes[idx,]
testDiabetesTxt  <- diabetes[-idx,]

### EXPLORE
head(trainDiabetesTxt$diagnosisText,2)

table(trainDiabetesTxt$readmitted)

### MODIFY
# 
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


# Examine
head(coefficients(textFit),10)
plot(textFit)  # high enough penalty the AUC (area under the curve, similar to accuracy) plummets

# Subset to terms with coefficients assigned
bestTerms <- as.data.frame(subset(as.matrix(coefficients(textFit)), 
                    as.matrix(coefficients(textFit)) !=0))
bestTerms$terms <- rownames(bestTerms)

# Review the penalty impact
nrow(bestTerms)
ncol(diabetesDTM)

# What are some impacting terms
head(bestTerms[order(bestTerms$s1, decreasing = T),])
head(bestTerms[order(bestTerms$s1, decreasing = F),])

# Make training predictions
trainingPreds <- predict(textFit, diabetesDTM, type = 'class')
confusionMatrix(as.factor(trainingPreds),
                as.factor(trainDiabetesTxt$readmitted))

## Check the pptx for evaluating new data

### Apply to new patients requires the construction of the new patient DTM exactly as the training set
testPatients   <- itoken(testDiabetesTxt$diagnosisText, 
                   tokenizer = word_tokenizer)

# Use the same vectorizer but with new iterator (vocabulary from the new unseen rows)
testDTM <-create_dtm(testPatients,vectorizer)
dim(testDTM)

# Score the unseen data 
testPreds   <- predict(textFit,
                       testDTM,
                       type = 'class',
                       s    = textFit$lambda.min) #cv$lambda.1se;lambda.min

# Confusion Matrix
confMatTest <- table(testPreds, testDiabetesTxt$readmitted)
confMatTest

# Accuracy
sum(diag(confMatTest))/sum(confMatTest)

# End
