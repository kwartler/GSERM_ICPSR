#' Author: Ted Kwartler
#' Date: June 15, 2023
#' Purpose: Live Code:
#' 
#' Choices:
#' "winemag-data-130k-v2.csv"
#' "2020-12-18_dellForum_k1_k5540.fst"

# library
#library(fst)
library(MLmetrics)
library(text2vec)

# Data Path
filePth <- '/Users/edwardkwartler/Desktop/GSERM_ICPSR/lessons/B_Basic_Visuals/data/winemag-data-130k-v2.csv'

# Data In
df <- read.csv(filePth) #read_fst

# Subset to specific countries
keeps <- c('Spain','Portugal','Chile','Argentina')
keeps <- df[df$country %in% keeps,]


# WFM
# Assoc
# Dendro
## Sentiment
# DTM to Tidy
# Lexicon 1
# Join
# Lexicon 2
# Join

### Can the descriptions alone predict the points assigned? (could do price too?)

## Sample
set.seed(1234)
idx <- sample(1:____, .7*nrow(____))
train <- ____[idx,]
validation <- ____[-idx,]

## Explore -- no need [see above]

## Modify

# Clean

# DTM - Training: itoken, create vectorizer

# DTM - Validation: use the itoken, and original vectorizer

# Model Fit: Reminder, make sure no NA or missing in Y
fit <- cv.glmnet(x = ____, y =____, type.measure = "mae")

## Assess
# Examine the variable accuracy tradeoff
plot(____)

# Get some predictions
inSamplePredictions <- predict(____, ____)
ooSamplePredictions <- predict(____,_____) #needs original vectorizer

# MLMetrics - 
MLmetrics::MAE(y_pred = ____, y_true = ____) #inSample
MLmetrics::MAE(y_pred = ____, y_true = ____) #ooSample

# Extract coefficients
coeffBetaVals <- as.matrix(coefficients(____))
coeffBetaVals

# Make into a data frame for easy data manipulation
coeffBetaVals <- data.frame(terms    = rownames(____),
                            beta      = ____[,_], 
                            row.names = NULL)

# Drop non-zero coefficient
coeffBetaVals <- subset(____, ____[,_]___)

# Label into our groups pos/neg
coeffBetaVals$col <- ifelse(coeffBetaVals$beta>0,'____','____')
head(coeffBetaVals)

# Need to rearrange to mimic the TDM data shape
fakeTDM <- tidyr::pivot_wider(coeffBetaVals, 
                              names_from = col, 
                              values_from = beta, 
                              values_fill = 0)
fakeTDM
fakeTDM <- as.data.frame(fakeTDM)
fakeTDM

# Now make terms into a row attribute & drop column
row.names(fakeTDM) <- ____$____
fakeTDM$terms <- ____
fakeTDM

# Finally, plot the comparison.cloud.  Color is positive or negative relationship to Y, size is beta coefficient/impact to Y
comparison.cloud(____,
                 colors= c('red', 'blue'),
                 title.size=1,
                 scale=c(3,0.1))

# End