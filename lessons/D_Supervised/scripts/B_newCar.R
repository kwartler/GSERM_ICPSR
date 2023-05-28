#' Author: Ted Kwartler
#' Date: May 28, 2023
#' Purpose: Toyota Corolla Regression example
#' 

# Libs
library(vtreat)
library(dplyr)
library(ModelMetrics)

# Options
options(scipen=999)

# Data
cars <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/D_Supervised/data/newCars.csv') 

# drop geo and text for this example
cars$state       <- NULL
cars$city        <- NULL
cars$allFeatures <- NULL
cars$monthlyPayment <- NULL # Target Leakage!  

# Partitioning 20% test set
splitPercent <- round(nrow(cars) %*% .8)

set.seed(2017)
idx      <- sample(1:nrow(cars), splitPercent)
trainSet <- cars[idx, ]
testSet  <- cars[-idx, ]

# EDA
summary(trainSet)

# Get the column names of our data frame
names(cars)

informativeFeatureNames <- names(cars)[5:24]
outcomeVariableName     <- names(cars)[25] # Or simply "listPrice"

# Preprocessing & Automated Engineering
# id & constant variable removal, dummy $Fuel_Type
dataPlan     <- designTreatmentsN(cars, 
                                  informativeFeatureNames, 
                                  outcomeVariableName)

treatedTrain <- prepare(dataPlan, trainSet)

# Since we are comfortable with the data, we can concisely write a lm equation that includes all variables using period instead of an equation
fit <- lm(listPrice ~ ., treatedTrain)
summary(fit)

#################
### Go to the ppt slides on Summary Output
#################

# Drop uninformative vars
# Primarily these are nearly univariate (single value)
# Hybrid is captured in the mileage stats so its multi-colinear! 

# First get the variable and p-values
pVals <- data.frame(varNames = names(na.omit(coef(fit))),
                    pValues = summary(fit)$coefficients[,4])

# Determine which variable names to keep 
keeps <- subset(pVals$varNames, pVals$pValues<0.1)

# Remove unwanted columns
treatedTrainParsimony <- treatedTrain[,names(treatedTrain) %in% keeps]

# Append the y-variable
treatedTrainParsimony$listPrice <- treatedTrain$listPrice

# Refit a model
fit2 <- lm(listPrice ~ ., treatedTrainParsimony)
summary(fit2)

#################
### Go to the ppt
#################

# Get Training Set Predictions
# Warning can be ignored but for those interested: 
# https://stackoverflow.com/questions/26558631/predict-lm-in-a-loop-warning-prediction-from-a-rank-deficient-fit-may-be-mis
trainingPreds <- predict(fit2, treatedTrainParsimony)

#Organize training set preds
trainingResults <-data.frame(actuals        = treatedTrainParsimony$listPrice,
                             predicted      = trainingPreds,
                             residualErrors = treatedTrainParsimony$listPrice-trainingPreds )
head(trainingResults)

# What is the RMSE? 
trainRMSE <- MLmetrics::RMSE(trainingResults$predicted, 
                              trainingResults$actuals)
trainRMSE


# Since we haven't looked at the test set, we *could* go back and adjust the model.
# Let's continue to the test set evaluation
treatedTest <- prepare(dataPlan, testSet)
testPreds   <- predict(fit2, treatedTest) 

#Organize training set preds
testResults <- data.frame(actuals   = testSet$listPrice,
                          predicted = testPreds)
head(testResults)

# KPI
testRMSE <- MLmetrics::RMSE(testResults$predicted, 
                             testResults$actuals)
testRMSE

# Side by Side
trainRMSE
testRMSE


# End 

