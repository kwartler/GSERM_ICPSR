#' Purpose: Why doesn't LM work for binary classification?
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28 2023
#'

# libs
library(ggplot2)
library(dplyr)

# Data
diamonds <- read.csv('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/D_Supervised/data/diamonds2023.csv')

# Convert to binary
diamonds$icedOut <- ifelse(diamonds$priceClean >= 11000,1, 0)
diamonds$price   <- NULL

set.seed(1234)
sampDiamonds <- sample_n(diamonds, 10000)

# Remember this?
p <- ggplot(sampDiamonds, aes(Carat, icedOut)) + 
  geom_point(alpha = 0.2)
p

# Since we see a relationship let's make a linear model to predict prices
fit   <- lm(icedOut ~ Carat + 0, sampDiamonds)
xBeta <- coefficients(fit)

# Add out model predictions; does this look like a good fit?
p <- p + geom_abline(intercept =  0, slope = xBeta, color='red')
p

# Suppose you *could* get a 12 carat diamond
hopeDiamond  <- data.frame(Carat = 10)
gettingCrunk <- predict(fit, hopeDiamond)

# 1= Yes, the diamonds is more than $11k; 0 means no.  What does this mean?  We must have used the wrong method!
gettingCrunk

# End