#' Purpose: Learn some basic string manipulation functions
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# Get the data path
filePath <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/coffeeVector.csv'

# Libs
library(stringi)

# Options & Functions
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

# Get Data
text <- read.csv(filePath, header=TRUE)

# Logical T/F vector that a string appears at least ONCE
coffee    <- grepl("coffee", text$x, ignore.case=TRUE)
starbucks <- grepl("starbucks", text$x, ignore.case=TRUE)

# Review Logical Output
head(starbucks, 10)

# Find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$x, ignore.case=TRUE)

# Grep for indexing
text[grep('mug', text$x),]

# Logical T/F for one word OR another appears at least ONCE
keywordsOR  <-"mug|glass|cup"
mugGlassCup <- grepl(keywordsOR, text$x,ignore.case=TRUE)
head(text$x[mugGlassCup])

# Logical Search AND operator, regular expression
keywordsAND <- "(?=.*mug)(?=.*cute)"
cuteMug     <- grepl(keywordsAND, text$x,perl=TRUE)
head(text$x[cuteMug])

# Calculate the % of times among all tweets
sum(coffee) / nrow(text)
sum(starbucks) / nrow(text)
sum(mugGlassCup) / nrow(text)

# Count occurrences of words per tweet
text$x[654]
theCoffee     <- stri_count(text$x, fixed ="the")
theCoffeeGrep <- stri_count(text$x, regex ="\\bthe\\b") #anchored, nearly equivalent
identical(theCoffee, theCoffeeGrep)
theCoffee[654]
theCoffeeGrep[654]
text$x[654]
sum(theCoffee) / nrow(text)
sum(theCoffeeGrep) / nrow(text) #anchoring makes a difference!

# Suppose you want to make regular expression substitutions
originalCup <- text[grep("mug", text$x),]
originalCup[1:3]
gsub('mug', 'cup', originalCup[1:3]) # remember you may need to use anchors for the search pattern!!

# BE VERY CAREFUL! Sometimes Anchors matter!! Let's remove the RT (retweets)
exampleTxt <- 'RT I love the Statue of Liberty'
gsub('rt','', exampleTxt)
gsub('rt','', exampleTxt, ignore.case = T)
gsub('^RT','' ,exampleTxt) #another type of anchor
gsub('\\bRT\\b','' ,exampleTxt) # escaped "\b" is actually a "backspace" thus its only looking for that

# End