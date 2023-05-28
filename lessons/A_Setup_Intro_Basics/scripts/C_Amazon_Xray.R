#' Title: Amazon X-Ray part 2
#' Purpose: Explore more movie data
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 27, 2023
#'

### 1. Set the path to our ata
# URL to the file
# Lego
# 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/lego_onScreenCharacters.csv
# Force Awakens
# 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/forceAwakens_onScreenCharacters.csv'
# Mary Poppins
# 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/poppins_onScreenCharacters.csv'

movieURL <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/A_Setup_Intro_Basics/data/forceAwakens_onScreenCharacters.csv'

# Turn off scientific notation
options(scipen = 999)

### 2. Load libraries to customize R
library(ggplot2)
library(ggthemes)

### 3. Read in data
# Use the read.csv function for your specific onScreenCharacters.csv file
charDF   <- read.csv(movieURL)

### 4. Apply functions to clean up data & get insights/analysis
# Use the names function to review the names of charDF
names(charDF)

# Review the top 6 records of charDF
head(charDF)

# Remove a raw data column
charDF$changeType <- NULL

# Calculate a new vector sceneEnd - CharAppearance
charDF$charDuration <- charDF$sceneEnd - charDF$appearance

# Divide the appearance by 1000 to calculate seconds
charDF$appearanceSecs <- charDF$appearance / 1000

# Divide the sceneEnd by 1000 to calculate seconds
charDF$sceneEndSecs <- charDF$sceneEnd / 1000

# Total character appearances
nrow(charDF)

# Sometimes there are duplicated rows (not in lego movie)
# ie Star Wars BB-8 is duplicated because there are 2 puppeteers, lets remove any duplicate records
charDF$dupes <- duplicated(charDF) #T/F if it is duplicated
head(charDF)

# Show any rows that are TRUE duplicates; don't worry about grep...we cover it later
charDF[grep('TRUE', charDF$dupes),] 

# drop dupes 
nrow(charDF)
charDF <- subset(charDF, charDF$dupes != TRUE)
nrow(charDF)

### 5. Project artifacts ie visuals & (if applicable)modeling results/KPI
# Tally the number of scenes by character; like a pivot table in Excel
charTally <- as.matrix(table(charDF$character))
charTally

# Subset; like "filter" in Excel
charTally <- subset(charTally, charTally[,1]>2)
charTally

# Basic plot
barplot(t(charTally), las = 2)

# Timeline of character appearances
ggplot(charDF, aes(colour = character)) + 
  geom_segment(aes(x = appearanceSecs, xend = sceneEndSecs,
                   y = character, yend = character), linewidth = 3) +
  theme_gdocs() + theme(legend.position = "none")
#ggsave('~/Desktop/GSERM_ICPSR/personalFiles/somePlot.jpeg')

# Now see just top n characters
n <- 6
topPerformers <- sort(charTally[,1], decreasing = T)[1:n]
names(topPerformers)

topChars <- charDF[charDF$character %in% names(topPerformers),]
ggplot(topChars, aes(colour = character)) + 
  geom_segment(aes(x = appearanceSecs, xend = sceneEndSecs,
                   y = character, yend = character),size=3) +
  theme_gdocs() + theme(legend.position = "none")

# End
