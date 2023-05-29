#' Purpose: Use textcat to ID which language is used
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# Libs
library(textcat)
library(cld2) #Google, compact language detector

# Options & Functions
testing <- F

# Data Path
filePath <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/E_SyntacticParsing_DataSources/data/tweets_Haddad_Fernando.csv'

# Data
if(testing == T){
  unknownLanguageOne <- read.csv(filePath, 
                                 nrows = 100)
} else {
  unknownLanguageOne <- read.csv(filePath)
  
}

# Example languages supported
t(t(names(TC_byte_profiles)))

# Categorize the language
txtLanguage <- textcat(unknownLanguageOne$text)

# Review; overall OK!
head(txtLanguage, 20)

# Problematic texts; perhaps cleaning or longer passages would help
unknownLanguageOne$text[3]
unknownLanguageOne$text[4]
unknownLanguageOne$text[9]

# Most frequent
barplot(table(txtLanguage), las = 2)

# Using google compact language detector; 80 languages
text <- c("To be or not to be?", "Ce n'est pas grave.", "Nou breekt mijn klomp!")
detect_language(text) #english, french, dutch

# It can check mixed documents too; and webpages
# returns top 3 identified
detect_language_mixed(
  url('http://www.un.org/fr/universal-declaration-human-rights/'), plain_text = FALSE)

# End