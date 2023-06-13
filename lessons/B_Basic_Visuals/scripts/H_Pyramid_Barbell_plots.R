#' Purpose: Comparative visualizations for text
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 23, 2023
#'

# Data Input, locally you can use list.files()
british  <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/BritishAirways.csv'
ryan     <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/RyanAir.csv'
txtFiles <- c(british, ryan)

# Libs
library(tm)
library(qdap)
library(plotrix)
library(ggplot2)
library(ggthemes)

# Options & Functions
Sys.setlocale('LC_ALL','C')

# Lowercase
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Corpus processing
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words
stops <- c(stopwords('SMART'), 'amp', 'britishairways', 
           'british', 'flight', 'flights', 'airways', 
           'ryanair', 'airline', 'flying')

# Read in Data, collapse, clean & organize.  For advanced programming students, this could be done with a custom function
textA <- read.csv(txtFiles[1])
textA <- paste(textA$text, collapse = ' ')
textA <- VCorpus(VectorSource(textA))
textA <- TermDocumentMatrix(cleanCorpus(textA, stops))
textA <- as.matrix(textA)

# Repeat
textB <- read.csv(txtFiles[2])
textB <- paste(textB$text, collapse = ' ')
textB <- VCorpus(VectorSource(textB))
textB <- TermDocumentMatrix(cleanCorpus(textB, stops))
textB <- as.matrix(textB)

# Merge by the terms
df        <- merge(textA, textB, by ='row.names')
names(df) <- c('terms', 'britishAir', 'ryanAir')

# Examine the words in common
df[6:10,]

# Calculate the absolute differences among in common terms
df$diff <- abs(df$britishAir - df$ryanAir)

# Organize df for plotting
df<- df[order(df$diff, decreasing=TRUE), ]
top35 <- df[1:35, ]

# Pyarmid Plot
pyramid.plot(lx         = top35$britishAir, #left
             rx         = top35$ryanAir,    #right
             labels     = top35$terms,  #terms
             top.labels = c('britishAir', 'Terms', 'ryanAir'), #corpora
             gap        = 5, # space for terms to be read
             main       = 'Words in Common', # title
             unit       = 'wordFreq') 

# ggplot interface
# ggplot needs a defined "character class"
class(top35$terms) #inherited from "merge()"
top35$terms <- as.character(top35$terms)

p <- ggplot(top35, aes(x = terms)) +
  geom_bar(aes(y = britishAir, fill = "British Airways"), 
           stat = "identity", width = 0.8) +
  geom_bar(aes(y = -ryanAir, fill = "RyanAir"), 
           stat = "identity", width = 0.8) +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "")
p

# Change the aesthetics
p2 <- ggplot(top35, aes(x = terms)) +
  geom_bar(aes(y = britishAir, fill = "British Airways"), 
           stat = "identity", width = 0.4, alpha = 0.5) +
  geom_bar(aes(y = -ryanAir, fill = "RyanAir"), 
           stat = "identity", width = 0.4, alpha = 0.5) +
  geom_point(data = top35, aes(y = britishAir), color = 'blue', alpha = 0.75) +
  geom_point(data = top35, aes(y = -ryanAir), color = 'red', alpha = .75) +
  geom_text(aes(y = 0, label = terms), size = 3, nudge_y=0, color = 'black', fontface='bold') +
  coord_flip() +
  scale_fill_manual(values = c("blue", "red"), name = "") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "") +
  theme(axis.text.y=element_blank()) +
  ggtitle('Comparing BritishAir to RyanAir')
p2

# End
