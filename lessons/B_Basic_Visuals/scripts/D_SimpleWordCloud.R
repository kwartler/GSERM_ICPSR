#' Purpose: Build a simple word cloud with bi-grams
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 23, 2023
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/chardonnay.csv'

# Libs
library(tm)
library(qdap) # Comment out if you have qdap problems
library(wordcloud)
library(RColorBrewer)
library(ggwordcloud)

# Options & Functions
Sys.setlocale('LC_ALL','C')

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

# Create custom stop words
stops <- c(stopwords('english'), 'lol', 'amp', 'and', 'chardonnay')

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Data
text <- read.csv(filePath, header=TRUE)

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(text$text))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Make bi-gram TDM according to the tokenize control & convert it to matrix
wineTDM  <- TermDocumentMatrix(txtCorpus, 
                               control=list(tokenize=bigramTokens))
wineTDMm <- as.matrix(wineTDM)

# See a bi-gram
exampleTweet <- grep('wine country', rownames(wineTDMm))
wineTDMm[(exampleTweet-2):(exampleTweet),870:871]

# Get Row Sums & organize
wineTDMv <- sort(rowSums(wineTDMm), decreasing = TRUE)
wineDF   <- data.frame(word = names(wineTDMv), freq = wineTDMv, row.names = NULL)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
set.seed(1234)
wordcloud(wineDF$word,
          wineDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))

pdf('~/Desktop/GSERM_ICPSR/personalFiles/exampleWC.pdf')
wordcloud(wineDF$word,
          wineDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))
dev.off()

# More common ggplot interface, Single Color
ggplot(plotDF, aes(label = word, size = freq, color = 'red')) +
  geom_text_wordcloud() +
  theme_minimal() 


# More common ggplot interface, scale continuous colors
plotDF <- wineDF[1:100,]
ggplot(plotDF, aes(label = word, size = freq, color = freq)) +
  geom_text_wordcloud() +
  theme_minimal() +
  scale_color_gradient(low = "black", high = "red") #scale_color_gradient2(low = 'grey',high='black') will make it a spectrum

# End
