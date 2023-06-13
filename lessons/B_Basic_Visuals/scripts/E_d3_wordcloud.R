#' Purpose: Make a d3 wordcloud webpage
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#'

# Declare the data path
filePath  <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/B_Basic_Visuals/data/chardonnay.csv'

# Libs
library(tm)
library(qdap)
library(wordcloud2)
library(RColorBrewer)
library(echarts4r)
library(dplyr)

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
stops <- c(stopwords('english'), 'lol', 'amp', 'chardonnay')

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
wineDF   <- data.frame(word = names(wineTDMv), freq = wineTDMv)

# Regular dynamic WC, click the pop-out in the viewer
wordcloud2(data = wineDF[1:50,])
?wordcloud2

# Choose a color & drop light ones
pal <- brewer.pal(8, "Dark2")
wordcloud2(wineDF[1:50,], 
           color = pal, 
           backgroundColor = "lightgrey")

# Some built in shapes need to click "show in new window"
# 'circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', & 'star'
wordcloud2(wineDF[1:50,],
           shape = "cardioid",
           color = "blue",
           backgroundColor = "pink")

# Now let's use a more up to date package echarts4r
wineDF[1:50,] %>% 
  e_color_range(freq, color, colors = c("tomato", "goldenrod")) %>% # Append the column name color, with the colors hex codes
  e_charts() %>% 
  e_cloud(
    word = word, 
    freq = freq, 
    color = color, 
    shape = "circle",
    rotationRange = c(0, 0),
    sizeRange = c(15, 100)
  ) %>% 
  e_tooltip() %>% 
  e_title("Chardonnay")


# End