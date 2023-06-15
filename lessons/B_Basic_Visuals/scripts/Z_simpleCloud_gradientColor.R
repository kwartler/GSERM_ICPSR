#' Author: Ted Kwartler
#' Date: June 15, 2023
#' Purpose: Example Gradient Cloud  

# Example gradient word cloud
library(dplyr)
library(wordcloud)
library(gplots)


data(crude) #Corpus
tdm    <- TermDocumentMatrix(crude)
tdmM   <- as.matrix(tdm)
wfm    <- rowSums(tdmM)
wfm <- data.frame(word      = names(wfm),
                  freq      = wfm, 
                  row.names = NULL)
wfm
wordcloud(wfm$word,wfm$freq)
dev.off()

# Define the deciles
wfm$decile <- ntile(wfm$freq, 10)
head(wfm)

# Make the gradient - 10 (last one is the darkest)
colGradient <- sapply(seq(from = 0.1, to = 1, by = .1), 
       function(x) adjustcolor(col2hex('red'), x))
# "#1F78B41A" "#1F78B433" "#1F78B44D" "#1F78B466" "#1F78B480"
# "#1F78B499" "#1F78B4B3" "#1F78B4CC" "#1F78B4E6" "#1F78B4FF"
colGradient <- data.frame(decile = 1:10, 
                          color     = colGradient)
colGradient
# Join the gradient by decile
wfm <- left_join(wfm, colGradient, by = 'decile')

wordcloud(wfm$word,wfm$freq, colors = wfm$color)
dev.off()

# End