#' Purpose: Other sentiment libraries 
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: June 5, 2023


# Libraries
library(rvest)

# WD
setwd("~/Desktop/GSERM_ICPSR/personalFiles")

# Two data sets
transcriptNVDIA <- 'https://seekingalpha.com/article/4607199-nvidia-corp-nvda-q1-2024-earnings-call-transcript'
pgNvidia <- read_html(transcriptNVDIA) %>% 
  html_nodes(xpath = '/html/body/div[2]/div/div[1]/div/main/div/div[2]/div/article/div/div/div[1]/div/section[1]/div/div/div/div[3]/div/div/div[1]') %>%
  html_text()
#writeLines(pgNvidia, 'transcriptNVDIA.txt')

transcriptFoxConn <- 'https://seekingalpha.com/article/4348554-hon-hai-precision-industrys-hnhaf-management-on-q1-2020-results-earnings-call-transcript'
pgFox <- read_html(transcriptFoxConn) %>% 
  html_nodes(xpath = '/html/body/div[2]/div/div[1]/div/main/div/div[2]/div/article/div/div/div[1]/div/section[1]/div/div/div/div[3]/div/div/div[1]') %>%
  html_text()
#writeLines(pgFox, 'transcriptFox.txt')

# Prepare - tolower, stopwords

# Lexicons

# Measure polarity/sentiment

# Visuals

# End
