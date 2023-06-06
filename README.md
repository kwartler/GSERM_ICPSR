# GSERM_ICPSR

## Install packages

```
# Easiest Method to run in your console
install.packages('pacman')
pacman::p_load(caret,cld2,clue,cluster,dplyr,dygraphs,echarts4r,fairness,ggplot2,ggthemes,ggwordcloud,glmnet,googleLanguageR,httr,hunspell,
jsonlite,kmed,lda,LDAvis,leaflet,lexicon,lsa,lubridate,mapproj,maps,mgsub,MLmetrics,ModelMetrics,openNLP,plotrix,plyr,pROC,
qdap,radarchart,RColorBrewer,reshape2,rvest,SentimentAnalysis,sentimentr,skmeans,spelling,stringi,stringr,
tesseract,text2vec,textcat,textdata,tidyr,tidytext,tm,treemap,udpipe,vtreat,wordcloud,wordcloud2,xts,yardstick,zoo)

# You can install packages individually such as below if pacman fails.
install.packages('tm')

# Or using base functions use a nested `c()`
install.packages(c("lda", "LDAvis", "treemap"))

```

```
# There is one additional package we will install on day 1 from a local .tar.gz file.
# Try this code with an updated path to your own copy of the file
install.packages("~/Desktop/GSERM_ICPSR/openNLPmodels.en_1.5-1.tar.gz", repos = NULL, type = "source")

# Or you can try the original datacube for it, though it often fails.
install.packages('openNLPmodels.en', repo= 'http://datacube.wu.ac.at/')
```