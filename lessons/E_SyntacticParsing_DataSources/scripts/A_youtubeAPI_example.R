#' Purpose: Demonstrate f12 in Chrome for API
#' Author: Ted Kwartler
#' Date: May 29, 2023
#'

# Libraries
library(jsonlite)
library(stringr)
library(plyr)

# Options; google api returns UTF-8 text
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

# Youtube URL
# https://www.youtube.com/watch?v=K5Rly83zfuI&ab_channel=TheDailyShowwithTrevorNoah
youtubeCaption <- "https://www.youtube.com/api/timedtext?v=K5Rly83zfuI&caps=asr&opi=112496729&xoaf=4&hl=en&ip=0.0.0.0&ipbits=0&expire=1685395487&sparams=ip%2Cipbits%2Cexpire%2Cv%2Ccaps%2Copi%2Cxoaf&signature=D7134F2733D5B94B64D9126E987B9F8AD70BE054.89EE5161AB6532100151EEFDE435760B80FBD2C3&key=yt8&lang=en-US&fmt=json3&xorb=2&xobt=3&xovt=3&cbrand=apple&cbr=Chrome&cbrver=112.0.0.0&c=WEB&cver=2.20230525.00.00&cplayer=UNIPLAYER&cos=Macintosh&cosver=10_15_7&cplatform=DESKTOP"

# Backup file
#youtubeCaption <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/E_SyntacticParsing_DataSources/data/f.json'

# Go get the data
dat <- fromJSON(youtubeCaption) # you can even pass in a URL to go to a webpage

# closed captioning data
dat$events$tStartMs
dat$events$dDurationMs
dat$events$segs[1:10]

# Get each first column called utf8
rawTxt <- lapply(dat$events$segs, "[", 'utf8') 

# organize just the single column
rawTxt <- do.call(rbind, rawTxt)

# Drop line returns "\n"
rawTxt <- gsub('[\r\n]',' ',rawTxt[,1])

# Sometimes there are entries that are empty so they need to be dropped
head(rawTxt,10)
rawTxt <- rawTxt[nchar(rawTxt) != "0"]

# Sometimes, there is extra spacing from the gsub
rawTxt <- str_squish(rawTxt)

# If you want it as a single chunk
oneChunk <- paste(rawTxt, collapse = ' ')
oneChunk

# If you want to retain the meta data
textDF <- data.frame(startTime = dat$events$tStartMs/1000,
                     duration  = dat$events$dDurationMs/1000,
                     text = unlist(lapply(dat$events$segs, "[", 'utf8') ))

# Examine to make sure format is ok
head(textDF, 10)

# End
