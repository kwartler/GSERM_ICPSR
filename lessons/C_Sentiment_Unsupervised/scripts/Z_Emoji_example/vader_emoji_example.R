# Author: TK
# date: June 13, 2023
# purpose: Learn about vader a new microblog heuristic based polarity port from python

# The academic paper has the following listed heuristics:
# Punctuation:particularly !, increases the magnitude of the intensity. "Class is boring!!!" > than "Class is boring"
# ALL-CAPS: increases intensity "Class is BORING" > "Class is boring."
# Valence Shifters: affect intensity "Class is super SUPER boring" > "Class is boring" >"Class is partially boring"
# Polarity conjunctions: "But" has two independent clauses and the latter is assumed to be dominant.  "Class is boring but it is manageable" has less intensity than "Class is boring" even though it is "mixed sentiment" [latter clause dominates]
# Negations: window is 3 previous terms, "Class is not boring" switches the negative intensity

# Formula for a normalized score:
# x = sum of valence words (see above) from the microblog lexicon
# alpha = 15 (default value from the paper)
# x / sqrt((x + 15))

# Libs
library(vader)
library(emoji) # Get the emoji lexicon or load one manually
library(mgsub)

# Examine what this brings in
data(emojis)
head(emojis)
nrow(emojis)

# Computer UTF and emojis:
emojis$emoji[2]

# 8 bits (4 * 2 character codes)
charToRaw(emojis$emoji[2]) #The four bytes f0, 9f, 98, and 83 correspond to the four "code units" of the UTF-8 encoding of the emoji.

# 16 bits (4 * 16 binary codes)
intToBits(charToRaw(emojis$emoji[2]))

# Convert it back
rawToChar(charToRaw(emojis$emoji[2]))

# Data for vader analysis; to preserve the emojis, its saved as RDS file type
unicorns <- readRDS('~/Desktop/GSERM_ICPSR/lessons/C_Sentiment_Unsupervised/scripts/Z_Emoji_example/unicorns.rds')

# Small sample
unicorns$text[c(720, 804)]

# Look at smile halo in the emoji lexicon
emojis[14,1:2]

# Option 1 just drop all non standard characters
# Pros: fast
# Cons: loses context/info
# gsub("[^\x01-\x7F]", "", unicorns$text[c(720, 804)])

# Instead mgsub them
st <- Sys.time()
subTxt <- mgsub::mgsub(unicorns$text[1:1000], 
                       emojis$emoji, 
                       paste0(' ', emojis$name,' '))
Sys.time() - st #10x longer for 1000 tweets than simply removing them.

# since that took >1.5min, let's save a copy for next time!
#tmp <- data.frame(docID = unicorns$status_id, 
#                  emojiSubText = subTxt)
#write.csv(tmp, '~/Desktop/GSERM_ICPSR/personalFiles/emojiUnicornSub.csv', row.names = F)

# Let's look at what we did
subTxt[c(720, 804)] 


###### LEARN ABOUT VADER
#### There is an issue in the vader package that needs to be address
get_vader(' this is boring') #4 words?!
get_vader('this is boring') #3 words
get_vader(' 😇 so much fun ') # 5 words
get_vader('😇 so much fun') # 4 words - emoji is now a space, and neutral
get_vader('😇😇 so much fun') # still 4 words, two emojis with no space are one word


# as a preprocesing step remove extra spaces & preceeding, post spaces & drop emojis.
smallTxtFix <- unicorns$text[c(720, 804)]

smallTxtFix <- gsub("[^\x01-\x7F]", "", smallTxtFix) #drop emojis
smallTxtFix <- trimws(gsub("\\s+", " ", smallTxtFix)) # remove extra spaces

# Apply vader
smallVader <- vader_df(smallTxtFix)
smallVader # results are awful and should be a list

# Let's write a function to fix this error and return a more concise object
fixVader <- function(textVec){
  require(dplyr)
  tmpTxt <- gsub("[^\x01-\x7F]", "", textVec) #drop emojis
  tmpTxt <- trimws(gsub("\\s+", " ", tmpTxt)) # remove extra spaces
  
  # Apply vader after fixing the spacing issue
  tmpVader <- vader_df(tmpTxt)
  
  # Make the response more organized
  vaderTermResults <- list()
  for(i in 1:nrow(tmpVader)){
    if(nchar(tmpVader[i,]$text)>0){
      oneTxt <- tmpVader[i,]$text
      oneTxt <- unlist(strsplit(oneTxt, '\\s+'))
      wordScores <- gsub('[{]|[}]','',tmpVader[i,]$word_scores)
      wordScores <- sapply(strsplit(wordScores, ', '), as.numeric)
      wordScores <- data.frame(token = oneTxt, vaderScore = wordScores)
      vaderTermResults[[i]] <- wordScores
    } else {
      wordScores <- data.frame(token = NA, vaderScore = NA)
      warning('one doc has NA for tokens, could be all emojis, non-unicode etc')
      vaderTermResults[[i]] <- wordScores
    }
    
  }
  
  finalResponse <- list()
  for(i in 1:nrow(tmpVader)){
    docLevelStats <- tmpVader[i,3:6]
    docLevelStats$order <- i
    docLevelStats <- docLevelStats %>% select(last_col(), everything())
    finalResponse[[i]] <- list(documentLevelVader = docLevelStats,
                               termLevelVader = vaderTermResults[[i]])
  }
  return(finalResponse)
}

# apply it where emojis are dropps
noEmojiVader <- fixVader(unicorns$text)

# Extract doc level attributes and organize
justVaderDocLevel <- lapply(noEmojiVader, '[',1)
justVaderDocLevel <- do.call(rbind, lapply(justVaderDocLevel, unlist))
justVaderDocLevel[c(720, 804),]

# You can also get specific doc results
noEmojiVader[[720]][2] # doc 720,second element are token score

# Now let's apply this to the emoji sub text from before and see the different results
emojiSubVader <- fixVader(subTxt)

# Same data structure so we will extract and review in the same
justVaderDocLevelwEmoji <- lapply(emojiSubVader, '[',1)
justVaderDocLevelwEmoji <- do.call(rbind, lapply(justVaderDocLevelwEmoji, unlist))
justVaderDocLevelwEmoji[c(720, 804),]
emojiSubVader[[720]][2]

# End