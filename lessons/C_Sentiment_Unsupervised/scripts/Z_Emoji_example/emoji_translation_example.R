# This is merely an example substitution script.  The lexicon of emojis is loaded, 
# you would add your own text vector and then perform a multiple global substitution.
# libs
library(mgsub)
library(pbapply)

# read in the emoji list
emoji <- read.csv('emojis.csv')

# sapply with progress bar, the multiple global sub (mgsub)
subbedTxt <- pbsapply(textVector, mgsub, emoji$emoji, emoji$name)

