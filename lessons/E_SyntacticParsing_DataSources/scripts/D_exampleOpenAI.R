#' Purpose: Example GPT 3.5 API call
#' Author: Ted Kwartler
#' May 4, 2023

# Only needed the first time, and can be saved to the environment to keep it from the actual script
# Add your own API key into this script and save it to your personal files folder.
# This script creates a system object with your API credentials.
# Blank example script in lesson scripts folder lessons/E_SyntacticParsing_DataSources/scripts/openAIKey.R
# Source the script from your personal files folder
source("~/Desktop/GSERM_ICPSR/personalFiles/openAIKey.R")

# Library
library(httr)
library(jsonlite)

# Get the API Key
apiKey <-Sys.getenv("OPENAI_KEY")

# Construct the prompting
promptList <- list(list(role='system',content = 'You are a helpful assistant.'),
                   list(role='user', content ='Who won the world series in 2020?'),
                   list(role='assistant', content = 'The Los Angeles Dodgers won the World Series in 2020.'),
                   list(role='user',content="Where was it played?"))


# Put together the API arguments
# Great resources for these parameters:
# https://docs.cohere.com/docs/temperature
# https://docs.cohere.com/docs/controlling-generation-with-top-k-top-p
args <- list(messages=promptList,
             model = 'gpt-3.5-turbo',
             temperature=0, # creativity
             max_tokens=256, # maximum numbers of tokens to return
             top_p=1,#probability distribution cutoff, lower values = less variety, higher values = more unpredictable
             frequency_penalty=0, # discourage the repetition of the same tokens
             presence_penalty=0) # discourage GPT-3 from generating text from a specific category

# Post the arguments and data to the openAI service
req <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  body = toJSON(args, auto_unbox=TRUE),
  add_headers (
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", apiKey)
  )
)

# Review the response object
req
status_code(req)
class(req)
tmp <- content(req)

# Obtain just the information returned
response <- capture.output(cat(fromJSON(content(req, as = "text", encoding = "UTF-8"))$choices$message$content))
response

# For NLP tasks it can do named entity recognition, sentiment analysis (though its hard to know the exact method)
# consider some text:
profURL <- 'https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/E_SyntacticParsing_DataSources/data/allBios.csv'

# Bring in the data
allGSERM <- read.csv(profURL)

# Construct the prompting
promptList <- list(list(role='system',content = 'You are a helpful assistant, trained to extract professor names and universities from text and return the information like this:
                        professor_name = Jim Smith
                        role           = Adjunct Professor,
                        university     = University of Hard Knocks'),
                   list(role='user', content ='Zeno AdamsZeno Adams is Assistant Professor of Finance at the University of St.Gallen. He teaches various courses in real estate markets and spatial econometrics. His classes focus on data analysis and applied econometrics using the free and powerful language R. His research is focused on the spatial interaction and spatial networks between real estate markets, firms, and population flows. His research has been published in various academic journals such as Journal of Financial and Quantitative Analysis, Journal of Banking and Finance, and Real Estate Economics.'),
                   list(role='assistant', content = 'professor_name = Zeno Adams
role = Assistant Professor of Finance,
university     = University of St.Gallen'),
                   list(role='user', content= allGSERM$bio[2]))

args <- list(messages=promptList,
             model = 'gpt-3.5-turbo',
             temperature=0, 
             max_tokens=256, 
             top_p=1,
             frequency_penalty=0, 
             presence_penalty=0) 

# Post the arguments and data to the openAI service
req <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  body = toJSON(args, auto_unbox=TRUE),
  add_headers (
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", apiKey)
  )
)

# Obtain just the information returned
response <- capture.output(cat(fromJSON(content(req, as = "text", encoding = "UTF-8"))$choices$message$content))

# Compare the extracted information to the original
response
allGSERM$bio[2]

# One could easily write a custom script for this [I have for other classes] but we'll use a loop
allProfs <- list()
for(i in 1:nrow(allGSERM)){
  print(i)
  # Construct the prompting
  promptList <- list(list(role='system',content = 'You are a helpful assistant, trained to extract professor names and universities from text and return the information like this:
                        professor_name = Jim Smith
                        role           = Adjunct Professor,
                        university     = University of Hard Knocks'),
                     list(role='user', content ='Zeno AdamsZeno Adams is Assistant Professor of Finance at the University of St.Gallen. He teaches various courses in real estate markets and spatial econometrics. His classes focus on data analysis and applied econometrics using the free and powerful language R. His research is focused on the spatial interaction and spatial networks between real estate markets, firms, and population flows. His research has been published in various academic journals such as Journal of Financial and Quantitative Analysis, Journal of Banking and Finance, and Real Estate Economics.'),
                     list(role='assistant', content = 'professor_name = Zeno Adams
role = Assistant Professor of Finance,
university     = University of St.Gallen'),
                     list(role='user', content= allGSERM$bio[i]))
  
  args <- list(messages=promptList,
               model = 'gpt-3.5-turbo',
               temperature=0, 
               max_tokens=256, 
               top_p=1,
               frequency_penalty=0, 
               presence_penalty=0) 
  
  # Post the arguments and data to the openAI service
  req <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    body = toJSON(args, auto_unbox=TRUE),
    add_headers (
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", apiKey)
    )
  )
  
  # sometimes openAI gets too many requests and returns a 429 error
  # "message": "That model is currently overloaded with other requests. 
  # You can retry your request, or contact us t...
  if(status_code(req)==429){
    Sys.sleep(2)
    warning('429 returned, waiting 2 seconds before trying again.  If this fails again just start at the correct i')
    req <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      body = toJSON(args, auto_unbox=TRUE),
      add_headers (
        "Content-Type" = "application/json",
        "Authorization" = paste("Bearer", apiKey)
      )
    )
  }
  
  # Obtain just the information returned
  response <- capture.output(cat(fromJSON(content(req, as = "text", encoding = "UTF-8"))$choices$message$content))
  resp <- t(as.data.frame(response))
  resp <- as.data.frame(resp)
  names(resp) <- c('profName','role','uni')
  allProfs[[i]] <- resp
}

# Assemble
allProfsDF <- do.call(rbind, allProfs)

# I guess my prompting response needs to be cleaned up!
allProfsDF$profName <- gsub('professor_name = ','',allProfsDF$profName)
allProfsDF$role     <- gsub('role = |,','',allProfsDF$role)
allProfsDF$uni      <- unlist(lapply(strsplit(allProfsDF$uni,'= '), tail,1)) #weird spacing!
head(allProfsDF)

# End