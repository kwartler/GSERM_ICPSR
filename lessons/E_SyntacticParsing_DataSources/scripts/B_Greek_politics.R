#' Purpose: Get data API from politico EU website
#' Author: Ted Kwartler
#' Date: May 23, 2023
#'

# Libraries
library(jsonlite)
library(lubridate)
library(ggplot2)
library(ggthemes)

# original site
# https://www.politico.eu/europe-poll-of-polls/


# Using F12 and identified the line chart data
pollData <- fromJSON('https://www.politico.eu/wp-json/politico/v1/poll-of-polls/GR-parliament')

# Backup file
#pollData <- fromJSON('https://raw.githubusercontent.com/kwartler/GSERM_ICPSR/main/lessons/E_SyntacticParsing_DataSources/data/politicoGreekPolls.json')

# Examine a portion of one party
head(data.frame(pollData$polls$date,pollData$polls$parties$SYRIZA))
vertLines <- pollData$events
head(vertLines)

# Organize the data
plotDF <- data.frame(date   = as.Date(pollData$polls$date),
                     SYRIZA = pollData$polls$parties$SYRIZA)
# Basic Plot
p <- ggplot(plotDF, aes(x=date, y = SYRIZA)) + 
  geom_point(alpha = 0.1, color = 'red') +
  theme_few()
p

## Mimic the "Kalman" smoothing on the site

# Make a ts object, dates dont really matter here just the values of the curve
tsData <- ts(plotDF[,2], start = c(year(min(plotDF$date)), month(min(plotDF$date))), frequency = 12)
tsData

# Fit a structural time series model with Kalman filter; 
# looks at a window of observations to predict the next observation with Bayesian probabilities 
kalman <- stats::StructTS(tsData, type="BSM") #result should have a kalman filter model
plotDF$KalmanValues <- KalmanSmooth(plotDF$SYRIZA,kalman$model)$smooth[,1]

p + geom_line(data = plotDF, aes(x = date, y = KalmanValues),color = 'red') +
  geom_vline(data = vertLines, aes(xintercept = as.Date(date)), alpha = 0.25) +
  geom_text(data = vertLines, aes(x = as.Date(date), y = 50, label = name_short), 
            angle = 90, vjust = -0.5, hjust = .8, size = 2, color = "black") + 
  ggtitle('Greece — 2023 general election', subtitle = 'https://www.politico.eu/europe-poll-of-polls/') 
  

# End