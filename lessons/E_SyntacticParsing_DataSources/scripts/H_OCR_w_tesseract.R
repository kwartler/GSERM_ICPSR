#' Purpose: Use tesseract for OCR
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' Date: May 28, 2023
#' Notes: this is largely poached from here:
#' https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html#read_from_pdf_files
#' 
## WARNING: Tesseract is an optical character recognition engine for various operating systems. It is free software, released under the Apache License, Version 2.0, and development has been sponsored by Google since 2006. THE R PACKAGE INSTALLS THE TESSERACT SOFTWARE

# libs
library(tesseract)

# Image file location; can be a folder
img <- 'https://prestonjg.files.wordpress.com/2015/07/ny-times-moon-front.jpg'
#http://jeroen.github.io/images/testocr.png #ENG
#http://a2010.kiosko.net/10/30/fr/lemonde.750.jpg #FRA

# Declare the language
#tesseract_info()$available
#tesseract_download("fra")
eng  <- tesseract("eng")

# Perform Optical Character Reco
text <- ocr(img, engine = eng)
cat(text)

# You can also get coordinates & confidence for each letter
results <- ocr_data(img, engine = eng)
results

# Here are tips to improve results:
#https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html


# End