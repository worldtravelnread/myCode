## Get Foreign Exchange Rate from Federal Reserve




## Install packages and load libraries
if(!require(dplyr, quietly = TRUE)) install.packages("dplyr")
library(dplyr)
if(!require(xlsx, quietly = TRUE)) install.packages("xlsx")
library(xlsx)


## Set home directory for this script
homeDir <- "C:/Users/b1050549/Documents/Anti_Corruption/0_ISC_payments/00_analysis_code"
setwd(homeDir)

## download html: the URLs for GBP and Euro are below. 
## Other currencies can be added
gbpFileUrl <- "http://www.federalreserve.gov/releases/h10/hist/dat00_uk.htm"
euroFileUrl <- "http://www.federalreserve.gov/releases/h10/hist/dat00_eu.htm"

data <- readLines(gbpFileUrl)
head(data, 250)

## Get the dates and rates lines
datePattern <- "<th id=\"r1\" headers=\"a1\">([^<]*)</th>"
ratePattern <- "<td headers=\"a2 a1 r1\">([^<]*)</td>"

dateLines <- grep(datePattern, data, value = TRUE)
head(dateLines, 25)

rateLines <- grep(ratePattern, data, value = TRUE)
head(rateLines, 25)


## Remove the html tags
dateHead <- "<th id=\"r1\" headers=\"a1\">"
dateEnd <- "</th>"
dates <- gsub(dateHead, "", dateLines)
dates <- gsub(dateEnd, "", dates)
head(dates, 25)

rateHead <- "<td headers=\"a2 a1 r1\">"
rateEnd <- "</td>"
rates <- gsub(rateHead, "", rateLines)
rates <- gsub(rateEnd, "", rates)
head(rates, 25)

## Remove white spaces/blanks
dates <- trimws(dates)
rates <- trimws(rates)
head(dates, 25)
head(rates, 25)


## Reformat the dates as dates
dates <- strptime(dates, "%d-%b-%y")
class(dates)
head(dates, 25)


## Convert ND values in rates to NA logical constant
ConvertND <- function(vector) {
        for(i in seq_along(vector)) {
                if(vector[i] == "ND") {
                        is.na(vector[i]) <- TRUE
                }
        }
        vector
}

rates <- ConvertND(rates)
rates <- as.numeric(rates)
head(rates, 25)


## Bind the columns and name them
dates <- as.data.frame(dates)
rates <- as.data.frame(rates)
forex <- bind_cols(dates, rates)
names(forex) <- c("Date", "Rate")
forex


## Get system date and time
currentDate <- format(Sys.time(), "%m%d%y_%H%M")
currentDate


## Write data frame to .csv
exportFile <- paste("forex", currentDate, ".csv", sep = "")
##write.csv(forex, file = exportFile)
