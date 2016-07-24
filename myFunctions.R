## Generic functions written at work
## Sharon Francisco
## 14 July 2016


## AddVectorFunction
## This function adds a new vector with the same value to a data frame
AddVector <- function(vectorName, vectorData, target) {
        number <- nrow(target)
        newColumn <- rep(vectorData, number)
        newColumn <- as.data.frame(newColumn)
        names(newColumn) <- vectorName
        newColumn <- tbl_df(newColumn)
        target <- bind_cols(newColumn, target)
}


## Clean up function
CleanUp <- function(find, replace, data) {
        beforePattern <- paste("(^|[A-z]|[^A-z])+", find, 
                               sep = "")
        afterPattern <- paste(find, "($|[A-z]|[^A-z])+", 
                              sep = "")
        data <- gsub(pattern = beforePattern, 
                     replacement = replace, x = data)
        data <- gsub(pattern = afterPattern, 
                     replacement = replace, x = data)
}


## Function to search for character strings
mySearch <- function(find, where) {
        target <- paste("(^|[A-z]|[^A-z])+", find, "|", find, 
                        "($|[A-z]|[^A-z])+", sep = "")
        result <- grep(pattern = target, x = where, value = TRUE)
}


## Function to return logical value for search
FndTextLogical <- function(find, where) {
        what <- paste("(^|[A-z]|[^A-z])+", find, "|", find, 
                      "($|[A-z]|[^A-z])+", sep = "")
        result <- grepl(pattern = what, x = where)
}


## This function is specific to foreign exchange rate calculations
## to change a rate, specifically if a rate is missing or NA.

UpdateRate <- function(dfDateColumn, targetDate, dfRateColumn,
                       newRate) {
        for(k in seq_along(dfRateColumn)) {
                if(dfDateColumn[k] == targetDate) {
                        dfRateColumn[k] <- newRate
                }
                else {
                        dfRateColumn[k]
                }
        }
        dfRateColumn
}


