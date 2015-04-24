complete <- function(directory, id=1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    numOfRowVec <- c()
    for (i in id) {
        fileName = ""
        if (i < 10) {
            fileName = paste("00", i, ".csv", sep="")  
        } else if (10 <= i & i < 100) {
            fileName = paste("0", i, ".csv", sep="")
        } else if (100 <= i & i < 999) {
            fileName = paste(i, ".csv", sep="")   
        }
        fileName <- file.path(directory, fileName)
        df <- read.csv(fileName)
        df <- na.omit(df)
        numOfrows = nrow(df)
        numOfRowVec <- c(numOfRowVec, numOfrows)
    }
    result <- data.frame(id=id, nobs=numOfRowVec)
    result
}