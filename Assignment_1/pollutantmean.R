pollutantmean <- function(directory, pollutant, id=1:332) {
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    df_list <- list()
    df_all <- data.frame()
    result = 0
    #index = 1
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
        df_all <- rbind(df_all, df)
        #df_vec[index] <- df
        df_list <- c(df_list, list(df))
        #index <- index + 1        
    }
    if (pollutant == "sulfate") {
        result <- mean(df_all$sulfate, na.rm = TRUE)    
    } else if (pollutant == "nitrate") {
        result <- mean(df_all$nitrate, na.rm = TRUE)            
    }
    result
}