corr <- function(directory, threshold=0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    #df_all <- data.frame()
    result <- c()
    files <- list.files(directory)
    flag = TRUE
    for (fileName in files) {
        fileName <- file.path(directory, fileName)
        df <- read.csv(fileName)
        df <- na.omit(df)
        correlation <- 0
        if (nrow(df) > threshold) {
            correlation <- cor(df$sulfate, df$nitrate)
            result <- c(result, correlation)
            flag <- FALSE
        }      
    }
    if (flag){
        numeric(0)
    } else {
        result    
    }
    
}