rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    outcomeData[,11] <- suppressWarnings(as.numeric(outcomeData[,11]))
    outcomeData[,17] <- suppressWarnings(as.numeric(outcomeData[,17]))
    outcomeData[,23] <- suppressWarnings(as.numeric(outcomeData[,23]))
    
    ## Check that state and outcome are valid
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    states <- unique(outcomeData$State)
    
    if (!state %in% states) stop("invalid state")
    
    if (!outcome %in% outcomes) stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    hospNames <- c()
    states <- sort(states)
    for (state in states) {
        hospName <- rankhospital(state, outcome, num)
        hospNames <- c(hospNames, hospName)
    }
    df <- data.frame(hospital=hospNames, state=states)
    df
    
}