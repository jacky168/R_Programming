best <- function(state, outcome) {
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

    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    if (outcome == "heart attack") {
        hospNames <- by(outcomeData, outcomeData[,7], function(x) x[which(x[,11]==min(sort(x[,11]), na.rm=TRUE)),2]) 
    } else if (outcome == "heart failure") {
        hospNames <- by(outcomeData, outcomeData[,7], function(x) x[which(x[,17]==min(sort(x[,17]), na.rm=TRUE)),2]) 
    }  else if (outcome == "pneumonia") {
        hospNames <- by(outcomeData, outcomeData[,7], function(x) x[which(x[,23]==min(sort(x[,23]), na.rm=TRUE)),2]) 
    }
    hospName <- hospNames[state][[1]]
    hospName
}