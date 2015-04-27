rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (outcome == "heart attack") {
        stateOutcome <- by(outcomeData, outcomeData[,7], function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,x$Hospital.Name, decreasing=FALSE),c(2,11)])
        # transform into data fame
        stateOutcome <- na.omit(stateOutcome[[state]])
#        sortStateOutcome <- stateOutcome[order(stateOutcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
#        sortStateOutcome <- na.omit(sortStateOutcome)
        
    } else if (outcome == "heart failure") {
        stateOutcome <- by(outcomeData, outcomeData[,7], function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,x$Hospital.Name, decreasing=FALSE),c(2,17)])
        # transform into data fame
        stateOutcome <- na.omit(stateOutcome[[state]])
        
    }  else if (outcome == "pneumonia") {
        stateOutcome <- by(outcomeData, outcomeData[,7], function(x) x[order(x$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,x$Hospital.Name, decreasing=FALSE),c(2,23)])
        # transform into data fame
        stateOutcome <- na.omit(stateOutcome[[state]])
    }
    if (num == "best") {
        num = 1
    } else if (num == "worst") {
        num = nrow(stateOutcome)
    }
    hospName <- stateOutcome[num,1]
    hospName

}