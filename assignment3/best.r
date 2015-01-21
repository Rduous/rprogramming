best <- function(state, outcome) {

    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

    ## Check that state and outcome are valid
    outcomeNames <- c("heart attack", "heart failure", "pneumonia")
    outcomeCols <- c(11, 17, 23)
    outcomeTypeIndex <- match(outcome, outcomeNames)
        
    if (is.null(state) || is.na(state) || sum(outcomes[,7] == state) == 0 ) stop("invalid state")
    if (is.null(outcome) || is.na(outcome) || is.na(outcomeTypeIndex) ) stop("invalid outcome")

    ## Return hospital name in that state with lowest 30-day death rate
    col <- outcomeCols[outcomeTypeIndex]
    # filter by state
    outcomes <- outcomes[(outcomes[,7] == state),]
    # cast row as numeric, for numeric sort
    outcomes[,col] <- as.numeric(outcomes[,col])
    # sort
    sorted <- outcomes[ order(outcomes[,col],outcomes[,7]),]
    sorted[1,2]
}
