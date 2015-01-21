source("sortByOutcome.R")

ranks <- c("best", "worst")

rankall <- function(outcome, num = "best") {

    # get sorted data
    sorted <- sortByOutcome(outcome = outcome)

    stateNames <- unique(sorted[,7])
    stateNames <- sort(stateNames)

    splitOutcomes <- split(sorted, f=sorted[,7])
    hospitals <- sapply(stateNames, getNthElement, num, splitOutcomes )

    data.frame( hospital=hospitals, state=stateNames)
}

getNthElement <- function(x, num, splitOutcomes)  {
    if (identical(num, ranks[1])) {
        n <- 1 
    }
    else if (identical(num, ranks[2])) {
        n <- nrow(splitOutcomes[[x]])
    }
    else { 
        n <- num 
    }
    splitOutcomes[[x]][n,2]
}
