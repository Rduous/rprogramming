source("sortByOutcome.R")

ranks <- c("best", "worst")

rankhospital <- function(state, outcome, num = "best") {

    # validate state
    if (is.null(state)) stop("invalid state")

    # get sorted data
    sorted <- sortByOutcome(state, outcome)

    # validate/translate rank
    if (is.numeric(num)) {
        if (num > nrow(sorted)) return(NA)
    } else {
        if (is.character(num)) {
            if (identical(num, ranks[1])) num <- 1
            else if (identical(num, ranks[2])) num <- nrow(sorted)
        } 
    }
    if (! is.numeric(num)) stop("invalid rank")

    sorted[num,2]

}
