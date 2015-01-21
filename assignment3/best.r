source("sortByOutcome.R")

best <- function(state, outcome) {
    if (is.null(state) ) stop("invalid state")
    sorted <- sortByOutcome(state,outcome)
    sorted[1,2]
}
