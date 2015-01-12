source("helpers.R")

complete <- function(directory, id = 1:332) {
  nobs <- sapply(id, function(x, directory) {nrow(readFiles(directory, x, complete.cases))}, directory=directory)
  result <- data.frame(id, nobs)
  return(result)
}
