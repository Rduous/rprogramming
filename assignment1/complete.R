source("helpers.R")

complete <- function(directory, id = 1:332) {
  result <- data.frame(id=numeric(), nobs=numeric())
  frameList <- lapply(id, getResultRow, directory = directory)
  return(Reduce(function(x,y) {rbind(x,y)}, frameList)) 
}

getResultRow <- function(i, directory) {
  complete <- readFiles(directory, i, complete.cases)
  newFrame <- data.frame(id=i, nobs=nrow(complete))
  return(newFrame)
}
