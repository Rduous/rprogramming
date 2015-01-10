source("helpers.R")

corr <- function(directory, threshold = 0) {
  id <- 1:332
  csvList <- lapply(id, function(x) {readFiles(directory, x, complete.cases)})
  numRowsList <- lapply(csvList, nrow)
  passesThresholdList <- csvList[numRowsList > threshold]
  result <- sapply(passesThresholdList, function(x) {cor(x[,"nitrate"], x[,"sulfate"],use = "all.obs")})
  return(result)
}
