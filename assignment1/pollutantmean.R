source("helpers.R", local=TRUE)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  csv <- readFiles(directory, id)
  return(mean(csv[,pollutant], na.rm=T))
}