source("helpers.R", local=TRUE)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  csv <- readFiles(directory, id)
  good <- ! is.na(csv[,pollutant])
  values <- csv[good,pollutant]
  #print(values)
  return(mean(values))
}
