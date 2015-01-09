### list files without having to change the directory
listFiles <- function(directory = getwd()) {
  list.files(directory)
}

### Sanity check function
countRows <- function(directory, ids=1:332) {
  files <- listFiles(directory)
  num <- 0
  for (i in ids) {
    num <- num + nrow(read.csv(files[[i]]))
  }
  return(num)
}

### Can I generalize?  Allow row-filtering function to be abstract for reuse
### Usage: noNaNs <- readFilesWithFilter(getwd(), fxn=complete.cases)
readFiles <- function(directory, ids = 1:332, fxn = trivialFun) {
  files <- listFiles(directory)
  oldwd <- getwd()
  setwd(directory)
  csvList <- lapply(ids, readFileToCsvWithFilter, fileList = files, fun = fxn)
  setwd(oldwd)
  return(Reduce(function(x,y) {rbind(x,y)}, csvList)) 
}

### use the passed-in filter fxn to return only those rows that pass the filter
readFileToCsvWithFilter <- function(id, fileList, fun) {
  csv <- read.csv(fileList[[id]])
  good <- fun(csv)
  n <- nrow(csv)
  return(csv[good,])
}

### Default filter function that returns all rows
trivialFun <- function(x) {
  return(rep(T, nrow(x)))
}
