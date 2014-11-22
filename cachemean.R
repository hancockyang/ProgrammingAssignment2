makeVector <- function(myVector = numeric()) {
  calculatedMean <- NULL
  set <- function(passVector) {
    myVector <<- passVector
    calculatedMean <<- NULL
  }
  get <- function() myVector
  
  setmean <- function(passMean)
  { calculatedMean  <<- passMean }
  getmean <- function() calculatedMean 
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  calculatedMean <- x$getmean()
  if(!is.null(calculatedMean)) {
    message("getting cached data")
    return(calculatedMean)
  }
  myVector <- x$get()
  calculatedMean <- mean(myVector, ...)
  x$setmean(calculatedMean)
  calculatedMean
}