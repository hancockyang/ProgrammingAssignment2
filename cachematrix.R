## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(myMatrix = matrix()) {
    calculatedinverse <- NULL
    set <- function(passMatrix) {
        myMatrix <<- passMatrix
        calculatedinverse <<- NULL
    }
    get <- function() myMatrix
    
    setinverse <- function(passInverse)
    { calculatedinverse  <<- passInverse }
    getinverse <- function() calculatedinverse 
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    calculatedinverse <- x$getinverse()
    if(!is.null(calculatedinverse)) {
        message("getting cached data")
        return(calculatedinverse)
    }
    myMatrix <- x$get()
    calculatedinverse <- solve(myMatrix, ...)
    x$setinverse(calculatedinverse)
    calculatedinverse
}
