## Put comments here that give an overall description of what your
## functions do

## The goal of the 'makeCacheMatrix' function is to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
      x <<- y
      cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The goal of the 'cacheSolve' function is to return the inverse of a matrix.
## It checks to see if the inverse has already been computed and if so,
## the function skips the calculation and retrieves the cached value.
## Otherwise it computes the inverse. 
## This function assumes the matrix is always invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseFunction <- x$getInverse()
    if(!is.null(inverseFunction)) {
       message("Getting cached data")
       return(inverseFunction)
  }
  data <- x$get
  inverseFunction <- solve(data, ...)
  x$setInverse(inverseFunction)
  inverseFunction
}
