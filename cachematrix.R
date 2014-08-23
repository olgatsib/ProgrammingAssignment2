## The pair of functions caches the inverse of a matrix, 
## since matrix inversion is usually a costly computation.
## Assumption: the matrix supplied is always invertible

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                   # reset inverse to null for a new object
  set <- function(y) {              # set an instance of the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x               # return the instance of the matrix
  setInverse <- function(solve) inverse <<- solve # save inverse matrix in cache
  getInverse <- function() inverse  # return inverse matrix from cache
  # put all accessible functions into the list
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get inverse matrix of 'x' from cache if exists
  inverse <- x$getInverse()    # get inverse from cache
  print (inverse)
  if (!is.null(inverse)) {     # inverse matrix exists in cache
    message("Getting cached data")
    return (inverse)
  }
  data <- x$get()              # get an instance of the matrix
  inverse <- solve(data, ...)  # compute inverse matrix for the returned instance
  x$setInverse(inverse)        # save result into cache
  inverse                      # return result
}
