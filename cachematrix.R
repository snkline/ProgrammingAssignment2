## Functions to wrap a matrix with functions for getting the value,
## setting the value, getting the inverse, and setting the inverse,
## and solving the inverse using the wrapped matrix.

## makeCacheMatrix wraps a matrix witht functions to get and set both the
## matrix, and the inverse of the matrix.
## get() returns Matrix
## set(Matrix) sets Matrix value
## getinverse() gets Inverse Matrix, NULL if unset
## setinverse(Matrix) sets Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL    
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Wraps matrix solve() function. First checks to see if inverse has
## already been calculated and returns it if so. Otherwise calculate
## inverse using solve() and store the inverse in cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat,...)
  x$setinverse(inverse)
  inverse
}
