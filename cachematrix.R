## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly.
## The pair of functions implemented here allows for caching the inverse of a matrix.

## makeCacheMatrix() creates an object that can cache a matrix and its inverse. 
## It returns a list of 4 functions: 
##
## set() sets the value of the matrix
## get() gets the value of the matrix
## setInv() sets the value of the inverse matrix
## getInv() gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse 
  getInv <- function() inv 
  list(set = set, get = get, setInv = setInv, getInv = getInv) 
}


## cacheSolve() computes the inverse of the matrix kept in makeCacheMatrix().
## cacheSolve() retrieves the inverse from the cache, if the inverse has already been
## calculated, otherwise computes the inverse using solve(), caches the result
## in makeCacheMatrix() before returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
