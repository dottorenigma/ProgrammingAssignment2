## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than computing it repeatedly.
##
## makeCacheMatrix() and cacheSolve() implemented here allow for caching the inverse
## of a matrix. They are a direct application to the matrix inversion case of the
## pattern shown by the vectorMean example.
##
## if a is an invertible matrix and ca <- makeCacheMatrix(a) then 
## all(round(cacheSolve(ca) %*% ca$get()) == diag(rep(1,nrow(ca$get()))))
## holds TRUE
##
## makeCacheMatrix() returns a list of 4 functions that opearates on its
## environment that is used to cache a matrix and its inverse: 
##
##  set() sets the value of the matrix
##  get() gets the value of the matrix
##  setInv() sets the value of the inverse matrix
##  getInv() gets the value of the inverse matrix

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


## cacheSolve() computes the inverse of the matrix kept in makeCacheMatrix() .
## cacheSolve() retrieves the inverse from the cache using x$getInv(), if the
## inverse has already been calculated, otherwise computes the inverse of the 
## matrix returned by x$get() using solve() and caches the result using
## x.setInv() before returning it.

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


## makeCacheMatrix2() is an alternative implementation of the Matrix
## inverse caching problem that does not need the cacheSolve() function.
## makeCacheMatrix2() returns a list of 3 functions that opearates on its
## environment that is used to cache a matrix and its inverse:  
##
## set() sets the value of the matrix
## get() gets the value of the matrix
## getInv() gets the value of the inverse matrix
##
## if a is invertible matrix and ca <- makeCacheMatrix2(a) the following 
## invariant holds TRUE
## all(round(ca$getInv() %*% ca$get()) == diag(rep(1,nrow(ca$get()))))  

makeCacheMatrix2 <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  getInv <- function(...) {
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    inv <<- solve(x, ...)
    inv
  }  
  list(set = set, get = get, getInv = getInv) 
}