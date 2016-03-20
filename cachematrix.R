## Put comments here that give an overall description of what your
## functions do

## This function creates an object list containing the following functions:
## 1. set de value of the matrix
## 2. get de matrix
## 3. set inverse of the matrix
## 4. get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(invIn) inv <<- invIn
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function checks whether the object list of the original matrix already has the inverse matrix; 
## if positive, returns the inverse matrix; 
## if negative, computes the inverse matrix, set this on the cache of list object
## and then return the inverse matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}