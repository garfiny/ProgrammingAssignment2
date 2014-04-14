## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# set original matrix and initial inverse value to be NULL
# defines get and st inverse functions
# defines inverse function for inversing
# returns list of defined funtions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  inverse <- function() {
    m <- matrix(1, ncol(x), nrow(x))
    m[1: ncol(x),] <- x[, 1:ncol(x)]
    m
  }
  list(set = set, get = get,
       inverse = inverse,
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Write a short comment describing this function
# getInverse first of argument 'x' and check if is null
# if x is not null, retrives the cached inversed matrix
# if x is null, inverse the matrix and set it back
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- x$inverse()
  x$setInverse(i)
  i
}
