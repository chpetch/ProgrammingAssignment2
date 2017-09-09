## Overall, this script contains two functions.
## 1. makeCacheMatrix is to make a cache for an inverse of a matrix x
## 2. cacheSolve is to recall if the inverse of the matrix x is already calculated.

## make function to cache matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
    x <<- y
    minv <<- x
  }
  get <- function(){
    x
  }
  setminv <- function(solve) minv <<- solve
  getminv <- function() minv
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}

## make function to recall a cache of matrix inversion of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setminv(minv)
  minv
}
