## This set of functions will create a matrix object that 
## can cache its inverse, retrieve the cached matrix, and
## retrieve or calculate its inverse.

## the makeCacheMatrix function will create a matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {i <<- inverse}
  getinv <- function() {i} 
  list(set = set, get = get, setinv = setinv, 
       getinv = getinv)
}


## The cacheSolve function will input the matrix created by
## the makeCacheMatrix function and either retrieve its inverse
## if it has already been calculated or, if not,  calculate it
## using the solve() function

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
