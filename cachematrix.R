## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a list of 4 functions, 
## Set: which set the input matrix as an argument for later use
## Get: retrieve input matrix for later calculation
## Setinv: stores the previously calculated inverse matrix as cache
## Getinv: retrieve the previously calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) {
    m <<- solve
  }
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## cacheSolve is for executing the functions to calculate inverse matrix
## if there is a matrix already stored (calculated previously), it will output the previously calculated inverse matrix
## if there is a new matrix input (i.e. no previously calculated output, m = NULL), data will take the new matrix input by x$get()
## then solve(data,...) will calculate inverse matrix
## x$setinv(m) will store the new inverse matrix as cache
## at last, the inverse matrix 'm' will be printed as output

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
