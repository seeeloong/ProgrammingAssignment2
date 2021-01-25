## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a list of 4 functions, 
## Set: which set the input matrix as an argument for later use
## Get: retrieve input matrix for later calculation
## Setmean: save the previously calculated inverse matrix as cache
## Getmean: retrieve the previously calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) {
    m <<- mean
  }
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
}

## cacheSolve is for executing the functions to calculate inverse matrix
## if there is a matrix already stored (calculated previously), it will output the previously calculated inverse matrix
## if there is a new matrix input (i.e. no previously calculated output, m = NULL), data will take the new matrix input by x$get()
## then calculate(data,...) will call the function 'calculate' defined below, to calculate inverse matrix
## x$setmean(m) will store the new inverse matrix as cache

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- calculate(data, ...)
  x$setmean(m)
  m
}

calculate<-function(x){
  a<-x[1,1]
  b<-x[2,1]
  c<-x[1,2]
  d<-x[2,2]
  f<-(1)/((a*d) - (b*c))
  x[1,1]<-d*f
  x[2,1]<--b*f
  x[1,2]<--c*f
  x[2,2]<-a*f
}
