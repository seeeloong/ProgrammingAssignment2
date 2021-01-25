## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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
