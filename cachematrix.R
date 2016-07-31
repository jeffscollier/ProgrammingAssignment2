## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {        
  
  im <- NULL
  
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x

  setInverted <- function(inverted_matrix) im <<- inverted_matrix

  getInverted <- function() im

    list(set = set, get = get,
     setInverted = setInverted,
     getInverted = getInverted)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverted()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverted(im)
  iim
}
