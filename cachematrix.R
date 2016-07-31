## The two functions in this file  allow for the creation of a matrix object
## that can cache its inverse after it is solved. Setting the data to new
## values wipes out the cached data causing the inverse to be solved the next
## time its requested


## The makeCacheMatrix function helps store the data and is really a list 
## containing functions to:
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of the mean
##    - get the value of the mean

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


## the cacheSolve function attempts to retrieve a solved inverse.
## If found the inverse is retruned. If the inverse matrix does
## not exist (retrieves NULL) it uses the traditional solve function 
## create the inverse which is then stored and returned 

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
  im
}
