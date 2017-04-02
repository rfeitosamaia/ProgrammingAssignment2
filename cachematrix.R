## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list containing 4 functions to set and get a matrix and its
## inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    return(x)
  }
  
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  getinverse <- function() {
    return(i)
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function calculates the inverse of a matrix and set it to the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
