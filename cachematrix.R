## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix that allows it to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             # This initializes inv as null
  set <- function(y) {                    # This defines the set function with input variable y
    x <<- y                       
    inv <<- NULL                        # If there is a new matrix, reset inv to NULL
  }
  get <- function() x                     # This defines the get function which returns the value of the matrix argument x
  
  setinverse <- function(inverse) inv <<- inverse  # Sets the value of the matrix to the inverse of the input
  getinverse <- function() inv                     # Gets the value of the inverse when called.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix above
## Also, if the inverse has already been calculated then the cacheSolve will get hte inverse of the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
