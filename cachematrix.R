## Functions for calculating the inverse of a matrix.
## The inverse can be cached and returned whenever needed. 
## If the matrix is altered the cached inverese will be removed, and 
## a new inverse calculated when needed.

## Creates and returns an object to hold a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  ## set function. This saves the matrix and clears the previous cached inverse
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## Get function. Returns the matrix
  get <- function() x
  
  ## Set inverse  
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  ## Get inverse  
  getInverse <- function() inverseMatrix
  
  ## Return all four methods we have just defined
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)  
  
}

## Returns the inverse of a matrix. This will return the cached copy
## if available, otherwise calculates it and saves it in the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First check if the inverse exists
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## Nope, we will have to create it ourselves.
  matrix <- x$get()
  if(!is.null(matrix)) {
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    return(inverse)
  }
}
