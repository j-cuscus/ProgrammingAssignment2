## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    ## As we are changing the matrix we need to clear the previous cached inverse
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)  
  
}


## Write a short comment describing this function

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
