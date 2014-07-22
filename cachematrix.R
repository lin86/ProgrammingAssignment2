## Function makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse as NULL
  inverse <- NULL
  
  ## set-function stores new matrix, and sets inverse back to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get-function shows currently stored matrix
  get <- function() x
  
  ## setinverse-function stores the calculated inverse (from cacheSolve)
  setinverse <- function(newinverse) inverse <<- newinverse
  
  ## getinverse-function shows the stored inverse
  getinverse <- function() inverse
  
  ## combine functions into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special matrix or, 
## if the inverse has already been calculated before, returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get the inverse currently stored in the special matrix object
  inverse <- x$getinverse() 
  
  ## if inverse is not NULL: retrieve the inverse from the cache and end the cacheSolve-funtion
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse is NULL: get matrix, calculate its inverse, and store inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
