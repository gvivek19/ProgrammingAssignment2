## MakeCacheMatrix creates a matrix Object with getters and setters and also calculating & storing the inverse of the matrix.
## CacheSolve returns the inverse of the matrix

## This function creates an object with 4 functions.
## set -> Sets the matrix
## get -> Gets the original matrix
## setinverse -> Sets the inverse of the original matrix. Has a parameter which holds the inverse matrix.
## getinverse -> Returns the inverse matrix set using setinverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function returns the inverse matrix of the object passed.
## It uses the makeCacheMatrix functions to store n the cache.
## If the value is not in the cache, it is calculated, stored in the cache ad returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
