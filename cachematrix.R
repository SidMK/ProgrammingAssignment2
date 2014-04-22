## The following function "makeCacheMatrix" will take x (a square invertible matrix) as input.
## o/p: a list serving to provide the input to cacheSolve(), containing the following functions to
##              1. set matrix
##              2. get matrix
##              3. set inverse
##              4. get inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL #inv will store the resultant inversion
  set = function(y) {
    x <<- y
    inv <<- NULL  # <<- assigns a value to an object for an environment different from current environment
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function "cacheSolve" will take x (the output of makeCacheMatrix()) as input.
## It will evaluate if inverse was calculated and return it if it was done. If it wasn't done, the function will calculate it and set the value in the cache.

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  # Now if inverse was already calculated, the following will evaluate and return it.
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse was not calculated the following will give inverse.
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # We will set the value of inverse in cache
  x$setinv(inv)
  
  return(inv)
}