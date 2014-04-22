## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  null <- NULL
  set <- function(mat) {
    x <<- mat
    null <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) null <<- inverse
  getInverse <- function() null
  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setinverse = setInverse)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedInverse <- x$getinverse()
  if(!is.null(cachedInverse)) {
  return(cachedInverse)
  }
  data <- x$get()
  cachedInverse <- solve(data, ...)
  x$setinverse(cachedInverse)
  cachedInverse
}
