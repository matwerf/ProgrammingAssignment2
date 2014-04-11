## Pair of functions that cache the inverse of a matrix

## creates a special "matrix" object to cahce the inverse, 
## which is really a list containing functions that contains the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##'x' is an invertible matrix
  ## Returns list containing the functions
  m <- NULL
  ##sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##gets the value of the matrix
  get <- function() x
  ##sets the value of the inverse
  setinverse <- function(solve) m <<- solve
  ##gets the value of the invese
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## 'x' is a cached matrix from the makeCacheMatrix
  ## Return a matrix that is the inverse of 'x'
  ## prints "getting cached data if the inverse was cached"
  
  ## checks for a cached matrix, 
  ## if cached prints a message and returns the inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not previously cached, calculates and caches the inverse
  #then returns the inverted matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
