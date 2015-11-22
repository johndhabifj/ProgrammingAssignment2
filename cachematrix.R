## These functions enables 2 same matrix to be inversed only once 
## by utilizing a cache.Whenever the same matrix inversion is repeated,
## the actual computation will only be done the first time. The repetition
## will only get the result straight from cache.

## This function is a list of cache operation
## set: store the target matrix for inversion
## get: get the target matrix (if no matrix is stored, matrix 1X1 with value NA returned)
## setinverse: store the inverted matrix to cache
## getinverse: get inverted matrix from cache
## contains 2 cache, target matrix & inverted matrix cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes created object from the first function as input
## If the cache is empty or not a matrix, target matrix will be inverted
## else, inverted matrix from cache is used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(is.matrix(m)) {  #instead of using is.null, is.matrix used to prevent usage of invalid matrix
    message("getting cached data")
    return(m)
  }
  ## get target matrix to be inverted from target matrix cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
