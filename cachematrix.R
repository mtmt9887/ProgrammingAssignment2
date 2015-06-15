## The following pair of functions compute and cace the 
## inverse of a matrix. Considering that the supplied matrix
## is always invertible

## Creates a special "matrix" object that can cache its inverse
## Considering that the provided matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
  
  ##Input: x, square invertible matrix
  
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  #Returns list containing funcions to
  ##  1. Get the matrix
  ##  2. Set the matrix
  ##  3. Get the inverse
  ##  4. Set the inverse

}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated 
##(and the matrix has not changed), 
## then the cachesolve retrieves the result from the cache.

cacheSolve <- function(x, ...) {
  
  ## Input: x, output from makeCacheMatrix
       
  m <- x$getinverse()
  
  ## If the inverse has already been computed,
  ## it will be retireved from cache
  if(!is.null(m)) {
    message("getting cached data")
    
    ## Returns the inverse of the matrix
    return(m)
  }
  
  ## if not in cache, the inverse will be computed
  mat <- x$get()
  m <- solve(mat, ...)
  
  ## if computed, the value of the inverse will be
  ## stored in the cache
  x$setinverse(m)
  
  ## Returns the inverse of the matrix
  m
  
}
