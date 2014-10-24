## The functions in this script can be used to cache the result of calculating 
## the inverse of a matrix.
## To create matrix whose inverse can be cached, use the makeCacheMatrix 
## function, passing the original matrix as an argument.
## The cacheSolve function can then be used to evaluate the inverse of a 
## cacheable matrix. cacheSolve will check whether the inverse has already been 
## computed. If so, the cached result will be returned. Otherwise, it will be
## computed and stored in the cache.
## 
## Example usage:
## m <- matrix (c(1,2,3,4),nrow=2,ncol=2)
## cacheableMatrix <- makeCacheMatrix(m)
## computedInverse <- cacheSolve(cacheableMatrix) ## The inverse is computed 
## cachedInverse <- cacheSolve(cacheableMatrix) ## The inverse is returned from cache


## makeCacheMatrix enables caching of the inverse of the matrix passed as argument 

makeCacheMatrix <- function(x = matrix()) {
  
  ## the cached matrix
  cachedInverse <- NULL
  ## set a new matrix and reset the cache to NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  ## get the matrix to invert
  get <- function() x
  ## populate the cache
  setInverse <- function(inverse) cachedInverse <<- inverse
  ## retrieve the cache
  getInverse <- function() cachedInverse
  ## return the list with all functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of a cacheable matrix.
## If the inverse for a cacheable matrix has already been calculated,
## the result will be returned form the cache, not recomputed.

cacheSolve <- function(x, ...) {
        
  ## get the inverse from the cache
  m <- x$getInverse()
  
  ## if the cache is not null, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## the cache was null, so we get the matrix and compute its inverse
  data <- x$get()
  ## compute the inverse
  m <- solve(data, ...)
  ## store the inverse in the cache
  x$setInverse(m)
  ## return the result
  m
}
