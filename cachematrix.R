## The functions in this script can be used to cache the result of 
## calculating the inverse of a matrix.
## To create matrix whose inverse can be cached, we need to use the
## makeCacheMatrix function, passing the matrix as an argument.
## 

## makeCacheMatrix enables caching of the inverse of the matrix passed
## as argument 

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of a cacheable matrix
## if the inverse for a cacheable matrix has already been calculated,
## the result will be returned form the cache, not recomputed

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
