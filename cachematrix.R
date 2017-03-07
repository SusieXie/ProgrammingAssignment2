## Caching the inverse of a matrix
## To avoid the repeated computation for matrix inversion, I write 
## pair of functions to cache the inversion of the matrix


## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- null
      set <- function(y) {
      	      x <<- y
      	      inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
       
}


## This function computes the inverse of the "matrix"
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## cacheSolve should retrieve theinverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.mull(inv)) {
        	    message("getting cached data")
        	    return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
