## Functions to support efficient matrix inversion by caching inverse so
## is not calculated twice.



## This function creates a special "matrix" object that can cache its
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # this will hold the cached inverse

  ## Method to reset the matrix to a new value
  ## also clears the inverse cache
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ##  Method to return the value of the matrix
  get <- function() {x}

  ## Method to set the cached inverse
  setinverse <- function(inverse) {inv <<- inverse}

  ## method to get the cached inverse
  getinverse <- function() {inv}

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the invers has not previously been computed it computes it and caches
## the results.
## If the inverse has already been calculated (and  the matrix has
## not changed), then the cachesolve should retrieve the inverse from
## the cache.
##
## Assumes that the matrix supplied is invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)){
    # The inverse has not been cached

    # compute the invese
    inv <- solve(x$get(), ...)

    # Cache the result
    x$setinverse(inv)
  }else
  {
    # the invers has been cached
    message("getting cached data")
  }
  # Return a matrix that is the inverse of 'x'
  inv
}
