## Coursera Assignment - Caching the Inverse of a Matrix

## Creates a matrix object that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes inverse of matrix created by makeCacheMatrix. 
## If inverse has already been calculated, retrieve inverse from cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    y <- x$get()
    m <- solve(y, ...)
    x$setInverse(m)
    m
}