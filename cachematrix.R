## Matrix inversion is usually a costly computation and their may 
## be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix,
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getInverse() # tries to retrieve the inverse from makeCacheMatrix
    if(!is.null(m)) { # if inverse is present in makeCacheMatrix
        message("getting cached data")
        return(m) # returns the inverse
    }
    data <- x$getMatrix() # assigns the matrix from makeCacheMatrix to var data
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
