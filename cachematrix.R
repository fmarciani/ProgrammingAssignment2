## These functions compute the inverse of a matrix, then cache the
## inverse so that it can be retrieved. This means we are taking
## advantage of the lexical scoping rules in R to preserve the matrix 
## inverse, so that we don't have to compute it repeatedly.

## In makeCacheMatrix, we first create a "matrix" object to store our 
## cache of the matrix inverse. The function is written so that we can 
## a) set the value of the matrix, b) get the value of the matrix, 
## c) set the value of the inverse, and d) get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## cacheSolve calculates the inverse of the "matrix" created with 
## makeCacheMatrix. cacheSolve first checks to see if the inverse 
## has already been calculated, and if so, it retrieves the inverse 
## from the cache, skipping the computations. If the inverse has not 
## already been calculated, this function calculates the inverse of 
## the matrix and caches it using the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
