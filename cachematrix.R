## These functions compute the inverse of a matrix, then cache the inverse so that it can be retrieved. 
## Via the lexical scoping rules in R, caching stores the matrix inverse, so we don't have to compute it 
## over and over.

## makeCacheMatrix creates a "matrix" object to store the cache of the matrix inverse. It a) sets the value
## of the matrix, b) gets the value of the matrix, c) sets the value of the inverse, and d) gets the value 
## of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Making inv to store the cached inverse matrix:
        inv <- NULL
        
        ## Setting the value of the matrix:
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## Getting the value of the matrix:
        get <- function () x
        
        ## Setting the value of the inverse:
        setinverse <- function(inverse) inv <<- inverse
        
        ## Getting the value of the inverse:
        getinverse <- function() inv
        
        ## Returning the matrix, using the functions above:
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)
}


## cacheSolve checks to see if the inverse has already been calculated. If so, it retrieves the inverse 
## from the cache and skips calculations. If not, the matrix is calculated, then cached using setinverse.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        ## Checking to see if the inverse is calculated, and returning it from the cache:
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Calculating the inverse if there is none in the cache:
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Caching the inverse:
        x$setinverse(inv)
        
        ## Returning the inverse:
        inv
}
