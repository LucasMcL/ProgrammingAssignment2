## This R file contains two functions: makeCacheMatrix and cacheSolve
## Further descriptions are given above the respective functions


## makeCacheMatrix takes a matrix as input and returns a list of functions
## which are called by the cacheSolve function.
## This function simply stores the matrix and provides the necessary
## data to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function takes as input a makeCacheMatrix object
## Using the functions defined within makeCacheMatrix, this function
## calculates and returns the inverse of the stored matrix value.
## If the inverse had been previously calculated, then the cached
## value is returned instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}