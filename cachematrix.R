## ProgrammingAssignment2：write a pair of functions that 
## cache the inverse of a matrix.

## This function creates a list of functions: 
## set: a function to set the matrix
## get: a function to get the matrix
## setinverse: a function to cache the inverse matrix
## getinverse: a function to get the inverse matrix setted by
##             setinverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) I <<- solve
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
        I
}
