## The makeCacheMatrix and cacheSolve functions are used to reduce the need
## of recaculating the inverse of a matrix repeatedly in a loop by caching the 
## the inverse of the specified matrix. The makeCacheMatrix is used to create a 
## special "matrix" to store the inverse and the cacheSolve function is used to 
## compute the inverse of the created matrix if the value is not already in 
## the cache.

## The makeCacheMatrix function creates a special "matrix", which is really a 
## special list containing a function to set the value of the matrix, get the 
## value of the matrix, set the value of the inverse, and get the value of the 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## The cacheSolve function calculates the inverse of the matrix created
## if the value of the inverse is not already stored in the cache.  
## If the value is stored in cache, the funciton skips the computation 
## and retrieves the value from cache. 

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}