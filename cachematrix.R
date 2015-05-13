## This file contains two functions designed to calculate
## and cache matrix inversions. 

## makeCacheMatrix creates a list with separate functions for
## setting and getting the value of both the input matrix and
## the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) m <<- inverse
        get_inv <- function() m
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}



## cacheSolve returns the inverse of the input matrix. If 
## the inverse of the matrix has already been computed and
## cached it returns the cached version.


cacheSolve <- function(x, ...) {
        m <- x$get_inv()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inv(m)
        ## Return a matrix that is the inverse of 'x'        
        m
}
