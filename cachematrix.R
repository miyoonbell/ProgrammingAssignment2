## cachematrix.R:
## The purpose of this function is to write a pair of functions 
## that cache the inverse of a matrix.

## Usage:
# > x <- matrix(rnorm(9), 3,3) 
# > m <- makeCacheMatrix(x) 
# > m$get()
# > cacheSolve(m)
# > cacheSolve(m) -> you can get cached data.

## makeCacheMatrix:
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { # global variable <- function local value
                x <<- y 
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(ivm) m <<- ivm 
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data) #calculate inverse of matrix
        x$setmatrix(m)
        m
        }
