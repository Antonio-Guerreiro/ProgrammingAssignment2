## These functions show the use of the scoping rules of R
## and of the <<- operator to cache the value of an object
## rather than to recompute it.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(M = matrix()) {
        i <- NULL
        set <- function(N) {
                M <<- N
                i <<- NULL
        }
        get <- function() M
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" M
## built by the makeCacheMatrix function above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.
## Assumes that the matrix supplied is invertible.

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of the special 'matrix' M
        i <- M$getinv()
        if(!is.null(i)) {
                message("getting cached data...")
                return(i)
        }
        data <- M$get()
        i <- solve(data)
        M$setinv(i)
        i
}
