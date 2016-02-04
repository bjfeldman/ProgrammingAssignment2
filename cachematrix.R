## Put comments here that give an overall description of what your
## functions do

## This pair of functions calculates and caches the inverse of a matrix, which 
## can then be used repeatedly without recalculating it each time it's used in
## another function.

## Write a short comment describing this function
## The first function, `makeCacheMatrix` caches a matrix, which is
## part of a list that contains functions to set the value of the matrix, get 
## that value, set the value of the matrix inverse, and get that value

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- solve(y)
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks to see whether the inverse of the matrix, defined when the 
## first function was called, is already cached. If not, the function calculates the 
## inverse and inserts it in the cached list produced by the previous function. 
## Finally, it returns the matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}



