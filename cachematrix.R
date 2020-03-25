## Put comments here that give an overall description of what your
## functions do

## --------   makeCacheMatrix 
## This function is a list of functions:
##  + set: set in cache a matrix x
##  + get: return the matrix x
##  + setinverse: set 'inv' in cache, i.e. the inverse of x
##  + getinverse: return 'inv', i.e. the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseM) inv <<- inverseM
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## --------   cacheSolve 
## This function receives an argument of the 'makeCacheMatrix' type
## and returns the inverse matrix. If 'inv' is in cache it directly
## returns it, else it gets the matrix and computes its inverse using
## solve() and saves it to cache.

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
