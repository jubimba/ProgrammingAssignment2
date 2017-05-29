## Functions created for the Coursera Introduction to R Week 3 course
## Peer-graded Programming Assignment 2: Lexical Scoping



## makeCacheMatrix creates a special matrix object that can cache
## its inverse in order to possibly save computing time later on

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve computes the inverse of a special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache, otherwise it computes it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
  
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    #print(data)
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
