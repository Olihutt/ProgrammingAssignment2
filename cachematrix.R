## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix returns a list of x which includes functions to return x, set x, return the inverse, and set the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns either the cached inverse of x if it has already been cached
## Or computes the inverse of x, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## assigns the current cached inverse of x to m
    m <- x$getinverse()
    
    ## if the cached inverse is not null then returns the cached inverse
    if(!is.null(m)) {
        message("getting cached inverse")
        
        ## returns the cached inverse and exits the function
        return(m)
    }
    
    ## if it hasn't already exited the function, sets the matrix to data
    data <- x$get()
    
    ## computes the inverse and assigns it to m
    m <- solve(data, ...)
    
    ## sets the inverse of x to m
    x$setinverse(m)
    
    ## returns m
    m
}
