## The following functions use a memory cache to store the input and inverse 
## matrices. cacheSolve tries to find the saved inverse matrix before
## calculating one. 

## Creates a list that contains functions for getting and setting values in the
## input matrix and setting and getting the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    ## xInv is the inverse matrix.
    xInv <- NULL
    
    ## Function 1: Sets values in input matix. Makes the inverse matrix NULL.
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    
    ## Function 2: Returns the input matrix.
    get <- function() x
    
    ## Function 3: calculates the inverse matrix.
    setinv <- function(solve) xInv <<- solve
    
    ## Function 4: Retuns the inverse matrix.
    getinv <- function() xInv
    
    ## Return a list of the four functions.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## casheSolve returns the saved (cached) inverse matrix. If there is none then 
## it calcualtes the inverse matrix, saves it in the cache and returns it.

cacheSolve <- function(x, ...) {
    ## Retrieves the inverse matrix in the cache.
    m <- x$getinv
    
    ## Returns the cached inverse matrix and exits function.
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    
    ## Retrieves the input matrix.
    data <- x$get()
    
    ## Calculates the inverse matrix.
    m <- solve(data)
    
    ## Stored the inverse matrix in the cache.
    x$setinv(m)
    
    ## Returns the inverse matrix.
    m
}
