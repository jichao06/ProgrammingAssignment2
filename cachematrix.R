## makeCacheMatrix will create a Matrix which could cache its inverse.
## cacheSolve would either calculate the inverse or retrieve the cache when it's available 

## A function to wrap a matrix with a cache of its inverse
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(i) inverse <<- i
    get_inverse <- function() inverse
    
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Calculate the inverse only when the cache is unavailable,
## Otherwise, the cache would be returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(is.null(x$get_inverse())) {
        data <- x$get()
        i <- solve(data)
        x$set_inverse(i)
    }
    else {
        message("getting cached inverse")
    }
    x$get_inverse()
}