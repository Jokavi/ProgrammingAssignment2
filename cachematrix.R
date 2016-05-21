## a pair of functions that cache the inverse of a matrix
library(MASS)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix = function(x = matrix()) {
    inverse = NULL
    set = function(y){
        x <<- y
        inverse <<- NULL
    }
    get = function() x
    setinverse = function(Inverse) inverse <<- Inverse
    getinverse = function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve = function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse = x$getinverse()
    if(!is.null(inverse)){
        message("getting cached inverse")
        return(inverse)
    }
    message("inverse is not in memory, computing inverse...")
    data = x$get()
    inverse = ginv(data)
    x$setinverse(inverse)
    inverse
}
