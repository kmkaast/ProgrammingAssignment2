## makeCacheMatrix stores and returns 4 functions: set , get , setinverse, and getinverse They are briefly described below:
## set - retrieves new matrix, sets Cache to NULL
## get - returns the stored matrix
## setinverse - puts solved matrix into Cache
## getinverse - retrieves Cache

makeCacheMatrix <- function(x = numeric()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function checks if the Cache is null- if not null, then it gets the cached value, if null, then in calculates the inveserse 
 
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <-x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
