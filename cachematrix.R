## Create functions which can cache certain computation results and retrieve them when required.  
##Use lexical scoping to cache values between the Global environment and sub environments.
## There are two functions required below, and they are patterned very closely after the makeVector and cachemean functions in the example documentation.

## makecacheMatrix is a function that creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv  
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## cachesolve computes the inverse of the special matrix returned by makecachematrix. 
##If the inverse has already been calculated and has not changed, then retrieve the inverse matrix from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
