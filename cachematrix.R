## These pair of functions cache the inverse of a matrix.


makeCacheMatrix <- function(x = matrix()) {
        ##  This function creates a special "matrix" 
        ## object that can cache its inverse.
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

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" 
        ## returned by makeCacheMatrix above. 
        ## then the cachesolve should retrieve the inverse from the cache.
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
