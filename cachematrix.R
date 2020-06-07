## Create an environment that contains functions to set and get an matrix as
## well as functions for computing and getting it´s inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Using the environment created above to give out the inverse of an matrix
## if already computed before, it´s getting the inverse matrix out of the cache
## if not already computed it uses the functions written above in the list of functions
## to compute it

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
}