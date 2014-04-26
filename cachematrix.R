makeCacheMatrix <- function(x = matrix()) {
	  ## This function creates a matrix that can cache its inverse.
	  m <- NULL
        set <- function(y) {   ##setting up functions set and get
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve ##setting up functions for setting and getting Inverse of the matrix
        getInverse <- function() m
        list(set = set, get = get,  ##creating the list of functions
             setInverse = setInverse,
             getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
	  m <- x$getInverse()  ##query the cache
        if(!is.null(m)) {    ##if there is something in the cache, it returns it
                message("getting cached data")
                return(m)
        }
        data <- x$get() ##in the case of no cache, calculation of the inverse
        m <- solve(data, ...)
        x$setInverse(m)
        m

}