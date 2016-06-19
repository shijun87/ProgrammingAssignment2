## Function makeCacheMatrix creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        
        ## Function set changes the matrix stored in the main function
        set <- function(y) {
                x <<- y
                ## restores inv_m to NULL if as the previous inverse matrix
                ## value is not required anymore when matrix is changed.
                inv_m <<- NULL
        }
        ## returns the matrix x stored in the main function
        get <- function() x
        ## does not calculate the inverse of the matrix. 
        ## setinv stores the value of the input in a variable inv_m into
        ## the main function makeCacheMatrix. getinv returns the value.
        setinv <- function(solve) inv_m <<- solve
        getinv <- function() inv_m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getinv()
        if(!is.null(inv_m)) {
                message("getting cached data")
                ## Verifies that the value of inv_m stored previously
                ## exists and is not NULL. If true, it gets the inverse 
                ## matrix from the cache and skips the computation. 
                return(inv_m)
        }
        
        ## If false, data gets the matrix stored with makeCacheMatrix, 
        ## inv_m calculates the inverse matrix and stores it with x$setinv()
        ## returns the inverse matrix. 
        data <- x$get()
        inv_m <- solve(data, ...)
        x$setinv(inv_m)
        inv_m
}
