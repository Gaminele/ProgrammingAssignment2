## Caching the Inverse of a matrix no singular

## This function, makeCacheMatrix creates a special "matrix"

## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
							inv <- NULL
							set <- function(y) {
							x <<- y
							inv <<- NULL
							}
							get <- function() x
							setInverse <- function(solve) inv <<- solve
							getInverse <- function() inv
							list(set = set, get = get,
							     setInverse = setInverse,
								 getInverse = getInverse)
}


### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv		
}
