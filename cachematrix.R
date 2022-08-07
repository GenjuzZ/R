## To solve the problem of a costly computation,
## matrix inversion, I worte a pair of functions
## that are used to create a special object that 
## can store a matrix and cache its inverse.

## The following function creates a special "matrix" 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		n <- NULL
		set <- function(y) {
                x <<- y
                n <<- NULL
		}
        	get <- function() x
        	setInverse <- function(inverse) n <<- inverse
        	getInverse <- function() n
        	list(set = set,
			get = get,
             	setInverse = setInverse,
             	getInverse = getInverse)
}


## The next function computes the inverse of the matrix created
## by the "makeCacheMatrix" function. It would retrieve the inverse 
## from the cache if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		n <- x$getInverse()
     		if (!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        mat <- x$get()
        n <- solve(mat, ...)
        x$setInverse(n)
        n
}
