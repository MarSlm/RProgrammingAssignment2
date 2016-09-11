# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	invX <- NULL
	set <- function(y){
		x <<- y
		invX <<- NULL
	}
	get <-function() x 
	setInverse <- function(invMatrix) invX <<- invMatrix
	getInverse <- function() invX
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invX <- x$getInverse()
	if(!is.null(invX)){
		message("getting cached data")
		return(invX)
	}
	data <- x$get()
	invX <- solve(data, ...)
	x$setInverse(invX)
	invX
}
