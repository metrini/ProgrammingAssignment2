## Functions that cache and compute the inverse of a matrix.

## This function creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    inverseM <- NULL	  # initialize to NULL
	# create the matrix in the working environment
    set <- function(y) {
        x <<- y
        inverseM <<- NULL
    }
    get <- function() return(x)	# get the value of the matrix
    setinv <- function(inv) inverseM <<- inv
    getinv <- function() return(inverseM)
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseM <- x$getinv()	## attempt to get the inverse of the matrix stored in cache
    # return inverted matrix from cache if it exists
    # else create the matrix in working environment
	if(!is.null(inverseM)) {
        message("Getting cached data!")
        return(inverseM)
    }
    matrix <- x$get()	# create matrix since it does not exist
    inverseM <- solve(matrix, ...)
    x$setinv(inverseM)	# set inverted matrix in cache
    return(inverseM)
}