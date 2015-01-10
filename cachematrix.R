## The two functions together find the inverse of a given matrix, assuming that the matrix is invertible. 
## If the given matrix has an inverse recorded in the cache, the cacheSolve  

# makeCacheMatrix: create a matrix, containing the functions to set and get the value of the matrix,
# and also set and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
	
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get, 
	     setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: Return a matrix that is the inverse of 'x'.
## Read the inverse of given matrix if it is already in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
