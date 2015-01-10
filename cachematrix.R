## The two functions together find the inverse of a given matrix, assuming that the matrix is invertible. 
## If the given matrix has an inverse recorded in the cache, the cacheSolve  

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	# create a matrix, containing the functions to set and get the value of the matrix;
	# and also set and get the value of the inverse
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if (!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m

}
