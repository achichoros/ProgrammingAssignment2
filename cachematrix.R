## These two functions will turn use a matrix as input and create a special
## matrix that can cache it's inverse, and then cache or retrieve that inverse

## makeCacheMatrix turns the input matrix into the new special matrix object

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## Checks if the inverse of X has already been found.  If it has,
## then it retrieves it.  Otherwise it finds the inverse and caches it.

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
