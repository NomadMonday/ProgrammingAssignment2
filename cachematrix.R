#makeCacheMatrix and cacheSolve are functions used in tandem in order to create a
#cached version of a matrix's inverse, and reference that cached version
#instead of recalculating it.

#Creates a cacheable "matrix", which is really a list containing a function to
#get/set the matrix
#get/set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}


#Runs the solve function on the cacheable "matrix", but first checks if a cached version exists.
cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
