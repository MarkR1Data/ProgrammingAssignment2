##################################################################################
#
# The functions in this file will cache the inverse of a matrix.
# Caching the matrix inverse will be more efficient than calculating it
# each time it is needed.
# Coursera R Programming Course with Johns Hopkins
#
################################################################################

#===============================================================================
#
# FUNCTION : makeCaheMtrix()
#
# This function creates a list and provides accessor mehthods as follows :
# set() : set the value of the matrix
# get() : get the value of the matrix
# setInverse() : set the value of the matrix inverse
# getInverse() : get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


#===============================================================================
#
# FUNCTION : cacheSolve()
#
# This function will return the inverse of a mtrix. It generates the inverse
# using solve(). It tests if the inverse has already been calculated
# and uses it if so. Else it will calculate the inverse and cache it by
# calling the setInverse() method.
#

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
