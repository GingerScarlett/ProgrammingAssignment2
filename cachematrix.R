## This pair of functions provide a service that can cache the inverse of a matrix,
## thereby saving processing time when repeated calls are made to retrieve the inverse
## 
## They assume that the matrix supplied to makeCacheMatrix() is invertible
##

## makeCacheMatrix is called to initialize the structure which is a list containing
## the following functions:
## set(y) - used to set the matrix values; expects an invertible matrix
## get() - returns the matrix
## setInv() - used to set inverse of the matrix
## getInv() - used to retrieve the inverse of the matrix
makeCacheMatrix <- function(x) {
	i <- NULL
	isinv <- function(m) class(try(solve(m),silent=T))=="matrix"
	set <- function(y) {

		if (!isinv(y)) {
			return ("Please input an invertible matrix")
		}
		else {
			x <<- y
			i <<- NULL
		}
	}
	get <- function() x
	setInv <- function(newi) i <<- newi
	getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve is used to either return the previously computed inverse of the "special" matrix, if
## it has been computed before or it computes and stores the inverse for later retrieval

cacheSolve <- function(x) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data)
	x$setInv(i)
	i
}
