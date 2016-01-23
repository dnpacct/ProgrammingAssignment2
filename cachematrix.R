## Before you can get an inverse matrix you should call the function 
## makeCacheMatrix that create an object for storing the inverse 
## matrix in the cache. The inverse matrix can be obtained by calling
## the function cacheSolve

## makeCacheMatrix returns a matrix 'object' which is really the list 
## of the following functions:
## 1) set - the function set the original matrix to inverse later;
## 2) get - the function return the original matrix saved on set call;
## 3) setInv - the function saves the inverse matrix in the cache;
## 4) getInv - the function returns the previously cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(invMatrix) inv <<- invMatrix
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve manipulates a special vector returned by makeCacheMatrix
## in order to provide a version of the solve R function that caches 
## the inverse matrix instead of recalculating it at any call.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInv(inv)
	inv
}
