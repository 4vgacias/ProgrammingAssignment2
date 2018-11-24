## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
 	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get  <- function() x 
	setInv <- function(Inv) i <<- Inv
	getInv <- function() i
	list(set = set, get = get, 
	     setInv = setInv, 
	     getInv = getInv) 
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)

	}
	data <- x$get()
        i <- solve(data, ...)
        x$setInv(i)
	i 

}
