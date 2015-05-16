## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of functions that allow caching
## inverse of an invertible matrix along with the original matrix

makeCacheMatrix <- function(x = matrix()) {
	#initialize i to store inverse 
	i <- NULL
	
	#set function to change matrix and clear inverse
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	#get to return original matrix
	get <- function() x

	#set function to change inverse
	setinverse <- function(inverse) {
		i <<- inverse
	}
	
	#get to return inverse 
	getinverse <- function() i

	#makeCacheMatrix returns a list of functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve takes "makeCacheMatrix" and returns the inverse
## inverse is first looked up in cache, and if not found
## calculated using solve, and saved to cache of 
## "makeCacheMatrix" object supplied

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}
