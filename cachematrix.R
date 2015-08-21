## Jeffrey Gardner

## These functions are useful for cacheing data in order to avoid 
## strenuous computations by cacheing a result once then reusing it. 

## makeCacheMatrix: Returns a list of functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)
}

## cacheSolve: Calculates the inverse of the matrix created above. 
## If the inverse has already been calculated,  
## then cacheSolve retrieves the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}

## Test examples:
x <- matrix( c(5, 1, 1, 5),
		nrow=2,
		ncol=2,
		byrow = TRUE)
cx <- makeCacheMatrix(x)
cacheSolve(cx)			# Returns the inverse of the matrix
cacheSolve(cx)			# Returns the cached inverse of the matrix
