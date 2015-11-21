## Put comments here that give an overall description of what your
## functions do

## The two functions below are able to create a special 
## object that stores a matrix and cache's its inverse.

## Write a short comment describing this function

## The makeCacheMatrix function could generate a list of function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function

## The cacheSolve function calculates the inverse of matrix that generated
## by makeCacheMatrix function. It first checks whether the inverse
## has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Else, it calculates the inverse of the data 
## and sets the value of the inverse in the cache via the setinverse function.
## Assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
