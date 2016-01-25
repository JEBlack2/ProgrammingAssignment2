##
## Programming Assignment 2: Lexical Scoping 
## setwd("C:/JEB/educat/2015.DataScience/2016.01.RProgramming/2016.01.RProgramming.Week03")
## 
## Assignment: Caching the Inverse of a Matrixless 
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. 
##
## Write a pair of functions that cache the inverse of a matrix.
## 
## 1.	makeCacheMatrix: 
## 	Creates a special "matrix" object that can cache its inverse.
##
## 2.	cacheSolve: 
## 	Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## 	If the inverse has already been calculated (and the matrix has not changed), 
## 	then the cachesolve retrieves the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. 
## (e.g., if X is a square invertible matrix, then solve(X) returns its inverse)
## For this assignment, assume that the matrix supplied is always invertible.
## Note that style is a combination from lisp, smallTalk, and Python (and good old assembly language)

## makeCacheMatrix: Given an invertible matrix, create a "special matrix" that can cache its inverse.
## Input: "x" - an invertable square matrix (note we do not validate invertability; we just trust:-)
## Return: a list (used as input to "cacheSolve" function) containing the functions:
## 	getMatrix: to fetch the matrix "x" stored in the "special" object
## 	setMatrix: to change (or "set") the matrix stored in the "special" object
## 	getInverse: to fetch the cached inverse of the matrix "x"
## 	setInverse: to change (or "set")the cached inverse of the matrix "x"
##

makeCacheMatrix <- function(x = matrix()) { 	# (really should check invertibility of "x")
	cachedInverse <- NULL			            # start out with nothing

	getMatrix <- function() x 		            # return the matrix
	setMatrix <- function(newMatrix) {	        # re-set the matrix
		x <<- newMatrix			                # in parent environment
		cachedInverse <<- NULL }                # matrix changed; zap inverse

	getInverse <- function() cachedInverse     	# return the inverse (or NULL)
	setInverse <- function(inverse) { 	        # re-set (cache) the inverse 
		cachedInverse <<- inverse } 	        # save the inverse (in parent environment)

	list( 					                    # coerce the return to be a list
		getMatrix  = getMatrix,
		setMatrix  = setMatrix, 
		getInverse = getInverse,
		setInverse = setInverse
		)
}

## cacheSolve: Compute the inverse of the invertible square matrix passed to "makeCacheMatrix"
## Input: the special "matrix" (list object) returned by "makeCacheMatrix(x)"
## Return: the inverse of the invertible square matrix passed to "makeCacheMatrix"
## 	If the inverse has already been computed, and the matrix is unchanged, 
## 	then cacheSolve returns the previously computed inverse held in cache;
## 	otherwise, the inverse computed by the "solve" function is cached and returned.
##

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()			            # fetch the inverse (or not)

	if (!is.null(inverse)) {		                # if already computed and cached
		message("cacheSolve: returning cached inverse")
		return(inverse)			                    # skip computation and return cached inverse
	    } else { 				                    # otherwise, we have to compute it
        message("cacheSolve: inverting matrix")
	    invertibleMatrix <- x$getMatrix()           # fetch the original matrix
	    inverse <- solve(invertibleMatrix, ...)     # compute the inverse
	    x$setInverse(inverse) 		                # save it in the cache
	    return (inverse)		                    # return the new inverse
	    }
	stop("cacheSolve: Should not be here") 	    # oh, no! how'd we get here?
}

## ==================================================================

testAssignment2 <- function(iMatrix) {
	mList <- makeCacheMatrix(iMatrix)   	      # create "special" object

	time1 <- Sys.time()			                    # round 1
	cacheSolve(mList)
	time2 <- Sys.time()
	print(c("Duration 1:", time2-time1))

	time1 <- Sys.time()			                    # round 2
	cacheSolve(mList)
	time2 <- Sys.time()
	print(c("Duration 2:", time2-time1))

	mList$setMatrix( mList$getInverse() ) 	   # flip it around

	time1 <- Sys.time()			                   # round 3
	cacheSolve(mList)
	time2 <- Sys.time()
	print(c("Duration 3:", time2-time1))

	time1 <- Sys.time()			                   # round 4
	cacheSolve(mList)
	time2 <- Sys.time()
	print(c("Duration 4:", time2-time1))

	}
	