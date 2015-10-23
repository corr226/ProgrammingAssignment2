## This file contains two functions.  These functions pertain
## to assignment #3 of an R-Programming course.  The functions 
## are explained below but are intended to showcase the <<-
## operator
##


## This function, makeCacheMatrix, will take as its first
## 		argument, a matrix (assumed to be a square matrix).
##		It will create an object that holds:
##			1) The original matrix
##			2) A placeholder for the inverse of the original matrix
##			3) Getters and setters for the original matrix value; and
##				the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		# Create an object that contains a given matrix,
		# a placeholder for its inverse and the getters and 
		# setters to access these two items.

        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
 
		##  Return the object of the matix, inverse placeholder
		##		and getters and setters
		list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function, cacheSolve, will take as it first argument
##		an object created by the makeCacheMatrix method above.
##		It will inspect that object to see if the inverse of 
## 		the matrix has been computed.  If it has been computed, 
## 		the function will return that computation.  If it has 
## 		not been computed it will perform the computaion and 
##		set the result into the object so it will not have
## 		to be computed again when cacheSolve is called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        myMatrix <- x$get()
        i <- solve(myMatrix, ...)  # %*% myMatrix
        x$setinverse(i)

		## Return the matrix 
        i

}
