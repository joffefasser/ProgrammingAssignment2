# Matrix inversion is usually a costly computation and there may be some benefit to caching the
# inverse of a matrix rather than computing it repeatedly. 
# The purpose of the functions defined in this file is to provide such caching support for working 
# with matrix inversion computations.

#' This function creates a poor man’s cached matrix object by building a number of functions used for 
#' managing the user provided matrix structure along with a closure used for sharing object state 
#' among the functions. The object state contains the user provided matrix structure including the 
#' inverse calculation of this structure. 
#' @param x A square matrix. Is required to be invertible.
#' @return A list of functions sharing state by means of a closure.
#' @examples
#' makeCacheMatrix(matrix())
#' makeCacheMatrix()
#' a <- makeCacheMatrix(matrix(ncol=3, nrow=3))
makeCacheMatrix <- function(x = matrix()) {
	# Used for matrix structure sanity validation
        validatestructure <- function(m) {
                if(!is(m,"matrix")) 
			stop("Provided data structure is not a Matrix class ")

                if(nrow(m) != ncol(m)) 
			stop("Provided data structure is not a square Matrix")
  	}

	# Override existing cached matrix and inverse matrix objects 
        set <- function(new_m) {
		validatestructure(new_m)
                x <<- new_m
                inv_m <<- NULL
        }
        
	# Return currently cached matrix object
        get <- function() {
		x
        }
        
	# Return currently cached inverse matrix object
        getinverse <- function(...) {
		if(is.null(inv_m)) {
			inv_m <<- solve(x, ...)                	
		}
                inv_m
        }

	# Build initial closure state by validating and caching matrix objects as needed.
	# Acording to normal OO pratice this constructure like method should be coded in a fail-safe
	# manner hence we do not calculate and cache the inverse structure of the provided matrix.
	# The inverse calculation is thuse handled by the get function.
	validatestructure(x)
        inv_m <- NULL
        
	# Return the list of functions used for working whit the initialized closure
        list(set = set, get = get, getinverse = getinverse)
}


#' This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
#' should retrieve the inverse from the cache.
#' The function assumes that the matrix supplied is invertible
#' @param x A square matrix. Is required to be invertible.
#' @return A list of functions sharing state by means of a closure.
#' @examples
#' makeCacheMatrix(matrix())
#' makeCacheMatrix()
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
	if(!identical(names(x),c("set", "get", "getinverse"))) 
		stop("Provided list is not created by the makeCacheMatrix function")

	# Get the inverse matrix
	m <- x$getinverse(...)

	# Return result
	m
}



# Unit-test like function. 
# Only to be used for testing purposes.
test.cacheSolve <- function() {
	# Setup test data structures
	testMatrix <- matrix(rnorm(1000000), 1000)	
	testClosure <- makeCacheMatrix(testMatrix)

	# Execute tests
	message(sprintf("Run #%i: tc1 is %f, tc2 is %f", 0, 
		system.time(cacheSolve(testClosure))["elapsed"], 
		system.time(cacheSolve(testClosure))["elapsed"]))

	# Clear caches and rexecute tests
	for (i in 1:10) {
		testClosure$set(testMatrix)
		message(sprintf("Run #%i: tc1 is %f, tc2 is %f", i, 
			system.time(cacheSolve(testClosure))["elapsed"], 
			system.time(cacheSolve(testClosure))["elapsed"]))
	}
}


