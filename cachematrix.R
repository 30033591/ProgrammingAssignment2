## Put comments here that give an overall description of what your
## functions do
#  makeCacheMatrix: 
# 		this function creates a special matrix object that can cache its inverse
#  cacheSolve: 
# 		this function calculates the inverse of the object returned by makeCacheMatrix function if the inverse hasn't been calculated; otherwise it returns the inverse from the cache


## Write a short comment describing this function
# makeCacheMatrix contains four functions below
# 	set - set the value of the matrix, only square matrices are accepted, issues a warning for non-square matrices
# 	get - get the value of the matrix
# 	setinv - set the inverse of the matrix
# 	getinv - get the inverse of the matrix 
# it returns a list containing all above functions
makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		if (nrow(y)!=ncol(y)) 
			message("only square matrices are accepted")
		else
			x<<-y
			invx<<-NULL
	}
	get <- function()	x
	setinv <- function(ix)	invx<<-ix
	getinv <- function() invx
	list(set = set, get = get, 
		setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve calculates the inverse of matrix stored in the cache x
# for the first call or returns the inverse if it has already been calculated
#	 x must be an object returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
	# invx: inverse of the matrix stored in x$get() 
	invx <- x$getinv()
	# return invx if it was already computed
	if (!is.null(invx)) { 
		message("getting inverse matrix from cached data")
		return(invx)
	}
	mat <- x$get()
	# detval: the determinant value of matrix
	detval <- det(mat)
	# the matrix is not invertible if detval is not a number or zero 
	if (!is.na(detval) & detval!=0)
		invx <- solve(mat, ...)
	else 
		message("matrix not invertible. return null.")
	# set the invx value in cache structure and return it
	x$setinv(invx)
	invx
}
