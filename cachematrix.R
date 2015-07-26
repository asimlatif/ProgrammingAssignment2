## Function makeCacheMatrix creates and returns a list
## with variables set, get, setinverse & getinverse
## This list is then passed to cacheSolve function to compute
## the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {

   m            <- NULL

   set <- function (y) {
      x            <<- y
      m            <<- NULL
   }
   get <- function() x
   setinverse <- function(inv) m <<- inv
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
	getinverse = getinverse)
}


## cacheSolve function computes the inverse of matrix x. If x inverse
## has already been calculated and the matrix has not been changed, 
## cached value of inverse matrix is returned

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        
	if (!is.null(m)) {
	   message("getting cached data")
	   return(m)
	}
	data <- x$get()
	m    <- solve(data)
	x$setinverse(m)
	m
}
