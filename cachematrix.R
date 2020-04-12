## Create a makeCacheatrix function and a cacheSolve function to avoid computing the inverse of matrices more than once

## Create makeCacheMatrix which takes a matrix and returns a list of 4 functions: set, get, setSolve, getSolve.

makeCacheMatrix <- function(x = matrix()) {
	m<- NULL
	set<-function(y){
		x<<-y
		m<<- NULL
	}
	get <- function() x
	setSolve <- function(Solve) m<<-Solve
	getSolve <- function() m
	list(set=set, get=get, setSolve=setSolve, getSolve=getSolve) 
}

## Create cacheSolve which takes an object of the type makeCacheMatrix and returns the inverse of the matrix in it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getSolve() 
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setSolve(m)
     m
