## Creating two functions that together allow us to calculate the inverse of matrix only once, even if we need it several times.

## makeCacheMatrix is a function that takes a matrix in and returns a list fo four functions: set, get, setSolve, getSolve.

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

## cacheSolve first checks if the inverse has been already calculated. If not, it calculates it for the first time.

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

