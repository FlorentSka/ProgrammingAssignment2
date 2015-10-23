## Writing a function that caches the inverse of a Matrix
## Using such a function avoids computing the inverse
## every time its needed and gains calculation time

## the following function creates a list dedicated to store
## the inverse (an object for now not computed yet)

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
    x<<-y
    inv<<-NULL
  }
	get<-function() x
	setinverse <- function(inverse) inv<<-inverse
	getinverse <- function() inv
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Computes the inverse of a matrix that will be used by the 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
	inv<-x$getinverse()
	if(!is.null(inv)) {
	message("getting cached data of inverted matrix")
	return(inv)
	}
	data <- x$get()
	inv<-solve(data,...)
	x$setinverse(inv)
}