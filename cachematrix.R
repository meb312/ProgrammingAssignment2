## These functions first produce a set of functions to be used as inputs into
## cacheSolve, which returns the inverse of input matrix if makeCacheMatrix has not
## been rerun, otherwise, it calculates a new inverse matrix for the new input matrix.

## makeCacheMatrix creates the functions used as inputs to cacheSolve

makeCacheMatrix <- function(x = matrix()) {

	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-	function()x
	setinverse<-function(solve)m<<-solve
	getinverse<-function()m
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve takes the objects from makeCacheMatrix as inputs, checks if m exists
## and is not NULL; if so, it simply returns the stored value of m(the inveset matrix).
## If m is NULL, meaning makeCacheMatrix has been re-run and m reset, then cacheSolve
## calculate the new inverse matrix for m.

cacheSolve <- function(x, ...) {
	
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
