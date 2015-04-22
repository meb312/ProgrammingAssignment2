## The 1st function builds the script for the 2nd function.
## The 2nd function, cacheSolve, returns the inverse matrix of the square matrix input to 
## makeCacheMatrix.  If makeCacheMatrix has not been rerun since cacheSolve last ran, then
## the inverse matrix is returned from the cache without being calculated.  If
## makeCacheMatrix HAS been rerun (even with the same input matrix) since the last time
## cacheSolve was run, then cacheSolve will re-calculate the inverse matrix from scratch.

## makeCacheMatrix creates the functions used as inputs to cacheSolve

makeCacheMatrix <- function(x = matrix()) {

# Reset m to NULL each time makeCacheMatrix is called
	m<-NULL 

# Define set function to reset the value of x and reset m to null 
	set<-function(y){  
		x<<-y		 
		m<<-NULL
	}

# Define get function to return x 
	get<-	function()x 

# Define setinverse function to return the inverse of m using the solve function
	setinverse<-function(solve)m<<-solve

# Define getinverse function to return m 
	getinverse<-function()m

# Create list object of above functions to pass to cacheSolve
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}



## cacheSolve takes the objects from makeCacheMatrix as inputs, checks if m exists
## and is not NULL; if so, it returns the value stored in m(the inverse matrix).
## If m is NULL, meaning makeCacheMatrix has been re-run and m reset, then cacheSolve
## calculatse the new inverse matrix for m

cacheSolve <- function(x, ...) {
	
# Recalls stored value of getinverse and assigns to m
        m <- x$getinverse()

# Print m to verify whether or not m is NULL
		print(m)

# If m is non-null, return the stored value of m without recalculating and move
# to the end of the function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
	  }

# If m is null, set the data variable to the variable x stored above.
        data <- x$get()

# Recalculate the inverse matrix with the data variable (x) and store in m
        m <- solve(data, ...)

# Store the value of m in the setinverse variable
        x$setinverse(m)

# Print the result!
        m

}
