# Convert a numeric matrix into a list of 4 functions
# set: assign values to the matrix
# get: recall values within the matrix
# setSolve: define the inverse to the matrix
# getSolve: recall the inverse of the matrix
makeCacheMatrix <- function(x = numeric())
{
  # initially no inverse exists; set to NULL
	inv <- NULL
	# Define function "set" to set x to new value y.  
  # Reset inverse to NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
  # Define function "get" to get value of matrix
	get <- function() x
  # Define function "setSolve" to define inverse
	setSolve <- function(inverse) inv <<- inverse
  # Define function "getSolve" to recall computed inverse
	getSolve <- function() inv
  # Return list of 4 functions
	list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}
	
# Return matrix inverse
cacheSolve <- function(x, ...) {
    #Check if inverse is already computed
    inv <- x$getSolve()
    if(!is.null(inv)) {
            # If inv is not NULL, then there's an inverse
            message("getting cached data")
            return(inv)
    }
    # Otherwise, compute matrix inverse
    # First get the matrix
    data <- x$get()
    # Then compute the inverse
    inv <- solve(data)
    # Update the inverse in the list
    x$setSolve(inv)
    # Return result
    inv
}
