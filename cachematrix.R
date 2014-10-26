## To test the functions below, first create an invertible matrix and call the makeCacheMatrix function
## The first time the makeCacheMatrix function is called, since the results haven't yet been cached,
## the inverse of the matrix will be computed and stored in cache memory
## Any subsequent calls to the makeCacheMatrix function from the cacheSolve function 
## will return the cached version of the inverted matrix saving computation time in the process



makeCacheMatrix <- function(x = matrix()) {
    
    # Assign the matrix to ve inverted to the x cache memory variable and set s to NULL
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Get the cached value of x
    get <- function() x
    
    # Set the inverted matrix output for cached variable x to cached variable s
    setsolve <- function(solve) s <<- solve
    
    # Get the inverted matrix stored in cached variable s
    getsolve <- function() s
   
    # Create list containing "getter" and "setter" functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


# The cacheSolve function checks if the inverse of matrix x
# has been computed and stored in cache memory. If the inverse has already
# has not yet been computed, it sets the value in cache via
# setsolve function.  If the inverse for x already exists in memory
# then it shows the message "getting cached data" and returns the 
# cached inverted matrix x

cacheSolve <- function(x, ...) {

    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setsolve(s)
    s
    
}
