## First, create an invertible matrix and call the makeCacheMatrix function
## The first time the makeCacheMatrix function is called, since the results haven't been cached,
## the inverse of the matrix will be computed and stored in cache memory
## Any subsequent calls to the makeCacheMatrix function from the cacheSolve function 
## will return the cached version of the inverted matrix saving computation time in the process

## The makeCacheMatrix function declares a number of getter and setter functions
## The functions are described in-line below

makeCacheMatrix <- function(x = matrix()) {
    
    # 1. Assign the matrix to ve inverted to the x cache memory variable and set s to NULL
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # 2. Get the cached value of x
    get <- function() x
    
    # 3. Sets the output for inverting matrix x to cached variable s
    setsolve <- function(solve) s <<- solve
    
    # 4. Gets the value of s which is either NULL or the cached value of the inverted matrix
    getsolve <- function() s
   
    # 5. Creates list containing "getter" and "setter" functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


# The cacheSolve function checks if the inverse of matrix x
# has been computed and stored in cache memory. If the inverse
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
