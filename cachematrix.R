# This function creates a special matrix object that can cache its inverse (assuming the matrix supplied is invertible).

makeCacheMatrix <- function(x = matrix()) {.
    i <- NULL                                   # Initialize 'i' variable to NULL so the function can run the first time through.
    set <- function(y) {                        # Function 'set' reads in argument passed to makeCacheMatrix.
        x <<- y                                 # Set 'x' for the parent (set) environment equal to 'y'.
        i <<- NULL                              # Set 'i' for the parent (makeCacheMatrix) environment equal to NULL.
    }
    get <- function() x                         # Function 'get' assigns the matrix 'x' to it.
    setinv <- function(inverse) i <<- inverse   # Function 'setinv' takes a value and sets it to the value of 'i' in the.
    getinv <- function() i                      # makeCacheMatrix environment. Function 'getinv' returns the value of 'i'.
    list(set = set, get = get,                  # List the values of the functions in makeCacheMatrix.
    setinv = setinv,
    getinv = getinv)
}

# This function computes the inverse of the special matrix returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse
# from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()                             # Assign 'i' in this environment, the value of 'i' from the 'x' environment.
    if(!is.null(i)) {                           # If the 'x' environment has been evaluated before, print the message and
        message("getting cached data")          # value of 'i' (the cached inverse).
        return(i)
    }
    data <- x$get()                             # If the 'x' environment has never been evaluated, pull the x matrix into a local
    i <- solve(data, ...)                       # variable ('data'). Calculate the inverse of the matrix 'x' by calling the 'solve'
    x$setinv(i)                                 # function. Assign the calculated inverse to the 'x' environment using 'setmean'
    i                                           # Print the calculated inverse.
}
