## Functions to create a special matrix type which can cache it's inverse.
## Example usage:
##      m  <- matrix(c(1,3,3,1), nrow=2, ncol=2)
##      mc <- makeCacheMatrix(m)  # create the special matrix
##      mc$get()                  # get the matrix
##      mi = cacheSolve(mc)       # get the inverse
##      print(mc$get() %*% mi)    # returns the identity matrix
##      mi2 = cacheSolve(mc)      # get the inverse "cheaply", 
##                                # using already cached value

## makeCacheMatrix() creates a special matrix using list of four functions
##                   to set and get the matrix itself (get(), set())
##                   and to set and get it's inverse (setinv(), getinv())

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(invm) i <<- invm
    getinv <- function() i
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() returns the inverse matrix of special matrix created 
##      with makeCacheMatrix(). The inverse is computed only on the 
##      first call and cached. Cached value is used on consecutive calls.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
    


