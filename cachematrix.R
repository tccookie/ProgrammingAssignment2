## This pair of functions creates a matrix and calculates
## its inverse. The inverse is cached and subsequently retrieved
## rather than being re-calculated until the original matrix
## is modified, at which point it needs to be recalculated and cached again.

## This function creates a list of functions that hold the information
## about the inputted matrix and allow it to be manipulated, including
## by the subsequent function that will use setinverse() to store its
## result.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the function created using the previous
## function or retrieves the cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
