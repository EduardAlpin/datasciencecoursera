## The following 2 functions work together to set values of a 
## matrix, store the inverse in a cache and retrieve it, thus 
## minimizing computational time when the inverse needs to be
## retrieved multiple times.

## This function creates a list of 4 functions that do perform 
## the following:
## set: sets the values of matrix
## get: gets the values of matrix
## setinv: sets the values of the inverse of the matrix
## getinv: gets the values of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function calculates the inverse of the matrix 
## from the function above; it first checks to see if the inverse
## has already been stored. If so it gets the inverse from the
## cache and skips the calculation. If not, it calculates the 
## inverse and stores it via the setinv function.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
