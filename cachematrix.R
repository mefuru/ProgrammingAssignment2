## makeCacheMatrix returns a list that contains four functions
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y # Here we assign x to equal y, but in its parent environment
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve returns the inverse value from the matrix object passed via runn-
# ing the getinverse() function on the object. If this val is NULL, the function
# will calculate the inverse on the matrix values, update the list to contain
# this value and will return this value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting inverse matrix")
        return(inverse)
    }
    message("setting inverse matrix")
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
