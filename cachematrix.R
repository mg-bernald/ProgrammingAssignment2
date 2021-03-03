## There are 2 main functions: makeCacheMatrix and cacheSolve

## makeCacheMatrix creates a special "matrix" that is able to cache is inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y       ##sets the value of x in the parent env.
        inv <<- NULL
    }
    get <- function() {x} ##gets value of matrix
    setInverse <- function(inverse) inv <<- inverse ##assigns value of inv in parent env.
    getInverse <- function() inv ##gets the value of inv when called
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    ##list needed in order for $ operator in cacheSolve function to work properly
}

## cacheSolve computes the inverse of the special matrix created by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
           inv <- x$getInverse()
    if(!is.null(inv)){   ##verifies if inv is NULL
        message("getting chached data")
        return(inv)      ##returns inverse value
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv   ## Return a matrix that is the inverse of 'x'
}
