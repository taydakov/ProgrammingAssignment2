## Matrix inversion is usually a costly computation and there's a good opportunity to use caching
## for recurring calculations of matrix inversion.
## There are two functions in this file. One is for make caching easy and another for
## actual matrix inversion calculation


## Makes caching of matrix inversion easy to use. Returns a list of function for control access
## to data and matrix inversion and return cached value.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Uses cached value if possible.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}