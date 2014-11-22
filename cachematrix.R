# Cache for the Inverse of a Matrix

# The following two functions implement a cache for the inverse of a matrix
# such that for a given matrix the inverse has to be calculated only once.
#
# The matrix to be inverted is represented by a list object that the
# makeCacheMatrix() function creates from an R matrix. The inverse for that
# "matrix" object can then be obtained with cacheSolve().

# makeCacheMatrix()
# Creates a "matrix" object represented by a list of getter and setter
# functions to the actual R matrix object and its inverse. The latter are
# actually stored in the environment of the getter and setter functions
# defined by makeCacheMatrix.
# The matrix data may be supplied on creation of the "matrix" object or
# by calling the set() function. 
# The R matrix object may be obtained with get().
# Likewise, setsolution() and getsolution() operate on the stored inverse.
# The last two functions should, however, only be used by way of cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() return(x)
    setsolution <- function(solution) s <<- solution
    getsolution <- function() return(s)
    return(list(set = set, get = get,
                getsolution = getsolution,
                setsolution = setsolution))
}

# cacheSolve()
# Obtains the inverse of the given "matrix" object x. If that inverse has not
# yet been determined and stored in the cache, it is calculated by way of the
# R solve function. Otherwise, it is returned immediately from the cache.
#
# NB Any further arguments to cacheSolve are passed to R's solve function.
# These extra argumnets are not taken into account for the cache. Hence,
# cached results may be inaccurate if cacheSolve() is called with different
# arguments than on the first call.
cacheSolve <- function(x, ...) {
    s <- x$getsolution()
    if (is.null(s)) {
        a <- x$get()
        s <- solve(a, ...)
        x$setsolution(s)
    } else {
        message("getting cached data")
    }
    return(s)
}
