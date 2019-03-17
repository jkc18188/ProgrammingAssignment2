# Matrix inversion is a costly computation and there is benefit to 
# caching the inverse of a matrix rather than compute it repeatedly
# (e.g. very large matrices)

# The makeCacheMatrix computes the inverse of a given matrix and 
# generates a list. Similar to the makeVector function the following
# describes the steps.

# 1. sets value of matrix
# 2. gets value of matrix
# 3. sets value of inverse of the matrix
# 4. gets value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The cacheSolve function returns the inverse of the matrix. The first step
# checks if the inverse has already been computed by the makeCacheMatrix
# function. If makeCacheMatrix was already executed, cacheSolve skips the
# calculation step and grabs the result from makeCacheMatrix. If not, 
# cacheSolve computes the inverse and reports the result

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # calculates the inverse of the matrix if it is not cached
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
