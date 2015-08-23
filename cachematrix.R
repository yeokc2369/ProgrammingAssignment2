#
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following two functions are used to cache the inverse of a matrix.
#
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve returns the inverse of the matrix. 
# It first checks whether the inverse has already been computed. 
# If yes, it gets the result and skips the computation. 
# If no, it computes the inverse, sets the value in the cache via setinverse function.
#
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data from system:")
        return(inv)
    }
    else {
        message("First run:")
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Sample run:
#
# > x <- rbind(c(1, 2), c(3, 7))
# > m <- makeCacheMatrix(x)
# > m$get()
#       [,1]  [,2]
# [1,]     1     3
# [2,]     2     7
#
# When we run the solver the first time:
#
# > cacheSolve(m)
# First run:
#       [,1]  [,2]
# [1,]     7    -3
# [2,]    -2     1
#
# When we run the solver the second time:
#
# > cacheSolve(m)
# Getting cached data from system:
#       [,1]  [,2]
# [1,]     7    -3
# [2,]    -2     1
#  