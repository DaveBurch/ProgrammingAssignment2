# This file contains code that will create a "special" matrix object
# that can cache its inverse (the makeCacheMatrix function) and that
# will compute the inverse of the matrix returned by makeCacheMatrix
# (the cacheSolve function). cacheSolve only computes the inverse
# matrix a single time - if it already exists, the cached matrix
# is returned.

# The makeCacheMatrix function will:
#      1. Set the matrix
#      2. Get the matrix
#      3. Set the inverse of the matrix
#      4. Get the inverse of the matrix
#
# The function returns a list of the above objects.

makeCacheMatrix <- function(x = matrix()) {

    # x is required to be a square, invertible matrix for this
    # function to perform properly.
    
    # initialize the inverse matrix as NULL:
    
    inv <- NULL
    
    # Define a function that will assign a value for the input matrix
    # in the global environment (an environment different than the current
    # environment) and a flag indicating the inverse has not been
    # computed (i.e., the NULL value is assigned to inv):
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # Define a function to obtain the matrix from the global environment:
    
    get <- function() x
    
    # Define a function to assign a value for the inverse matrix in
    # the global environment:
    
    setinv <- function(inverse) inv <<- inverse
    
    # Define a function to obtain the inverse matrix
    # from the global environment:
    
    getinv <- function() inv
    
    # Return a list referring to the above functions:
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The cacheSolve function will return the value of an inverse matrix
# if it was previously computed and cached, or if not, compute the
# inverse and cache it.

cacheSolve <- function(cx, ...) {
    
    # cx is an object resulting from application of makeCacheMatrix().
    
    # Obtain the value of the inverse matrix from the global environment:
    
    inv <- cx$getinv()

    # If the inverse was previously determined, return the cached value:
    
    if (!is.null(inv)){
        message("returning previously cached data")
        return(inv)
    }
    
    # Since execution fell through from the "is" statement, the inverse
    # has not been previously computed. Do the computation and return the
    # result. Since cacheSolve only acts on objects created using
    # makeCacheMatrix, it is not necessary to test whether the input
    # matrix has changed (this would have required operation of
    # makeCacheMatrix, which would have resulted in setting of inv to NULL).
    
    message("computing, caching and returning inverse")
    mdata <- cx$get()
    inv <- solve(mdata, ...)
    cx$setinv(inv)
    return(inv)
}