## Put comments here that give an overall description of what your
## functions do
## Testing lexical scope
## makeCacheMatrix works as a class to store a matrix & its inverse
## cacheSolve calculates the inverse of a matrix if it hasn't been 
##  calculated already

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mtrx_i <- NULL
    set <- function(y) {
        x <<- y
        mtrx_i <<- NULL
    }
    get <- function() x
    setinverse <- function(mtrx_inverse) mtrx_i <<- mtrx_inverse
    getinverse <- function() mtrx_i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtrx_inverse <- x$getinverse()
    if(!is.null(mtrx_inverse)) {
        message("getting cached data")
        return(mtrx_inverse)
    }
    data <- x$get()
    mtrx_inverse <- solve(data, ...)
    x$setinverse(mtrx_inverse)
    mtrx_inverse    
}
