## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will cache a matrix
## cacheSolve will return the inverse of the cached invertible matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set x to be a square, invertible matrix
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## CacheSolve will return an inverted matrix

cacheSolve <- function(x = matrix( , nrow=2, ncol=2), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x["getinv"]
        if(!is.null(inv)) {
                ## if the matrix is invertible, it will return the inverse
                message("getting cached data")
                return(solve(x))
        }
        matrix.data <- x$get()
        inv <- Solve(matrix.data, ...)
        x$setinv(inv)
        inv
}
