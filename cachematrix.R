## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
                getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachsolve should retrieve the inverse 
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
