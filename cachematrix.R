## These functions calculate and cache the inverse of a matrix

## Usage:
##      load data   a<-makeCacheMatrix(matrix(1:4,2,2))
##      show data   a$get()
##      get or calculate inverse:
##         cacheSolve(a)

## Takes a square invertible matrix and provides setters and
## getters for the data (in matrix form). Also provides setters
## and getters for the the calculated inverse data (in matrix form)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(x) {
                i<<-solve(x)
        } 
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Takes the matrix from the above function. If the inverse
## has already been calculated then return the data, If the 
## inverse doesn't exist then calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
