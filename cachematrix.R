## The functions makeCacheMatrix and cacheSolve are two functions
## that take any non-singular square matrix.

## The makeCacheMatrix creates a list of functions that set and
## get values of a matrix and its respective inverse.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) m <<- solve
     getinverse <- function() m
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix specified in the
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
           message("getting cached data")
           return(m)   
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
