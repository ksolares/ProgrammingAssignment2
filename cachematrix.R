## Functions to cache the inverse of a matrix

## Creates matrix object that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
  }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                 message("getting cached data")
                 return(i)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
