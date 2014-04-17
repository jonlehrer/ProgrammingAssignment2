## These functions create a "CacheMatrix" object which can cache its
## inverse so that it does not need to be recomputed each time it is needed



## makeCacheMatrix creates a special "matrix" which is really a list 
## containing functions to set the value of the matrix,
## get the value of the matrix, set the value of the inverse,
## and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## cacheSolve takes a special 'matrix' created with MakeCacheMatrix
## and checks to see if the inverse has been calculated. If it has
## it returns that index. If not, it computes and stores the inverse
## via the setinverse function

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
