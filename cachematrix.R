## makeCacheMatrix makes a special "matrix" object that can cache its inverse, 
## and cacheSolve computes the inverse of the "matrix"

## makeCacheMatrix is a function that creates a special "matrix" object that
## can cache its inverse by setting the value of the matrix, getting the value
## of the matrix, setting the value of the inverse, then getting the value
## of the inverse.

makeCacheMatrix <- function(x = matrix()) {
                mat <- NULL
                set <- function(a) {
                        x <<- a
                        mat <<- NULL
                }
                get <- function() x
                setinverse<- function(inverse) mat <<- inverse
                getinverse <- function() mat
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        
## cacheSolve computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function. If inverse has already been calculated, then the
## cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached matrix")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
