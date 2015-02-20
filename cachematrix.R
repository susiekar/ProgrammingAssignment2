# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## Creates a list of functions that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
get <- function() x
setmatrix <- function(solve) i <<- solve
getmatrix <- function() i
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)

}


## Computes the inverse of the matrix returned by makeCacheMatrix(), 
## unless the inverse has already been calculated, in which case
## it retrieves it from the cache.

## The function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getmatrix()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmatrix(i)
        i
}


