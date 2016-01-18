## Below is a pair of functions that cache the inverse of a matrix

## this function creates a special "matrix" object that can cache its inverse
## result is comprised of a list of functions

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


## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## if the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.
## Note: matrix is assumed to be invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i

}
