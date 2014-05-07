## Matrix inversion is usually a costly computation and there may be 
##   some benefit to caching the inverse of a matrix rather than 
##   compute it repeatedly 
## Therefore, this project is to write a pair of functions 
##   that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object 
##   that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        v <- NULL
        set <- function(y) {
                x <<- y
                v <<- NULL
        }
        get <- function() x
        setinv <- function(inv) v <<- inv
        getinv <- function() v
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The function cacheSolve computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        v <- x$getinv()
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        data <- x$get()
        v <- solve(data, ...)
        x$setinv(v)
        v
}
