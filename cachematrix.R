# The makeCacheMatrix function takes as input a matrix and, in addition to storing
# its value in the local environment, it returns a list of functions that lets the 
# user set a new matrix to the cache, return the current cached matrix, set its inverse manually,
# or retrieve its previously cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


# The cacheSolve function takes the return of makeCacheMatrix and computes the inverse
# of the cached matrix or, if the inverse is already cached, simply returns that value instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
