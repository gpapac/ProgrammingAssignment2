## The following pair of functions compute and cache the inverse of a matrix
## in a special "matrix" object.
## Example of use:
##    mcdat<-makeCacheMatrix(matrix(c(2, 1, 5, 3), nrow=2, ncol=2))
##    cacheSolve(mcdat)   ## First time we call it, it computes the inverse
##    cacheSolve(mcdat)   ## If we call it again, it retrieves the cached inverse

## The function `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMtx) inv <<- inverseMtx
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function `cacheSolve` computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        dataMatrix <- x$get()
        inv <- solve(dataMatrix, ...)
        x$setinverse(inv)
        inv        
}
