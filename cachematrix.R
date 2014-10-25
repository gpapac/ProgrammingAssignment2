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
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse of the matrix
        setinverse <- function(inverseMtx) inv <<- inverseMtx
        ## get the value of the inverse of the matrix
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
        ## If the getinverse function of the special matrix object returns a value (not null),
        ## this is the cached inverse matrix and it is returned as a result
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Otherwise the matrix is retrieved from the special matrix object (by the function get)
        dataMatrix <- x$get()
        ## the inverse of the matrix is computed using the function solve
        inv <- solve(dataMatrix, ...)
        ## the inverse is set in the special matrix object in order to be cached
        x$setinverse(inv)
        ## the inverse is returned as a result
        inv        
}
