## These functions enable efficient computations of matrix inverses. MakeCacheMatrix
## returns a special cacheMatrix object, providing functionality to cache the 
## matrix inverse. CacheSolve first checks whether a matrix inverse is already
## cached and returns it, otherwise it calculates it. 


## This function returns a cacheMatrix object, enabling the user to store a once 
## calculated matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function returns the inverse of the parameter matrix 'x'. If the inverse
## has already been calculated, it returns the cached matrix, otherwise it 
## calculates it and stores it within the "cacheMatrix" object.

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
