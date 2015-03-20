## makeCacheMatrix returns an object with a matrix and a cached inverse
##
## cacheSolve returns a cached inverse from object x or calculates the
## inverse and caches the value in x while returning it


## makeCacheMatrix returns an object with a matrix and a cached inverse

makeCacheMatrix <- function(x = matrix()) {
    matrix.inverse <- NULL
    set <- function(y) {
        x <<- y
        matrix.inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix.inverse <<- inverse
    getinverse <- function() matrix.inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cacheSolve returns a cached inverse from object x or calculates the
## inverse and caches it while returning it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' 
    
    matrix.inverse <- x$getinverse()
    if(!is.null(matrix.inverse)){
        message("getting cached inverse")
        return(matrix.inverse)
    }
    data <- x$get()
    matrix.inverse <- solve(data, ...)
    x$setinverse(matrix.inverse)
    matrix.inverse
}
