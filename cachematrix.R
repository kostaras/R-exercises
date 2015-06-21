# makeCacheMatrix creates a list of functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve function returns the inverse of the matrix.
# 1. checks if the inverse has been computed. 
#    a. if Yes, it gets the precomputed result and returns.
#    b. else, it computes the inverse using solve() function
#                and sets the value in cache via setinverse
#                function. 
# cacheSolve assumes that the matrix is always invertible, therefore no
# additional checks for this purpose will be made.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    
    if(!is.null(inverse)) {
        message("getting cached data.")
        return(inverse)
    }
    
    data    <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
