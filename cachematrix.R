## The function makeCacheMatrix receives a matrix and creates a list
## that contains functions that set and get the value of the matrix
## and functions that set and get the inverse of the given matrix
## Its is also assumed that the given matrix is always invertible


makeCacheMatrix <- function(x = matrix()) {
    inv <<- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve gets the list made by the makeCacheMatrix function
## and it checks if the inverse is already calculated. If so, it gets the inverse
## from the cache and skips the computation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
