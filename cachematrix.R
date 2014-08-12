## Function to cache the inverse of a matrix

## This first function creates a special matrix ojbect that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix  
  
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse
    setinverse <- function(solve) i <<- solve
    
    ## get the value of the matrix
    getinverse <- function() i
    list(set =  set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## This second function gets the inverse created in the makecachematrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## check if the inverse of 'x' already exist, if so get from cache, otherwise calculate
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    
    ## set the inverse for the matrix in the cache via the setinverse function
    x$setinverse(i)
    i

}
