## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #sets empty cache of the matrix
        inverse <- NULL
        set_inv <- function(y){
                x <<- as.matrix(y)
                inverse <<- NULL
        }
        # gets cache of the matrix
        get <- function() x
        
        #sets inverse of the matrix
        setinverse <- function(inv) inverse <<- inv
        
        #gets inverse of the matrix
        getinverse <- function() inverse
        
        #lists the values of the matrix cache
        list(set_inv = set_inv, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #if inverse value is not null, return inverse stored in the cache
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("You have an inverse in the cache. Getting cached data...")
                return(inverse)
        }
        #else, calculate inverse and store in the cache
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
