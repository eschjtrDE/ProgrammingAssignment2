## The following functions enable a user to cache the inverse of a matrix
## after the first calculation and re-use the value. 


## This function creates a wrapper, that allows a user to store
## a cached iverse value for a matrix. It provides four functions:
## get, set, getinv, and setinv
makeCacheMatrix <- function(x = matrix()) {
    
        inv <- NULL
        
        # set the matrix (y should be a matrix)
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        
        # get the matrix
        get <- function() x
        
        # set the inverse (cache)
        setinv <- function(inverse) inv <<- inverse
        
        # get the inverse
        getinv <- function() inv
        
        # list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## This function returns a matrix that is the inverse of 'x'
## If possible, a cached value will be returned.
## 'x' should be from th form returned by 'makeCacheMatrix'
cacheSolve <- function(x, ...) {
    
        ## check for existing chached value
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        ## calculate the inverse and cache the result
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
