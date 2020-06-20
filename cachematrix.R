## This is an assignment of the course Rprogramming

## This function takes a matrix and returns a list with four functions, 
## set, get, setSolve, getInv

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) mInverse <- solve
        getInv <- function() mInverse
        list(set = set, get = get,
             setSolve = setSolve,
             getInv = getInv)
}


## This function return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        mInverse <- x$getInv()
        data <- x$get()
        mInverse <- solve(data, ...)
        
        if(all(mInverse == data) == TRUE) {
                message("getting cached data")
                return(mInverse)
        }
        
        x$setSolve(mInverse)
        mInverse
}
