## This function calculates the inverse of a matrix if it does not already exist in the cache.
## The purpose of this assignment is to demonstate the lexical scoping and special '<<-' operator.


## This function assigns the matrix to x, assigns null value to m,
## sets and gets the value of the matrix, 
## and sets and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve()
        getinverse <- function () m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        }


## Checks to see if the calculation has been previously done.  
## If previously done, return previous result.  
## If not, calculate result.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
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
