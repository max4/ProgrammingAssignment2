## Since matrix inversion is usually a costly computation and the following two functions
## creates the matrix object and caches its inverse.

## 1st function creates a special "matrix" object "x" that can cache its inverse "inver"

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinver <- function(solve) inver <<- solve
        getinver <- function() inver
        list(set = set, get = get,
             setinver = setinver,
             getinver = getinver)
}


## The following 2nd function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache via the setinver function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        
        ## Calcuates a matrix that is the inverse of 'x'
        data <- x$get()
        inver <- solve(data, ...)
        x$setinver(inver)
        inver
}
