## Below are two functions that are used to create a special object that stores 
## a matrix and caches its inversion

## creates a special "matrix", which is really a list containing 
## a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inversion
## get the value of the inversion


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## solve "matrix" object returned by makeCacheMatrix:
## first checks If the inverse has already been calculated 
## (and the matrix has not changed),
## if so - retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i        
}
