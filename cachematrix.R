## This script contains functions that allow
## to store and read a matrix, and to calculate
## inverse.
## The script assumes that the matrix is always
## invertible.


## makeCacheMatrix is a function that store a list of functions.
## This functions write (set), and retrieve(get) the given matrix. Also functions setinv and getinv allow us for write and retrieve the inverse of the matrix.

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


## This function check if the inverse of the matrix exist
## in that case it returns its value. If the inverse has not been calculated the function calculate the inverse and retrieve the value.

cachesolv <- function(x, ...) {
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

