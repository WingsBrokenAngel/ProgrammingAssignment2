## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## set: store the new value of matrix and clear the old inverse value
## get: return the inverse result of matrix
## set_inverse: store the inverse of matrix
## get_inverse: return the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse_val) inverse <<- inverse_val
    get_inverse <- function() inverse
    
    list(set = set, get = get, 
         set_inverse = set_inverse, get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    cache <- x$get()
    if(!is.null(cache)) {
        message("Return cached inverse.")
        cache
    }
    inverse <- solve(x$get(), ...)
    x$set_inverse(inverse)
    inverse
}
