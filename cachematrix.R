## Contains functions related to operations on matrices.
## Functions will create a special "matrix" object that 
## can cache its inverse, and compute or retrieve from cache
## the inverse of a given matrix, respectively.

## Returns a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        inverse_matrix <<- NULL
    }
    get <- function() x
    set_inverse <- function(new_matrix) {
        inverse_matrix <<- new_matrix
    }
    get_inverse <- function() inverse_matrix
    list(set = set, 
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Computes the inverse of the "matrix" object returned by
## the makeCacheMatrix function.  If the inverse has been
## calculated, inverse will be retrieved from cache.

cacheSolve <- function(x, ...) {
    cached_matrix <- x$get_inverse()
    if (!is.null(cached_matrix)) {
        message("getting cached data")
        return(cached_matrix)
    }
    matrix <- x$get()
    inverse_matrix <- solve(matrix)
    x$set_inverse(inverse_matrix)
    inverse_matrix
}
