## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL # stores cached inverse.
  
  set_matrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL  # Resets the cached inverse
  }
  
  get_matrix <- function() x # returns current matrix
  
  cache_inverse <- function(inv) inverse_matrix <<- inv
  
  get_inverse <- function() inverse_matrix # returns cached inverse
  
  list(
    set_matrix = set_matrix,
    get_matrix = get_matrix,
    cache_inverse = cache_inverse,
    get_inverse = get_inverse
  ) 
}
  

# This function returns the cached inverse of the matrix if it's available;
## if it's not available, it creates the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$cache_inverse(inv)
  
  inv  
}
