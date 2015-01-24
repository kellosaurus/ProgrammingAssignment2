## A pair of functions to inverse a matrix and cache the result
## Takes advantage of lexical scoping to cache the inverse of a matix, 
## sparing the user costly computation when reuse is required

## makeCacheMatrix creates a "special matrix" for use in cacheSolve
## it creates a list containg four functions:
## 1. 'set' the value of the matrix
## 2. 'get' the value of the matrix
## 3. 'set_inverse' value of the matrix
## 4. 'get_inverse' value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv_m <<- inverse
  get_inverse <- function() inv_m
  list(set = set, get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


## cacheSolve returns the inverse of the matrix.
## It first checks if the inverse of the matrix is already cached
## if it is it returns this value, breaking the loop saving computation
## if it is not it calculates the value and sets it in the cache.
## In both cases it prints a message to tell the user what is happening.

cacheSolve <- function(x, ...) {
  inv_m <- x$get_inverse()
  if(!is.null(inv_m)) {
    message("Getting the cached data")
    return(inv_m)
  }
  message("Inverting and caching data")
  data <- x$get()
  inv_m <- solve(data)
  x$set_inverse(inv_m)
  inv_m
}
