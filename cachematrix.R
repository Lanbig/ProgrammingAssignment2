## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inv) inv_matrix <<- inv
  get_inv <- function() inv_matrix
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inv()
  
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  data <- x$get()
  
  inv_matrix <- solve(data, ...)
  
  x$set_inv(inv_matrix)
  
  inv_matrix     
}
