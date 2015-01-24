## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix requires matrix for its input

makeCacheMatrix <- function(x = matrix()) {
  
  # initial inv_matrix and set to NULL
  inv_matrix <- NULL
  
  # Function for setting value for the matrix
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  # Function for getting value of the matrix
  get <- function() x
  
  # These will set and get of the inverted matrix
  set_inv <- function(inv) inv_matrix <<- inv
  get_inv <- function() inv_matrix
  
  #return the list from the functions (set,get,set_inv,get_inv)
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  #get the inverted matrix from cache
  inv_matrix <- x$get_inv()
  
  #if the inverted matrix is found, return it.
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  #if the inverted matrix is not found, get the matrix
  data <- x$get()
  
  #solve function will invert the matrix and store the value in inv_matrix
  inv_matrix <- solve(data, ...)
  
  #create the cache for the inverted matrix
  x$set_inv(inv_matrix)
  
  #retrun the inverted matrix
  inv_matrix     
}

## Example how to use the functions
## mat <- matrix(c(4,3,3,2), nrow=2, ncol=2)
## make_cm <- makeCacheMatrix(mymat)
## cacheSolve(make_cm)
