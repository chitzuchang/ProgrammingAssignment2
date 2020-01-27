## Functions should create a special matrix that can cache its inverse and then retrive the inverse from the cache

## This function will create a unqiue matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # get the value of matrix
  get <- function()x
  # set the value of the inverse 
  set_inverse <- function(inverse) m <<- inverse
  # get the value of the inverse 
  get_inverse <- function() m 
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}

## This function computes the inverse of the unqiue matrix returned by function makeCacheMatrix
## Result is, if inverse is already calculated and the matrix did not change, then this function 
## should retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # get the matrix, solve its inverse 
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  ## Return a matrix that is the inverse of 'x'      
  m
}
