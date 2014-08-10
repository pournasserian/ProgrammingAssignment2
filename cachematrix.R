## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(in_matrix = matrix()) {
  
  cached_out_matric <- NULL
  
  set <- function(in_matrix) {
    cached_in_matrix <<- in_matrix
    cached_out_matric <<- NULL
  }
  
  get <- function() cached_in_matrix
  
  setinverse <- function(out_matrix) cached_out_matric <<- out_matrix
  
  getinverse <- function() cached_out_matric
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinverse(m)
  m
  
}
