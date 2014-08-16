## makeCacheMatrix function will create a list of function
## to create cache functionality to store the caculated 
## inverse matrix with its 4 functions
## input variable for the function is a matrix
## set, get, setinverse, getinverse
## set function will store the input matrix
## get function will return the input matrix
## setinverse function will store the inverse of input matrix  
## getinverse function will return the inverse matrix value  

makeCacheMatrix <- function(x = matrix()) {
    
    ## Setting the initial value for inverse matrix 
    inverse <- NULL
    
    ## Setting new matrix in cache
    set <- function(y){
        
        ## Setting new matrix global variable
        x <<- y
        
        ## because the matrix has been changed, 
        ## the inverse variable should be set to NULL
        inverse <<- NULL
    }
    
    ## Getting the main matrix
    ## It returns the global variable
    ## which is stored previousely
    get <- function() x
    
    ## Setting the inverse result in global variable
    setinverse <- function(inv) {
        inverse <<- inv
    }
    
    ## Getting the sotred result (inverse) from global variable 
    getinverse <- function() inverse
    
    ## Create a ist of functioins to be used
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
        
}


## this function will return inverse matrix 
## input variable is a variabe as makeCacheMatrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    ## get the chached inverse result
    m <- x$getinverse()
    
    ## if the cache has value
    ## we should return it
    ## in there is no need to recalculate it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## the result is not calculated before
    ## so, we should calculate it for the first time
    ## get input matrix from cache 
    data <- x$get()
    
    ## calculate the inverse of input matrix
    m <- solve(data, ...)
    
    ## setting the result into cache
    x$setinverse(m)
    
    ## return inverse
    m
    
}




