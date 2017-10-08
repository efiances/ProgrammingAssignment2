## The aim of this exercise is to show how a pair of related
## functions perform the cache of a matriy and its inverse in order
## to earn time in recurrent calculation of such inverse.

## The following function (makeCacheMatrix) creates matrix object that caches the calculated inverse matrix of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  } 
  
  setInverse <- function(inverse){
    inv <<- inverse
  } 
  
  getInverse <- function(){
    inv
  } 
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve is designed to create the inverse matrix of the input matrix based on the information cached by the makeCacheMatrix detailed above (in the case that the inverse matrix were calculated before).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message("Retrieving information from cache...")
    return(inv)
  }
  
  Mat <- x$get()
  inv <- solve(Mat,...)
  x$setInverse(inv)
  inv
}
