## This file contains functions to 
## 1) cache the inverse of a matrix, 
## 2) try to get the cached inverse of the unchanged matrix, else recalculate inverse of the matrix

## Function Name: makeCacheMatrix 
## Description: this function is used to cache
## the inverse of matrix calculated in cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize cache variable
  invMatrix <- NULL
  
  ## set function to store the source matrix data in cache
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  ## get function to return the source matrix data from cache
  get <- function() x
  
  ## set function to store inverse of the matrix in cache
  setInverse <- function(inverted) invMatrix <<- inverted
  
  ## get function to return the inverse of the matrix in cache
  getInverse <- function() invMatrix
  
  ## list of functions with names
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function Name: cacheSolve
## Description: This function is used to try to get 
## the cached inverse of the unchanged matrix, 
## else recalculate inverse of the matrix

cacheSolve <- function(x, ...) {
  ## fetch matrix that is the inverse of 'x' from cache
  invMatrix <- x$getInverse()
  
  ## fetch matrix that is source of 'x' from cache
  data <- x$get()
  
  ## If source matrix is unchanged and inverse of that matrix is available in cache, return it
  if(!is.null(data) & !is.null(invMatrix) & data==x){
    message("Getting cached data.")
    return(invMatrix)
  }
  ## else re-calculate the inverse of input matrix, store it in cache and return the inverse value
  makeCacheMatrix$set(x)
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
}
