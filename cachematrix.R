## This R script file contain functions which compute and cache matrix inversion
## If the supplied matrix is not changing since inverse matrix was calculated, then 
## there is no need to recalculate the inverse and return the cached inverse matrix instead.
## A new inverse matrix will be calculate and cache if the supplied matrix is different.

## Function: makeCacheMatrix
## Description: 
##  A data access function which provide getter and setter methods 
##  of a matrix and the inverse of it. The function contains get, set methods
##  to store a supplied matrix and also getinverse and setinverse methods
##  to cache the inverse of the suplied matrix.
## Usage : makeCacheMatrix(x = matrix())
## Arguments: x -> a square numeric or complex matrix
## Return Value : a list of methods contains within the functions

makeCacheMatrix <- function(x = matrix()) {
  
  ## start with the cache of inverse matrix unavailable
  inversematrix <- NULL
  
  ## set(matrix) is a setter method to set new matrix and clear the cached inverse matrix
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  
  ## get() is a getter method to get the supplied matrix
  get <- function() x
  
  ## setInverseMatrix(inversematrix) is a setter method to set and cache 
  ## the inverse matrix for the supplied matrix
  setInverseMatrix <- function(inversematrix) inversematrix <<- inversematrix
  
  ## getInverseMatrix() is a getter method to get the cached inverse matrix
  getInverseMatrix <- function() inversematrix
  
  ## return a list of getter and setter functions within the function
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}

## Function: cacheSolve(cacheMatrix, ...)
## Description: 
##  Returns the inverse of the passed in cacheMatrix object
##  If the inverse has already been calculated and the matrix not changed
##  since the last invocation, returns the cached version.
##  If the inverse has not yet been calculated or the underlying matrix 
##  changed since the last call, (re)computes the value with a call to solve(...)
## Usage : cacheSolve(x, ...)
## Arguments: x -> return list / object from makeCacheMatrix
## Return Value : an inverse matrix

cacheSolve <- function(x, ...) {
  
  ## start with get the current cached inverse matrix for the matrix 'x'
  inversematrix <- x$getInverseMatrix()
  
  ## if a cached inverse matrix is currently avaiable, then return the cached inverse matrix
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  ## if cached inverse matrix is currently not yet calculated,
  ## get the supplied matrix 'x' for further calculation
  data <- x$get()
  
  ## calculate the inverse matrix for the supplied matrix 'x'
  inversematrix <- solve(data, ...)
  
  ## cache the inverse matrix for the supplied matrix 'x'
  x$setInverseMatrix(inversematrix)
  
  ## return the calculated inverse matrix from the supplied matrix
  inversematrix
}