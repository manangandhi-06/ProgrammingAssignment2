## These functions help in caching the inverse of a given matrix

## This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initializing the matrix_inverse variable
  inv <- NULL
  
  ## Setting up the matrix
  setMatrix <- function(matrix) {
    x <- matrix
    inv <- NULL
  }
  
  ## This function helps in retrieving the matrix
  getMatrix <- function() {
    x
  }
  
  ## Setting up the inverse of the matrix
  setInverse <- function(inverse) {
    inv <- inverse
  }
  
  ## This function helps in retrieving the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  ## Retrieving all the methods as a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Stroring the inverse of x in inv_matrix variable      
  inv_matrix <- x$getInverse()
  
  ## If the inverse already exists, then return the same
  if( is.null(inv_matrix) == FALSE) {
    return(inv_matrix)
  }
  
  ## If inverse not present,
  ## First, Get the matrix
  data <- x$get()
  
  ## Calculate the inverse 
  inv_matrix <- solve(data) %*% data
  
  ## Setting the inverse
  x$setInverse(inv_matrix)
  
  ## Returning the inverse 
  inv_matrix
}
