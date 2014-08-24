## Put comments here that give an overall description of what your
## functions do
## Intent of these functions is to define a customized way
## of caching matrix inversion operation on a matrix
## "makecachematrix" enables user in creating a customized functions 
## which enable query/retrieve operation on the sourcematrix and inverse of the sourcematrix
## "cachesolve" enables the user to retrieve the inverse of the matrix if already computed or
## computes the inverse of the matrix and caches the same on the customized matrix representation

## Write a short comment describing this function
## This function creates a special matrix which caches the matrix inverse
## It creates functions for querying or updating the sourcematrix and the cached inverse of the matrix
makecachematrix <- function(sourcematrix = matrix()) {
  inverse <- NULL
  setmatrix <- function(newmatrix) {
    sourcematrix <<- newmatrix
    inverse <<- NULL
  }
  getmatrix <- function() sourcematrix
  setinverse <- function(inversematrix) inverse <<- inversematrix
  getinverse <- function() inverse
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function queries the inverse matrix from the custommatrix 
## and if not available computes the inverse of the source matrix
## Sets the newly computed inverse of the matrix onto the custom matrix structure
## which sets the inverse variable via lexical scoping using "<<-" operator
cachesolve <- function(custommatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
		computedinversematrix <- custommatrix$getinverse()
  if(!is.null(computedinversematrix)) {
    message("Getting cached matrix")
    return(computedinversematrix)
  }
  data <- custommatrix$getmatrix()
  inverse <- solve(data, ...)
  custommatrix$setinverse(inverse)
  inverse
}
