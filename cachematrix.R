## These functions in combination with each other cache the inverse of a matrix

#
# This function caches a matrix. Given the matrix argument, this function will store
# the inverse of the matrix if it wasn't cached beforehand. This function also provides
# several supporting functions to get and set the original matrix and the inverse matrix.
#
makeCacheMatrix <- function(initialMatrix = matrix()) {
  # start with an null inverse matrix - cache it within this function
  matrixInverse <- NULL
  
  # create function to set the matrix, and null the inverse matrix
  setMatrix <- function(matrixValue) {
    initialMatrix <<- matrixValue
    matrixInverse <<- NULL
  }

  # setter function for inverse matrix
  setMatrixInverse <- function(newMatrixInverse) matrixInverse <<- newMatrixInverse
  
  # getters
  getMatrix <- function() initialMatrix
  getMatrixInverse <- function() matrixInverse
  
  # return a list containing the getter and setter functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}

#
# This function will take a [cached] matrix object and get the inverse. Internally this
# function will use a cached inverse (if it has already been stored) or calculate
# a new inverse (and then store it)
#
cacheSolve <- function(cachedMatrixObj, ...) {
  matrixInverse <- cachedMatrixObj$getMatrixInverse()
  
  # if the inverse has already been set, just return it
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  # the inverse wasn't already set, so need to calculate it, store it, and return it
  data <- cachedMatrixObj$getMatrix()
  matrixInverse <- solve(data)
  cachedMatrixObj$setMatrixInverse(matrixInverse)
  matrixInverse
}
