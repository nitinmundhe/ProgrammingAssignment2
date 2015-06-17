#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly 
#I have written a pair of functions that cache the inverse of a matrix.


## This function receives the matrix as input and is provides getter and setter method for that matrix and its inverse

makeCacheMatrix <- function(mat = matrix()) {
  matrixInverse <- NULL
  get <- function() mat
  set <- function(newMat) {
    mat <<- newMat
    matrixInverse <<- NULL
  }
  setMatrixInverse <- function(inverse) matrixInverse <<- solve(inverse)
  getMatrixInverse <- function() matrixInverse  
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## This function receives the object of the makeCacheMatrix as its input and return invrse of matrix from its chache if exist else first cache it and then return the inverse

cacheSolve <- function(mCM, ...) {
  matrixInverse <- mCM$getMatrixInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached Matrix Inverse")
    return(matrixInverse)
  }  
  data <- mCM$get()
  matrixInverse <- solve(data)
  mCM$setMatrixInverse(matrixInverse)
  matrixInverse  
        ## Return a matrix that is the inverse of 'x'
  
}
