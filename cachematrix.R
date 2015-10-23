# makeCacheMatrix function creates a list that contains a function to
# 1. set the value of the matrix "sourceMatrix"
# 2. get the value of the matrix "sourceMatrix"
# 3. set the value of inverse of the matrix "inverseMatrix"
# 4. get the value of inverse of the matrix "inverseMatrix"

makeCacheMatrix <- function(sourceMatrix = matrix()) {
  
  #initializing inverted matrix
  inverseMatrix <- NULL
  
  # make set function
  set <- function(y) {
    sourceMatrix <<- y
    inverseMatrix <<- NULL
  }
  
  # this function gets source matrix
  get <- function() {
    sourceMatrix
  }
  
  # inverting matrix by solve function available in R
  setinverse <- function(solve) {
    inverseMatrix <<- solve
  }
  
  # getting inverted matrix 
  getinverse <- function() {
    inverseMatrix
  }
  
  #creating list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function returns the inverse of the matrix
# this function assumes that matrix is always invertible
cacheSolve <- function(sourceMatrix, ...) {
  
  # before calculating try to get cached version of result
  inverseMatrix <- sourceMatrix$getinverse()
  
  # if inverse matrix already exists, return it with message
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # otherwise get source matrix
  data <- sourceMatrix$get()
  
  # calculate its inverse matrix
  inverseMatrix <- solve(data, ...)
  
  # set inverse matrix
  sourceMatrix$setinverse(inverseMatrix)
  
  # return fresh inverse matrix which will be cached
  inverseMatrix
}
