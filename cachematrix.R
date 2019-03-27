# "makeCacheMatrix" takes as input a square matrix and returns a list of 4 functions
#1.  set: set the value of the matrix inputMatrix passed as parameter
#2.  get: get the value of the matrix inputMatrix passed as parameter
#3.  setsolve: set the value of the inverse of inputMatrix
#4.  getsolve: get the value of the inverse of inputMatrix

makeCacheMatrix <- function(inputMatrix = matrix()) {
  #variable assignment
  inverseMatrix <- NULL
  # <<- used to get to the parent environment
  # inputMatrix is set to have the value of setMatrix
  # inverseMatrix initialised to NULL to avoid former caching
  set <- function(setMatrix) {
    inputMatrix <<- setMatrix
    inverseMatrix <<- NULL
  }
  #anonymous function to get the value of inputMatrix
  get <- function() inputMatrix
  #function to actually solve the inverse of the inputMatrix
  setsolve <- function(solve) inverseMatrix <<- solve
  #anonymous function to get the value of inverseMatrix
  getsolve <- function() inverseMatrix
  #returns the list of functions specified above
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# "cacheSolve" function returns inverseMatrix which is the inverse of a matrix
# cached via "makeCacheMatrix"

cacheSolve <- function(inputMatrix, ...) {
  #  $getsolve extract operator to get inverse matrix of the cached inputMatrix
  # if not yet calculated it gets simply initialised to NULL
  inverseMatrix <- inputMatrix$getsolve()
  if(!is.null(inverseMatrix)) {
    message("getting cached data for the matrix")
    return(inverseMatrix)
  }
  #if it's a new calculation of the inverse
  #  $get extract operator to get values of the cacheds inputMatrix
  squareMatrix <- inputMatrix$get()
  inverseMatrix <- solve (squareMatrix, ...)
  inputMatrix$setsolve(inverseMatrix)
  inverseMatrix
}
