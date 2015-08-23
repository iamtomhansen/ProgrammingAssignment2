## These functions will save and recall matrices and their inverses. This
## means that the inverse does not need to be computed every time one wishes
## to use it. Additionally, by using a second function to calculate/recall
## the inverse matrix, the inverse is never computed in the first place if it
## is not needed.

## This function takes a matrix and creates a list of functions that cache
## and recall the matrix and its inverse. "get" will return the value of the
## saved matrix, "set" will store the new matrix. Setting a new matrix will
## delete the cached inverse. The set functions use the "<<-" operator to
## change the variables in the workspace (default) environment.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() {x}
  setinv <- function(solved) {xinv <<- solved}
  getinv <- function() {xinv}
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function returns the inverse of a matrix. If the inverse of that
## matrix has not yet been solved, it will solve and save the inverse.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("Inverse is already cached, using cached data.")
    return(xinv)
  }
  newmatrix <- x$get()
  xinv <- solve(newmatrix, ...)
  x$setinv(xinv)
  return(xinv)
}