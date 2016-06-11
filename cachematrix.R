## Function to find the inverse of a matrix
## and cache the result to save later calculation time

## makeCacheMatrix produces a list containing four functions
## cacheSolve returns a matrix that is the inverse of 'x', either from the cache or recalculated

makeCacheMatrix <- function(x = matrix()) {
  ##  produce a list containing four functions:
  ##          1. set the matrix - using matrix as input
  ##          2. get the matrix
  ##          3. set the inverse
  ##          4. get the inverse
  ##     list is used as the input to cacheSolve()
  
  inv = solve(x)
  # set the matrix function
  setMatrix = function(y) {
    x <<- y
    inv <<- solve(x)
  }
  getMatrix <- function() x
  setInv <-function(solve) x <<- solve
  getInv <- function() inv
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- x$getMatrix()
  
  x$setInv(m)
  m
}
