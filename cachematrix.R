# Put comments here that give an overall description of what your
## functions do

## Function creates a special "matrix" object
## that can cache its inverse
## assume matrix is always invertible

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # assigns new matrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # returns matrix stored in the main function
  get <- function() x
  # store inverse of matrix
  calculateInverse <- function(solve) m <<- solve
  # returns stored inverse of matrix
  getInverse <- function() m
  # newly created matrix will have all four functions
  list(set = set, get = get,
       calculateInverse = calculateInverse,
       getInverse = getInverse)
}


## Function computes the inverse of the special
## "matrix" returned by makeChacheMatrix
## If inverse has already been calculated,
## function retrieves inverse from cache
## if matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  # return inverse if it has been cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # otherwise retrieve matrix stored with makeCacheMatrix
  data <- x$get()
  # compute inverse of matrix
  m <- solve(data, ...)
  # store inverse in memory
  x$calculateInverse(m)
  # return inverse
  m
  
}
