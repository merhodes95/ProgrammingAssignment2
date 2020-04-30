## The first function, makeCacheMatrix creates a special "vector", 
#which is really a list containing a functions to do 1 of 4 things:
##set the value of the vector
##get the value of the vector
##set the the inverted matrix
##get the the inverted matrix
##Put comments here that give an overall description of what your
## functions do

## initialize a variable with this function with the argument being
## a square matrix of any data
## the $getInverse will return null until cacheSolve is run
## returns a list of named functions to be called upon

makeCacheMatrix <- function(matrix) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  #names them to be called later in parent environment
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## the argument for cacheSolve is of type makeCacheMatrix
## Return a matrix that is the inverse of 'x'
## once cacheSolve is run, then $getInverse will return the 
## inverse of 'x' also
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
