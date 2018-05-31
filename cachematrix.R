library(matlib)

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will create a list containing the needed
## functions to read and write the cache values of inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(mean) m <<- mean
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## this function solves the matrix asking makeCache if the solution is already cached.
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvmatrix(m)
}

#Simple test
matrix <- makeCacheMatrix( matrix(c(1, -1/4,-1/4, 1),  nrow=2,  ncol=2))
cacheSolve(matrix)
matrix$getinvmatrix()
matrix$get()


