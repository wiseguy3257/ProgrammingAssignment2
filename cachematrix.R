## This program contains 2 functions: makeCacheMatrix() and cacheSolve().
## makeCacheMatrix() takes a matrix as its only parameter and creates a list.
## The list stores the original matrix's value and the cached value.
## cacheSolve() takes the list created by makeCacheMatrix() as a parameter and
## outputs a matrix that is the inverse of the original matrix.

## The first function creates a list containing functions to set the value of 
## the matrix, get the value of the matrix, set the value of the inverse matrix,
## and get the value of the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the matrix passed as a 
## parameter in the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m  
}
