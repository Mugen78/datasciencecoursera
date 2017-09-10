## Assignment: Caching the Inverse of a Matrix

## The makeCacheMatrix function creates a special “matrix” object that can cache its inverse.
## Within the makeCacheMatrix function a set list of functions to : 
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse
## 4 - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x 
  setinvmatrix <- function(inverse) mtrx <<- inverse
  getinvmatrix <- function() mtrx
  list(set = set, 
       get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

## The cacheSolve function computes the inverse of the special "matrix" created by the makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function
## will retrieve the inverse directly from the cache. 

cacheSolve <- function(x, ...) {
  mtrx <- x$getinvmatrix()
  if (!is.null(mtrx)) {
    message("getting cached data")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinvmatrix(mtrx)
  mtrx
}

