## This module, "cachematrix.R" contains a pair of functions that calculate the 
## inverse of a matrix and store the result in cache.  If the inverse matrix is 
## needed again, it can be looked up in the cache rather than recomputed.

## This function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to
##  1.  set the value of the original matrix
##  2.  get the value of the original matrix
##  3.  set the value of the inverse matrix
##  4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrix <- function(invMatrix) m <<- invMatrix
      getmatrix <- function() m
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}

## This function, "cacheSolve" calculates the inverse of the special "matrix"
## created with "makeCacheMatrix" function. However, it first checks to see if 
## the inverse matrix has already been calculated. If so, it `get`s the inverse 
## matrix from the cache and skips the computation. Otherwise, it calculates the
## inverse of the matrix and sets the value of the inverse matrix in the cache 
## via the `setmatrix` function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
