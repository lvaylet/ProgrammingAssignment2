## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

## Example:
## > m <- makeCacheMatrix(matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3))
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## > cacheSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]   -2    8   -5
## [2,]    3  -11    7
## [3,]    9  -34   21
## >

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix to NULL, to compute it on the first call to cacheSolve
  i <- NULL

  # Change value of stored matrix
  set <- function(y) {
    x <<- y # Store new matrix
    i <<- NULL # Set inverse to NULL in order to compute it again on the next call to cacheSolve
  }
  # Retrieve value of stored matrix
  get <- function() x
  # Change value of stored inverse
  setinverse <- function(inverse) i <<- inverse
  # Retrieve value of stored inverse
  getinverse <- function() i

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # Get cached data if available (instead of recomputing it)
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # i is NULL if inverse was never computed or if a new matrix was stored, so
  # compute the inverse and save it
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
