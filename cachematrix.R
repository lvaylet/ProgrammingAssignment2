## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly (there are also alternatives to matrix inversion that we will not
## discuss here). Your assignment is to write a pair of functions that cache the
## inverse of a matrix.

## > m <- makeCacheMatrix(matrix(rnorm(9),3,3))
## > cacheSolve(m)
## [,1]        [,2]      [,3]
## [1,]  0.8840004  0.78518642 -1.123824
## [2,]  0.5097846  0.01398283 -1.709970
## [3,] -0.3071629 -0.83397520  1.578815
## > cacheSolve(m)
## getting cached data
## [,1]        [,2]      [,3]
## [1,]  0.8840004  0.78518642 -1.123824
## [2,]  0.5097846  0.01398283 -1.709970
## [3,] -0.3071629 -0.83397520  1.578815
## >

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse matrix to NULL, to compute it on the first call to cacheSolve
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL # Set i to NULL in order to compute it again on the next call to cacheSolve
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
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
