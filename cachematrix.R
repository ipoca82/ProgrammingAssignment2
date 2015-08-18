# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than compute it
# repeatedly.
# These two functions are useful to cache the inverse of a matrix.


## makeCacheMatrix function: This function creates a special "matrix"
## object that can cache its inverse.
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix (see the function description above).
## If the inverse has already been calculated (and the matrix has not
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getsolve()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}
