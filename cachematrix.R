## This is my solution for the Programming assignment 2.

## Based on the "makeVector" function of the instructions I changed the input to a matrix
## and the "mean" function to a solve function. At the end I would end up with a special matrix
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}

## Once again, I based my answer on the example with the vector. I changed the "mean" function to
## the solve function. At the end, this function computes the inverse of the matrix by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
