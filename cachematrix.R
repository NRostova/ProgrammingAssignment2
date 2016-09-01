## Matrix inversion is usually a costly computation, therefore there might be some
## benefit to caching the inverse of the matrix instead of computing it repeatedly.
## The two functions below are used to cache the inverse of the matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          s <- NULL
          set <- function(y) {
            x <<- y
            s <<- NULL
          }
          get <- function() x
          setsolve <- function(solve) s <<-solve
          getsolve <- function() s
          list(set = set, get = get,
              setsolve = setsolve,
              getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <-x$getsolve()
        if(!is.null(s)) {
        	message("getting cached data")
        	return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
