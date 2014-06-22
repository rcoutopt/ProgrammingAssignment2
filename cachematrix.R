## This funcion creates a list of  functions to set and get the value of a matrix, and set and get the result of the function solve

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


## this function calculate the result of the function solve of the matrix returned by the function makeCacheMatrix
## how ever before make the calculation check is the calculation have already been made and stored the "special matrix" 
## returned by the function makeCacheMatrix

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
