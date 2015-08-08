##  This pair of functions are used to create a special object that stores a
##  square invertible matrix, compute its inversion and caches this new 
##  matrix.  
##  The cacheSolve function expects makeCacheMatrix to be assigned to a 
##  variable named "xx".

makeCacheMatrix <- function(x = matrix()) {
      
      ##  This function creates a special "matrix" object that can cache its
      ##  inverse. It defines a list of functions to get and set the value of
      ##  the matrix and to get and set the inverse of the matrix.
      
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)  }


cacheSolve <- function(x, ...) {
      
      ##  This function calculates the inverse of the special "matrix" 
      ##  returned by makeCacheMatrix above. If the inverse has already been
      ##  calculated (and the matrix has not changed), then the cachesolve
      ##  should retrieve the inverse from the cache.
      
      s <- xx$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- xx$get()
      s <- solve(data, ...)
      xx$setsolve(s)
      s
}
