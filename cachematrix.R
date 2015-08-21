## A pair of functions that compute and cache the inverse of a matrix rather than compute it repeatedly.
## Note that the 'makeCacheMatrix' function doesn't prepopulate the cache with the inverse of the matrix, as
## that happens only on the first 'cacheSolve' call (the inverse of the matrix is computed and cached). 
## After the first call - all subsequent calls to 'cacheSolve' will returned the cached inverse of the matrix.



## This function creates a special "matrix" object that can cache its inverse,
## which is really a list containing a function to 
##    1.set the matrix
##    2.get the matrix
##    3.set/solve the inverse of the matrix
##    4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  ## s will hold the inverse of the matrix x, initialize it to NULL
  s <- NULL
  
  ## Here are the 4 functions included in the list returned (as described above)
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  ## return the list of 4 functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Get the value of s, check if the inverse was already cached (not Null)
  ## If yes - return s (matrix that is the inverse of 'x')
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## Otherwise (in case s is Null), calculate inverse and cache it
  ## and return s (matrix that is the inverse of 'x')
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
