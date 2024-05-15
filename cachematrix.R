## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize cached inverse to NULL
  s <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL  # Reset cached inverse when matrix is set
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setsolve <- function(solve) {
    s <<- solve
  }
  
  # Function to get the cached inverse
  getsolve <- function() s
  
  # Return a list of functions
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}

## Compute the inverse of the special "matrix" object returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Retrieve cached inverse if available
  s <- x$getsolve()
  
  # If inverse is cached, return it
  if (!is.null(s)) {
    message("Getting cached inverse")
    return(s)
  }
  
  # If inverse is not cached, compute it and cache it
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setsolve(s)
  s
}
