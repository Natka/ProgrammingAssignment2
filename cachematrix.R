## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL # Clear the cached inverse when the matrix is updated
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInv <- function(inverse) m <<- inverse
  
  # Function to get the cached inverse
  getInv <- function() m
  
  # Return a list of the functions
  list(set = set, 
       get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse
  inv <- x$getInv()
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...) # Compute the inverse using solve()
  x$setInv(inv) # Cache the computed inverse
  inv

}
