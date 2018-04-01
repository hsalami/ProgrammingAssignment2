## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse

## This first function, makeCAcheMatrix creates a special matrix, 
## which is a list containing a function to set the value of the matrix,
## get the value of the matrix, set its inverse, and get the inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    }
  get <- function() x
  setinverse <- function(invx) inverse <<- invx
  getinverse <- function() inverse
  list(set = set, get = get,setinverse = setinverse,
       getinverse = getinverse)
}


##The second function solves for the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
  invx <- x$getinverse()
  if(!is.null(invx)) {
    message("getting cached inverse")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinverse(invx)
  invx
}
