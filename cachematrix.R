## Assume that the matrix supplied is always invertible 
## Two functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix so that matrix inversion can become time-consuming

## makeCacheMatrix function creates a special matrix object to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- matrix()
  set <- function(y) {
    x <<- y  	## Set the value of the matrix
    s <<- matrix()  ## Set the empty matrix
  }
  get <- function() x	## Get the value of the matrix
  setsolve <- function(solve) s <<- solve		## Set the value of the solve
  getsolve <- function() s	## Get the value of the solve
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)	## A list caches the inverse of a matrix
}


## cacheSolve function computes the inverse of the special matrix returned by makeCacheMatrix above 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached matrix")
    return(s)   ## Retrieve the inverse from the cache matrix if inverse has been calculated
  }
  data <- x$get()
  s <- solve(data, ...)  ## Compute the inverse of a square matrix
  x$setsolve(s)
  s		## Return a matrix that is the inverse of x
  
}


