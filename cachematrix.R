## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## set the value of the matrix
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix   
  s <- x$getsolve()
  
  ## check if there is the matrix   
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  ## if not: get the inverse of the matrix   
  data <- x$get()
  s <- solve(data, ...)
  
  ## set the inverse of the matrix 
  x$setsolve(s)
  s
}
