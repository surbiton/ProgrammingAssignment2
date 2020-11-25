## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## input needs to be a square matrix
  ## to include 4 functions: set a matrix, get the matrix, set the invert
  ## invert of a matrix; get the invert of a matrix
  iv <- NULL
  setm <- function(y = matrix()) {
    x <<- y
    iv <<- NULL
  }
  getm <- function() x
  setiv <- function(invert) iv <<- invert
  getiv <- function() iv  
  list(setm = setm, getm = getm,
       setiv = setiv, getiv = getiv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iv <- x$getiv()
  ## use cache if inversion already exists
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  } 
  ## calculate the inversion and store it 
  ## produce a message if input is not a square matrix
  if(nrow(x$getm()) == ncol(x$getm())){
    data <- x$getm()
    iv <- solve(data,...)
    x$setiv(iv)
    iv  
  } else {
    print ("invalid input - requires a square matrix!")
  }
}

