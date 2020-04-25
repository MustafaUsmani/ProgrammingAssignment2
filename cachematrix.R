## This program has 2 fuctions named makeCacheMatrix and  cacheSolve 
## which takes and calculate inverse

## This function makes the cache matrix 

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i<<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
  
}


## this function, cacheSolve solves the matrix and return 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
  
  
}