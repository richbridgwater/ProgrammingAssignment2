

## This function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function() {
    x <<- x      # cache matrix value
    m <<- NULL   # clear value
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)   # do inverse function on x and cache it
  getinverse <- function() m
  list(set = set, get = get,                # put functions in a list so the other function can access them
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function below computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m <- x$getinverse()

if(!is.null(m)) {     # if the value is already cached then return it
  message("getting cached data")
  return(m)
}
data <- x$get()       # else calculate it, cache it and return it
m <- solve(data, ...)
x$setinverse()
m
}



## Some paramaters to use for testing:
## a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## cacheSolve(a)
