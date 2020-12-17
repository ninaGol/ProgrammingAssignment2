## The functions should take a matrix, store the inversed matrix in the cache and retrieve it again

## This function creates a matrix to cache with the inverse of another matrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function produces the inverse of a matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
  }

# example
inMat <- makeCacheMatrix(matrix(sample(1:50,100, replace=T), 10,10))
cacheSolve(inMat)
