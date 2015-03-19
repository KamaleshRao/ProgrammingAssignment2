##Basic Code that caches the results of inverting a matrix

## Creates a matrix that can solve its inverse, by setting up a vector that sets and gets value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## Saves the results of a cache matrix that has not been solved yet.  If it has been solved, it returns the stored answer

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    ##already performed calculation
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

