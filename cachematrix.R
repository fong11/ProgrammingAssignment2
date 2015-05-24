
#makeCacheMatrix is a function that returns a list of functions
#Its purpose is to store a matrix and a cached value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  
  list(
    set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Calculates the inverse of the matrix created with the above function,
## uses cached result if it is available

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)  
  m
}
