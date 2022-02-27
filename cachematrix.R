# Allows users to create an object acting as a cached matrix
# First call to invert the matrix will complete the computation, cache the result,
# and return the result
# Subsequent calls to solve the matrix inversion will return the caches result

# first create your matrix, e.g.  myMatrix <- matrix(1:4, nrow = 2, ncol = 2)
# then pass this into makeCacheMatrix, e.g. myCMatrix <- makeCacheMatrix(myMatrix)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Then solve your matrix, the first time it will be calculated
# Subsequent calls it will use the cache to retrieve the calculated result
# e.g. result <- cacheSolve(myCMatrix)
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