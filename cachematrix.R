## The following functions will cache the inverse of a matrix created by calling function makeCacheMatrix(matrix). To access the cached Solve-result, call cacheSolve(matrix)


## Create a matrix including cache functionality
## Parameter x: matrix (e.g. matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'
## Parameter x: CacheMatrix created using the function above

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
