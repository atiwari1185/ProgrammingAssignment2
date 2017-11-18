## The following function creates a set of functions as listed below:
# 1 Get Matrix
# 2 Set Matrix
# 3 Set Inverse Matrix
# 4 Get Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inverse) minv <<- inverse
  getinversematrix <- function() minv
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function checks whenever an inversion of matrix is required
## if the inverted matrix is in cache then same is returned
## else the inversion is computed again and the value is returned

cacheSolve <- function(x, ...) {
  minv <- x$getinversematrix()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinversematrix(minv)
  minv
}