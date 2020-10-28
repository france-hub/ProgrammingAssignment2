#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix. If the inverse 
#has already been calculated (and the matrix has not changed),cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) ## Return a matrix that is the inverse of 'x'
  x$setinv(i)
  i
}