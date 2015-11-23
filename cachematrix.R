makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }  
  get <- function() mat
  setinv <- function(invr) inv <<- invr
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##
cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()  
  inv <- solve(data, ...)
  
  mat$setinv(inv)
  return(inv)
}