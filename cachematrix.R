## Two functions to calculate and store matrix inverse

## makeCacheMatrix takes a matrix and creates four functions
## that allow accessing and editing its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inv <<- solve
  getSolve <- function() inv
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## cacheSolve check for cached matrix inverse or calculates one
## if it doesn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getSolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setSolve(inv)
  inv
}
