## This pair of functions will cache the inverse of a matrix

## makeCacheMatrix function is to catch the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invm <<- solve
  getinv <- function() invm
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv) 
}

## this cacheSolve function can return the inverse of a matrix,
## either catched when exists or calculated when not exists

cacheSolve <- function(x, ...) {
  invm <- x$getinv()
  if(!is.null(invm)) {
    return(invm)
  }
  m <- x$get()
  invm <- solve(m, ...)
  x$setinv(invm)
  invm
}