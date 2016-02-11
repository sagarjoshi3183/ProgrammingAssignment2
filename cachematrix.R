## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(y = matrix()) {
  inverse_y <- NULL
  set <- function(z) {
    y <<- z
    inverse_y <<- NULL
  }
  get <- function(y) 
  setinverse<- function(inverse) inverse_y <<-inverse
  getinverse <- function(inverse_y)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.
cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_a <- a$getinverse()
  if (!is.null(inverse_a)) {
    message("getting cached inverse matrix")
    return(inverse_a)
  } else {
    inverse_a <- solve(a$get())
    a$setinverse(inverse_a)
    return(inverse_a)
  }
}