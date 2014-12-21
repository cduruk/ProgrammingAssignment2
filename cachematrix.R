# Matrix inversion is a costly operation that can
# benefit from caching. We expose a `cacheMatrix` object
# that wraps around a regular matrix but is able to
# cache its own reverse.


# makeCacheMatrix wraps around a regular matrix to return an object
# that can cache its inverse. This object is represeted as a list.
#
# Args:
#   x: A matrix. It must be invertible.
#
# Returns:
#   A list with four items:
#     # get: A function that returns the original matrix.
#     # set: A function that accepts the original matrix as an argument.
#     # getinverse: A function that returns the inverse of the original matrix.
#     # setinverse: A function that accepts the new inverse as an argument.
makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL

  set <- function(mx) {
    x <<- mx
    cached <<- NULL
  }

  get <- function() {
    x
  }

  setinverse <- function(inverse) {
    cached <<- inverse
  }

  getinverse <- function() {
    cached
  }

  list(get = get,
       set = set, 
       getinverse = getinverse,
       setinverse = setinverse)
}

# cacheSolve returns the inverse of the object as created by the
# makeCacheMatrix function.
#
# Args:
#   x: A list as returned by makeCacheMatrix.
#
# Returns:
#   The inverse of the special matrix object. Note that it
#   is a native R matrix object.
cacheSolve <- function(x, ...) {
  newinverse <- x$getinverse()

  if(!is.null(newinverse)) {
    message("Returning data from cache.")
    return(newinverse)
  }

  data <- x$get()
  newinverse <- solve(data)

  x$setinverse(newinverse)
  newinverse
}