

# Cach the inverse of a matrix rather than to compute it 
# repeatedly for matrix inversion




# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  in_ <- NULL
  set <- function(y) {
    x <<- y
    in_ <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) in_ <<- inverse
  getInverse <- function() in_
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  in_ <- x$getInverse()
  if (!is.null(in_)) {
    message("getting cached data")
    return(in_)
  }
  data <- x$get()
  in_ <- solve(data, ...)
  x$setInverse(in_)
  in_
}
