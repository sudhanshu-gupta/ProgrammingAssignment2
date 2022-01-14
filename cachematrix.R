## The program calculate the inverse of the given matrix and cache the inverse in-memory. 
## If for a given matrix, inverse is stored, it is return else it will be calculated, cached and returned.

## The function cache the inverse of the given matrix. The cache provide setInverse and getInverse function to set and get the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function return the inverse of the matrix. If inverse is found in the cache, 
## it is returned, else the inverse is calculated, cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return (inv)
  }
  
  y <- x$get()
  inv <- solve(y, ...)
  x$setInverse(inv)
  inv
}
