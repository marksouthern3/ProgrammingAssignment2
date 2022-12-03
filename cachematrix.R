## Meomizing the calculation of matrix inverses.
## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse to save on computation time. cacheSolve returns
## the inverse of one of these special matrices, either by calculating it for
## the first time or by retrieving it from the cache.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() { x }
  
  setinv <- function(inverse) { inv <<- inverse }
  
  getinv <- function() { inv }
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## Retrieves the inverse from the cache if the inverse has already been calculated
## and the matrix has not changed since its calculation.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinv()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
