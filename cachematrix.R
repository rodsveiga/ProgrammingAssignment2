## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. Below
## we write a pair of functions that cach the inverse of a matrix.
## It is assumed that the matrix supplied is always invertible.

## The function 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
    set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  # Here 'solve' is just argument. We use it following the exemple (mean is used)
  setinv <- function(solve) i <<- solve    
  getinv <- function() i
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## The function 'cacheSolve' computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  
  if(!is.null(i)) {  
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}
