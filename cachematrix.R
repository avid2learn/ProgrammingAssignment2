## makeCacheMatrix creates a special matrix object 'm that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # set the value of the matrix m
  m <- NULL
  set <- function(y) {
    x <<- y
  m <<- NULL
  }
  # get the value of the matrix m
  get <- function() x
  
  #set the matrix inverse 
  setinverse <- function(inverse) m <<- inverse
  
  #get the matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Assumption: the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
  ## Return a matrix 'inv_m' that is the inverse of 'x'
  inv_m <- x$getinverse()
  
  if (!is.null(inv_m)) {
    message("getting cached inverse matrix ...")
    return(inv_m)
  } 
   cache_m <- x$get()
   inv_m <- solve(cache_m,...)
   x$setinverse(inv_m)
   inv_m
}

