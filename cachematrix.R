## makeCacheMatrix will create a matrix that can cache its inverse, and cachesolve calculate the inverse of the matrix created by makeCacheMatrix using the solve function. If the inverse has already been calculated cachesolve will retrieve the inverse. 


makeCacheMatrix <- function(x = matrix()) {
 	 ## makeCacheMatrix = creates a matrix that can cache its inverse
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- solve(i)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

        
cachesolve <- function(x, ...) {
		## Cachesolve = returns a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}