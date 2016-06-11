## This program returns the inverse of matrix x. If it is not already 
##calculated, it calculates it. If not, it retrieves it from the cache.

##this function calculates the inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinversee <- function(inverse) inv <<- inverse
  getinversee <- function() inv
  list(set = set, 
       get = get,
       setinversee = setinversee,
       getinversee = getinversee)
}
## This function calculates the inverse of the matrix in makeCacheMatrix.
## If is is already calculated, it returns it from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversee()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversee(inv)
  inv
}

##Thanks
