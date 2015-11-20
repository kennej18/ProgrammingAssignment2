## Functions will cache the inverse of matrix passed to the function
## input of an matrix and creates a list of functions to cache inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  set <- function(y){
    inv <<- NULL
  }
  
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}

## Calculates inverse of the matrix
## Due to cache, inverse will populate alot quicker

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
