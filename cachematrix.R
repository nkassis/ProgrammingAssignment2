## Functions to calculate and cache the result of
## calculating the inverse of a matrix

## Create a matrix that can cache it's inverse using the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## Return or calculate the inverse of a matrix
## x is a list returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  i <- x$getinverse()

  #Is the inverse already set?
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

  #Otherwise we calculate the inverse
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
