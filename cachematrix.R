## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## We assume first that inverse_ equals NULL and
## x equals the matrix we want to make cache.
## Second, we set a function where x equals y
## and inverse_ equals NULL, using <<- element.
## Then, we set and get inverse through setinverse
## and getinverse and make a list (set,get,setinverse,
## getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inverse_ <- NULL
  set <- function(y) {
    x <<- y
    inverse_ <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_ <<- inverse
  getinverse <- function() inverse_
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## Here, we solve the problem, computing the inverse
## of the desired matrix. First, we get the
## inverse, and place a condition if the inverse
## was already computed, a message 'getting cached
## data' is displayed. Otherwise, the inverse is
## computed and the functions returns the
## inverse matrix of the given data.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_ <- x$getinverse()
  if (!is.null(inverse_)) {
    message("getting cached data")
    return(inverse_)
  }
  data <- x$get()
  inverse_ <- solve(data, ...)
  x$setinverse(inverse_)
  inverse_
}
