## makeCacheMatrix creates a special "matrix" object that caches its inverse
## 1. set the value of the "matrix" object
## 2. get the value of the "matrix" object
## 3. set the value of the inverse of the "matrix" object
## 4. get the value of the inverse of the "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## The function cacheSolve computes the mean of the inverse of the "matrix" object above.
## This function first checks to see if the mean of the inverse of the matrix has been calculated.
## If yes, the function gets the mean from the inverse of the matrix and skips the computation.
## If such mean has not yet been calculated, the function calculates the mean of the inverse of the matrix and sets the value of the mean.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
