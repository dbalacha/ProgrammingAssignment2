## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  MatrixInverse <- NULL
  set <- function(y) {
    x <<- y
    MatrixInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) MatrixInverse <<- inverse
  getinverse <- function() MatrixInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  MatrixInverse <- x$getinverse()
  if(!is.null(MatrixInverse)) {
    message("getting cached inverse")
    return(MatrixInverse)
  }
  matrix_data <- x$get()
  MatrixInverse <- solve(matrix_data)
  x$setinverse(MatrixInverse)
  MatrixInverse
}
