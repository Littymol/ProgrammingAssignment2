## Caching the Inverse of a Matrix
## Assumption- All the input matrices are invertible


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix creates a list containing a function to:
## Get & Set value of MATRIX
## Get & Set value of inv MATRIX

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## Computing inverse of square MATRIX is done with the solve
## solve(X) returns inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
