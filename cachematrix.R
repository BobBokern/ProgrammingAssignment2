## The two functions below will compute and cache the inverse of a matrix
## (assuming that the matrix supplied is always invertible)

## The function makeCacheMatrix creates a special matrix object,
## that is able to cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) inverse <<- Inverse
  getInverse <- function() inverse
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The function cacheSolve computes and returns the inverse of a special matrix returned by the 
## function makeCacheMatrix (see above). When the matrix is still the same and the inverse of this matrix has 
## already been calculated, the function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inverseOfX <- x$getInverse()
  if(!is.null(inverseOfX)) {
    message("Getting inverse from the cache...")
    return(inverseOfX)
  }
  data <- x$get()
  inverseOfX <- solve(data, ...)
  x$setInverse(inverseOfX)
  inverseOfX  
}



## Sample run:
x = rbind(c(1, -1/4), c(-1/4, 1))
m = makeCacheMatrix(x)
m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
