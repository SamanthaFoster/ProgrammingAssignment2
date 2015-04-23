## The following two functions (makeCacheMatrix, cacheSolve)
## work together to cache the inverse of a matrix so it does
## not have to be repeatedly calculated. 

## The makeCacheMatrix function creates a special
## "matrix" object that can cache its inverse.
## The output is a list of functions that get and set
## the inverse of the matrix "x". 

makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse of the matrix (i) to NULL the first
  ## time it is encountered
  i <- NULL
  
  ## set the value of the matrix (x)
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## set the value of the inverse (i)
  setinv <- function(inv) i <<- inv
  
  ## return the inverse
  getinv <- function() i
  
  ## make a list of the above functions for use in cacheSolve
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## The cacheSolve function computes the inverse of the special matrix
## returned by makeCacheMatrix.  If the inverse has already been calculated,
## it is retrieved from the cache.  

cacheSolve <- function(x, ...) {
  ## assign the inverse of x to i
  i <- x$getinv()
  
  ## if i is not NULL, it has already been calculated
  ## get i from cached data
  if(!is.null(i)){
    message("getting inverse from cache")
    return(i)
  }
  
  ## if i is NULL, it hasn't been calculated yet
  ## calculate the inverse
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  
  ## return the inverse i
  i
}
