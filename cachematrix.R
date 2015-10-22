## The pair of functions below will compute and cache the inverse of a matrix 
## to avoid the need to repeatedly compute the inverse when called.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function () x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set,get = get, setsolve = setsolve, getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been computed, this function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
