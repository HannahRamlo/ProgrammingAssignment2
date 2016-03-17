## Save time by accessing inverse of a matrix 
## through cache rather than computing it each time.


## makeCacheMatrix makes a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setin <- function(inverse) n <<- inverse
  getin <- function() n
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}



## cacheSolve computes inverse of matrix object of 
## the makeCacheMatrix object above. If the 
## inverse has already been calculated, cacheSolve 
## will retrieve it from the cache.

cacheSolve <- function(x, ...) {
  n <- x$getin()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setin(n)
  n
}
