## Put comments here that give an overall description of what your
## functions do
## This pair of functions cache the inverse of a matrix.
## Doing that, we don't need to calculate the value of the inverse
## everytime we need it.
 
## Write a short comment describing this function
## This function creates a list object that can cache its invers
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    setInv <- function(inv) i <<- inv
    getInv <- function() i
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Write a short comment describing this function
## This function computes the inverse of the list returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix set to the list has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
