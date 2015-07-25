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