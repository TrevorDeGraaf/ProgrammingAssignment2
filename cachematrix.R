## My first function makes a special matrix of inputed matrix that caches its different forms
## the second function either finds if it already has a cached inverse of a the matrix and prints out message/ inverse if does
## if it doesn't then calculates the inverse, sets caches for inverse and gives you the inverse

## makes a special matrix with caches for inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv<<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## either finds inverse of matrix from cache or calculates it and caches.

cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
}