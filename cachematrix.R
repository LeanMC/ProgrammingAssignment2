## makeCacheMatrix takes a (hopefully invertible) matrix,
## x, and returns a list of mutator and accessor
## functions. cacheSolve takes a list (created by 
## makeCacheMatrix, hopefully) and returns the
## inverse of a matrix.

## makeCacheMatrix clears the variable that (will) hold(s)
## the cached inverse, defines the mutators and accessors
## and returns a list whose elements are the getters and
## setters

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## cacheSolve takes a list (hopefully created by
## makeCacheMatrix), checks if there is a cached
## inverse and returns it if one is found. If not,
## cacheSolve accesses the matrix object, computes
## its inverse and assigns it to the inverse cache
## variable ('i'), and returns the just-computed
## inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
