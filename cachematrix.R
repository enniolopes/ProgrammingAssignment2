## The functions will compute the inverse of a square matrix, if it's already computed
##it will recover the result from the cache

#The function "makeCacheMatrix" creates a matrix object and can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

#The function "CacheSolve" computes the inverse of the matrix returned by 
#makeCacheMatrix, and if its already calculate it recover from cache.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}


##Example of use:
example_matrix <- matrix(1:4,2,2)
a <- makeCacheMatrix(example_matrix)
cacheSolve(a)
cacheSolve(a)