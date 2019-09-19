
## Builds a set of functions: set(), get(), setinv(), getinv()
# returns these as a list to the parent env
# env also includes cached objects defined within makeCacheMatix()
# x and m from setinv(inv)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  #
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x  # x is retrieved from makeCacheMatrix
  setinv <- function(inv) m <<- inv  # m is set to parent env
  getinv <- function() m
  # getters and settters: mutator and accessor methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# checks if there is already a non-NULL value in m
# if so returns cached version
# else if m is NULL, calculates inverse of square matrix
# and sets it to m

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # call get() function above
  m <- solve(data, ...)
  x$setinv(m)  # call setinv() function above
  m
}

# # Uncomment to test functions above
# aMatrix <- makeCacheMatrix(matrix(c(-1, 2.5, 2, -2), 2, 2)) ##invertible matrix
# aMatrix$get()
# aMatrix$getinv()  # should return NULL: m not yet assigned by cacheSolve
# # test caching behavior by setting new invertible matrix
# aMatrix$set(matrix((c(-1, 1.5, 1, -1)), 2, 2))
# cacheSolve(aMatrix)
# aMatrix$getinv()
