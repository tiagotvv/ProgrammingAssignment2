## Create makeCacheMatrix object and cacheSolve functions

makeCacheMatrix <- function(x = matrix()) {
  ### This function creates the CacheMatrix, which is a list with 4 different functions included.
  #
  # Example of usage:
  #
  # x <- matrix(c(1,2,3,4),nrow=2,ncol=2)
  # A <- makeCacheMatrix(x)
  #
  #> A$get()
  #     [,1] [,2]
  #[1,]    1    3
  #[2,]    2    4
  #
  #
  # Setting a new matrix
  # > A$set(matrix(c(3,4,5,6),nrow=2,ncol=2))
  # > A$get()
  #      [,1] [,2]
  #[1,]    3    5
  #[2,]    4    6
  #
  #
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}

cacheSolve <- function(x, ...) {
  ## This function computes (or uses the cached) inverse
  #
  # Example of usage:
  #
  # When calculating for the first time, it actually uses the solve function from R.
  # > x <- matrix(c(1,2,3,4),nrow=2,ncol=2)
  # > A <- makeCacheMatrix(x)
  # > cacheSolve(A)
  #     [,1] [,2]
  #[1,]   -2  1.5
  #[2,]    1 -0.5
  #
  # When done for the second time, the function gets the result from the cache
  # > invA <- cacheSolve(A)
  # getting cached data
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
        ## Return a matrix that is the inverse of 'x'

