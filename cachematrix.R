# Getting the inverse of a matrix in R is simple, but it may be computationally
# expensive. If we have a suitable square matrix, one line of R code will
# do the job, but if we may need to compute the same inverse many times, it may be useful 
# to cache the inverse. 
# The two functions makeCacheMatrix()
# and cacheSolve() work together to cache a the matrix and its inverse so that the computation
# only needs to be done once. See individual functions for implementation details and a 
# transcript of their use at the end of this file.

## makeCacheMatrix
#     Initialize a matrix and an intial value of its inverse. The matix may be empty. 
#     tThe inverse will be null
#     dDefine functions to get/set matrix and get/set its inverse
#     The return value will be a list of data and functions.
#     Taken together these functions and data use R's functional features
#     to create an 'object'
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y     # set/reset the the matix of interest
    i <<- NULL  #if matrix is changed, NULL out the inverse so cacheSolve() will recompute it
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve
#   Get the inverse of the cached matrix 
#   If it is NULL, compute the inverse
#   If it is not NULL, return the cached value
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {   #if the inverse is not NULL, use cached value
    message("getting cached data")
    return(i)
  }
  data <- x$get()   # the inverse was NULL. Get the matrix
  i <- solve(data)  # recompute matrix inverse
  x$setInverse(i)   # cache the inverse
  i                 # return the inverse
}
