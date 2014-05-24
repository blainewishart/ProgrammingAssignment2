# Getting the inverse of a matrix in R is simple, but it may be computationally
# expensive. If we have a suitable square matrix, one line of R code will
# do the job, but if we may need to compute the same inverse many times,in a loop
# for example, it may be useful to cache the inverse. The two functions makeCacheMatrix()
# and cacheSolve() work together to cache a the matrix and its inverse so that the computation
# only needs to be done once. See individual functions for implementation details.

## makeCacheMatrix
#      initialize a matrix and an intial value of its inverse. The matix may be empty. 
#      the inverse will be null

# include functions to get/set matrix and get/set its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y     # set/reset the the matix of interest
    m <<- NULL  #if matrix is changed, NULL out the inverse so cacheSolve() will recompute it
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve
#       get the inverse of the cached matrix 
#    if it is NULL, compute the inverse
#    if it is not NULL, return the cached value
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {   #if the inverse is not NULL, use cached value
    message("getting cached data")
    return(i)
  }
  data <- x$get()  #the inverse was NULL. Get the matrix
  i <- solve(data) # recompute matrix inverse
  x$setInverse(i)  # cache the inverse
  i               # return the inverse
}
