# Getting the inverse of a matrix in R is simple, but it may be computationally
# expensive. If we have a suitable square matrix, one line of R code will
# do the job, but if we may need to compute the same inverse many times,in a loop
# for example, it may be useful to cache the inverse. The two functions makeCacheMatrix()
# and cacheSolve() work together to cache a the matrix and its inverse so that the computagtion
# only needs to be done once.

## Encapsulagte functions to 
#initialize a matrix, 
#set its value (i.e. give it a new value), 
#get its value
#set its inverse
#get its inverse

#makeCacheMatrix <- function(x = matrix()) {
mm <- function(x = matrix()) {
#  makeVector <- function(x = numeric()) {
    i <- NULL
    set <- function(y) {   # set the value of the matrix to y
      x <<- y              # create/UPDATE x in parent context
      i <<- NULL           # create m in parent context and initialize
    }
    get <- function() {x}   #get the matrix
    setInverse <- function(inverse) {i <<- inverse} #set the inverse of matrix x
    getInverse <- function() {i}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
#  }
}


## Write a short comment describing this function

cs <- function(x, ...) {
#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 # cachemean <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    #
    # the required change
    #
    #m <- mean(data, ...)
    i<-solve(data)
    x$setInverse(i)
    i
#  }
}
