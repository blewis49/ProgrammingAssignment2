#  Week 3 - Programming Assignment 2
#  Date:  28 March 2018
#  Author:  William Lewis
# ---------------------------------
## The two functions below work together to calculate and cache the inverse of a matrix. 
## The makeCacheMatrix function initially stores (caches) a matrix in the global environment.
## The cacheSolve function takes the makeCacheMatrix object (a list of helper functions) and 
## checks to see if the inverse of the matrix exists.  If it does not, it calculates the inverse
## of the matrix and returns the inverse matrix.  If it already exists, it gets the inverse matrix
## from cached data in the global environment and returns the matrix.  
# ----------------------------------
## The makeCacheMatrix(x) function creates a makeCacheMatrix object and takes a matrix as input.
## It defines four helper functions, 2 setters and 2 getters that are used with 
## cacheSolve function to set and get the inverse of the input matrix or cache it
## until referenced again.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(arg1) {
            x <<- arg1
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) m <<- inverse
      getInverse <- function() m
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve(x,...) function takes a list containing four functions as input.  These four functions are used to 
## get a matrix and return its inverse that is cached in the global environment or calculate it otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
myMatrix <- makeCacheMatrix(m1)
cacheSolve(myMatrix)
