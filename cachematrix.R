
###################################################################
## Put comments here that give an overall description of what your
## functions do
## In this example we introduce the `<<-` operator which can be used to
## assign a value to an object in an environment that is different from the
## current environment (lexical scoping). 
## Below are two functions that are used to create a
## special object that stores a matrix and solves it's inverse
##
## I translated the example code for a vector - the lesson here was
##  also how to use Git repositories
###################################################################
## JWhite August 2014

rm(list=ls())
# Write a short comment describing this function
## Create a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse
  m <- NULL
  # set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # get the matrix
  get <- function() x
  # set the inverse 
  setsolve <- function(solve) m <<- solve
  # get the inverse
  getsolve <- function() m
  ##return a list of the methods
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  

}


## Write a short comment describing this function
## computes the inverse of "special" matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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

### testing my function with one simple matrix
mymat<-matrix(c(1.666,0.233,0.233,1.666), nrow=2, ncol=2)
## Solve this the ordinary way
#mymatI<-solve(mymat)
#mymatI
#mymat%*%mymatI

## test the Cached solution
#y<-makeCacheMatrix(mymat)
#cacheSolve(y)
#mymat%*%cacheSolve(y)
