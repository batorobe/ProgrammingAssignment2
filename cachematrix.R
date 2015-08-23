## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix, creates a matrix that is really a list containg
## a function to...
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set<-function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

## The following function calculates the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the inverse and skips the
## computation. Otherwise it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return (m)
    
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}
