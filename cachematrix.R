## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The ChacheMatrix function is creating a list contain 4 functions, 
## which will be used the casheSolve function later
## Those four functions essentially are creating an empty matrix, getting the matrix, 
## storing the inverse (in the m variable within the scope of makeCacheMatrix)
## and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function
## This function will use the functions from makeCacheMatrix 
##to store the inverse matrix

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


