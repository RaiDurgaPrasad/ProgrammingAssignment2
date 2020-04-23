## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This assignment illustrates how R works with scoping
## The overall goal of the functions is firstly to define
## a matrix and store the object, to cache the matrix and its inverse. 
## The second function allows to retrieve the inverse of the
## matrix stored in the makeCacheMatrix environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL           ##object inv initialized NULL
  set <- function(y) {        ## Set the value of x and inv.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix   ##SetInverse assing the input solveMatrix to inv
  getInverse <- function() inv
  list(set = set,                                           ##return the elements
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve is necessary to retrieve the inverse 
## from the 'special' cached matrix defined in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()               ## Return a matrix that is the inverse of 'x'
  if (!is.null(inv)) {                      ## if inv is not NULL, the cache inverse can be returned
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()                            ## if inv is NULL, then cacheSolve gets the matrix
  inv <- solve(mat)                         ## from the input x and determines the inverse.
  x$setInverse(inv)
  inv                           ##return the value
}
