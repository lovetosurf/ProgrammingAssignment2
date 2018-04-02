## Put comments here that give an overall description of what your
## functions do

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## tell when cacheSolve as run
  inv <-NULL
  ##store the matrix
  set <- function(y) {
    ##put matrix 'x' into cache
    x <<- y
    ##
    inv <<- NULL
  }
  ##return the matrix
  get <- function() x
  setinv <- function(inverse) inv<<-inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## this function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calulated (and the 
##matrix has not changed), then the cachesolve should retrieve the inverse
##from the cache


cacheSolve <- function(x, ...) {
  ## Return matrix that is inverse of x
  inv <- x$getinv()
  if(!is.null(inv)){
    message("retrieving cached inverse matrix")
    return(inv)
  }
  data_matrx <- x$get()
  inv <-solve(data_matrx)
  x$setinv(inv)
  inv
}
