## 2 functions that will create a special matrix object that stores an invertible
## matrix and caches its inverse

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is a square invertible matrix
  
  ## return is a list of functions that:
    ## 1. sets the value of the matrix 
    ## 2. gets the value of the matrix
    ## 3. sets the value of the inverse
    ## 4. gets the value of teh inverse
  
  inv = NULL # initial inverse value doesn't exist
  ## 1. sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 2. gets the value from 1
  get <- function(x) x
  
  ## 3. sets the value of the inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## 4. gets the value from 3
  getinv <- function() inv
  
  ## return
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## calculates the inverse of the special matrix created in the above
## function, first checking to see if the inverse has already been calculated
## if it has it gets the inverse from cache and skips calculation, 
## otherwise it calculates the inverse and sets it in the cache via setinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get inverse
  inv <- x$getinv()
  
  ## if inverse exists in cache return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## else calculate the inverse, and set it in cache using setinv function
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
