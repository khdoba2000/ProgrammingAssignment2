## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## THIS FUNCTION CREATES A SPESIFIC MATRIX that is list of functions
## to set and get the actual values, and also inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(data) inv <<- data
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## THIS FUNCTION GETS THE INVERSE OF THE MATRIX FROM CACHE, IF IT IS CALCULATED BEFORE,
## IF NOT, IT CALCULATES AND SETS IT TO THE MATRIX's ATTRIBUTES

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
  
}
