## makeCacheMatrix function will be able to cache a matrix inversion
## This function creates a special "matrix" object and cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set the value of the Matrix
  set <- function(y){
    x<<- y
    m<<- NULL
  }
  ## get the value of the matrix
  get <- function()x
  ## set the value of the matrix invrese            
  setinverse <- function(inverse) m <<- inverse
  ## get the value of the matrix inverse 
  getinverse <- function() m
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  if (!is.null(m)){
    message("Cached Data")  
    return(m)
  }
  input <- x$get()
  m <- solve(input,...)
  x$setinverse(m)
  m
}
