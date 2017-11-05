## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(rawMatrix = matrix()) {
  invMatrix <- NULL
  #set function for matrix
  set <- function(newMatrix){
    rawMatrix <<- newMatrix
    #set inverse to NULL -> resetting
    invMatrix <<- NULL
  }
  
  #get rawMatrix
  get <- function() rawMatrix
  setinverse <- function(newInverse) invMatrix <<- newInverse
  getinverse <- function() invMatrix
  
  #list functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cacheMatrix <- x$getinverse()

  #checkif it is NULL - if NOT => just return cached version ELSE calculate cached version
  if (is.null(cacheMatrix)){
    newRawMatrix <- x$get()
    
    cacheMatrix <- solve(newRawMatrix, ...)
    
    x$setinverse(cacheMatrix)
  }

  cacheMatrix
  
}
