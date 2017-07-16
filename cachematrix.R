# sets & gets both value of a matrix and its inverse
makeCacheMatrix <- function(matrix) {
  
  inverse <<- NULL
  
  get <- function() matrix
  
  set <- function(y) {
    matrix <<- y
  }
  
  
  getinverse <- function() inverse
  
  setinverse <- function(calculatedinverse) inverse <<- calculatedinverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# returns inverse of the matrix by computation first time & from cache every next time
cacheSolve <- function(matrix) {
  inverse <- matrix$getinverse()
  if(!is.null(inverse)) {
       return(inverse)
  }
  
  inverse <- solve(matrix$get())
  
  matrix$setinverse(inverse)
  
  inverse
}