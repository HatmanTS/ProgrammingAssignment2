## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list with Matrix to be cached
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## This function returns the inverse of a matrix. 
## To spend less time in calculation, this function
## cached the inverse so it only has to be calculated once
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting inverse matrix chached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
