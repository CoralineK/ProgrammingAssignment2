## Creates a list of functions to store/retrieve value of matrix
## and store/retrieve value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL      ## Stores value of inverse matrix, NULL until set during cacheSolve
  set <- function(y) {
    x <<- y         
    m <<- NULL   ## Overwrites value of m in parent environment
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## First checks for cached inverse matrix stored in m in makeCacheMatrix
## If m is NULL, will calculate the inverse matrix & store in m
## If m already has a stored value, this will be returned

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)    ## If m contains cached inverse, it is returned & function terminates 
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
