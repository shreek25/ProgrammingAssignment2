## The function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {
  m  <- NULL
  set  <- function(y){
    x <<- y
    m <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) m  <<- inverse
  getinverse  <- function() m
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}

## The function calculates the inverse of the matrix created in the above function. 
## If the inverse has already been calculated (and the matrix has not changed),
## Get the inverse from the cache and skips computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
  m  <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data  <- x$get()
  m  <- solve(data, ...)
  x$setinverse(i)
  m
}
