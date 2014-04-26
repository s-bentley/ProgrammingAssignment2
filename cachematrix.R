## Storing and retrieving the inverse of a square matrix to reduce computation
## times


## A list of functions that allow the storage and
## retrieval of a matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function()m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}




## A function that solves for the inverse of a square matrix using a cached 
## inverse if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
