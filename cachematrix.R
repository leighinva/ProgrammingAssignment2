## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this just generates a vector of function calls
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## the set function just takes the input and puts it into x, outside of the scope of the function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## the get function just returns the value of x
  get <- function() x
  
  ## this one is weird, but it just sets whatever the value coming in ...
  setinv <- function(solve1) m <<- solve1
  ## this one is weird too... it gets whatever was written to m
  getinv <- function() m
  ## and all we output is the vector of function calls
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## this will compute the inverse- either using the cached info or actually computing 
## the input to the function must be the output of the makeCacheMatrix (which is a vector of function calls)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  ## m will be null if it has not run for this object
  ## but if it is not null, we can just return the cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## this must be the first time through so I'll get the data
  data <- x$get()
  ## then I'll compute the inverse
  m <- solve(data, ...)
  ## then I'll need to set it in my object
  x$setinv(m)
  m
}
