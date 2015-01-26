## Makes a Matrix and stores it in cache to be used
## in case the inverse has already been solved

makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## Check if m is null, if so gets matrix inverse from
## cache, if not then solves the inverse using the make
## cache matrix function

cacheSolve <- function(x, ...) 
  {
    m <- x$getInverse()
    if(!is.null(m)) 
      {
        message("getting cached data")
        return(m)
      }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
        ## Return a matrix that is the inverse of 'x'

