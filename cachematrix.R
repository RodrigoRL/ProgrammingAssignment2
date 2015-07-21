## Use makeCacheMAtrix to create a CacheMatrix object that can store a matrix 'x' and its
## inverse 'invx' once it is computed for the first time
## Code is based on the example provided: makeVector and cachemean

## makeCacheMatrix creates an object which contrains a matrix 'x' and 4 functions to
## get/set matrix 'x' and its inverse 'invx'.

makeCacheMatrix <- function(x = matrix()) {
      
      invx <- NULL
      set <- function(y) {     # updates 'x' with input 'y'
            x <<- y
            invx <<- NULL
      }
      get <- function() x     # returns 'x'
      setinv <- function(m) invx <<- m     # sets the value of 'invx', the inverse of 'x'
      getinv <- function() invx     # returns 'invx'
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}

## cacheSolve computes the inverse of an object CacheMatrix or retrieves the inverse if it
## has been previously computed

cacheSolve <- function(x, ...) {
      
      m <- x$getinv()     # retrieve 'invx' from object CacheMatrix
      if(!is.null(m)) {     # check if inverse is stored in object CacheMatrix
            message("getting cached data")
            return(m)
      }
      
      mtrx <- x$get()     # retrieve 'x' from object CacheMatrix
      m <- solve(mtrx)     # find inverse of 'x'
      x$setinv(m)     # update 'invx' in object CacheMatrix
      m
}
