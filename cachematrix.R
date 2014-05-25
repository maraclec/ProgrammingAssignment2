## The functions makeCacheMatrix and cacheSolve are used in 
## conjunction to create the inverse of a matrix, cache the 
## inverse for later use, and retrieve the inverse for use.

## This function creates a list of functions that can cache or
## retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
     ## Setup
     mtrx <- NULL
     
     ## set function
     set <- function(y) 
     {
          x <<- y
          mtrx <<- NULL
     }
     
     ## get function
     get <- function() x
     
     ##setinverse function
     setinverse <- function(solve) mtrx <<- solve
     
     ##getinverse function
     getinverse <- function() mtrx
     
     ## Return a list of the functions 
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function will check if the inverse of the matrix has already been cached.
## If it has then it will retrieve it otherwise it will calculate it and then 
## cache it for later use.

cacheSolve <- function(x, ...) 
{
     ## Check the cache
     invrs <- x$getinverse()
     
     ## Return whats in the cache if it has already been done
     if(!is.null(invrs)) 
     {
          message("getting cached data")
          return(invrs)
     }
     
     ## If the inverse wasn't pre-calculated then calculate and cache it
     data <- x$get()
     invrs <- solve(data, ...)
     x$setinverse(invrs)
     
     ## Return the inverse matrix
     invrs
}
