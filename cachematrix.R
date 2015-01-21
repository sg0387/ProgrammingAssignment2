## Matrix inversion is a costly computation when needed to be done repeatedly.
## The following 2 functions can be used to cache the inverse of the matrix.  

## This function creates a special "matrix" object that can cache its inverse. 
## The function input is a matrix and returns a named list containing functions 
## that can get the value of the matrix, ## set the value of the matrix, 
## set the inverse of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## variable to hold inverse
  set <- function(y){ ## reset the matrix using this function. set inverse to null since matrix changed.
      x <<- y
      i <<- NULL
  }
  get <- function() x ## return the matrix
  setinverse <- function(inv) i <<- inv ## set the inverse
  getinverse <- function() i ## return the inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the matrix. It computes the inverse of 
## the special "matrix" returned by makeCachematrix function above. If the inverse 
## has already been calculated then cacheSolve returns inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix in object x
  inv <- x$getinverse() ## retrieve inverse
  if(!is.null(inv)){  ## if the inverse is already computed
    message("getting cached data")
    return(inv)
  }    
  data <- x$get()    ## get the original matrix
  inv <- solve(data) ## compute inverse
  x$setinverse(inv) ## set the value of the inverse in the special matrix object
  return(inv)
}
