## Below are two functions that are used to create a special objet
## that stores a matrix and cache's the inversion of the matrix. 


## The makeCacheMatrix function creates a list
## containing the functions to set and get the matrix and the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                                  ## set i to null, i is the symbol for the inverse matrix
  set <- function(y) {                       ## set the special matrix
    x <<- y
    i <<- NULL
  }
  smatrix <- function() x                    ## get the matrix
  setinversion <- function(solve) i <<- solve  ## set the inverse of the matrix
  matrixinversion <- function() i              ## get the inverse of the matrix
  list(set = set, smatrix = smatrix,           ## output of the function: a list of the four functions
       setinversion = setinversion,
       matrixinversion = matrixinversion)
}


## If the inverse matrix is already calculated the cacheSolve function gets the inverse matrix from the cache.
## If not it calculates and returns the inverse of the special matrix.

cacheSolve <- function(x, ...) {
  i <- x$matrixinversion()                ## get the inverse matrix from the cache and assign it to i
  if(!is.null(i)) {                       ## if there is the inverse matrix in the cache than
    message("getting cached data")        
    return(i)                             ## return the message ("getting cached data") and return as output the inverse matrix
  } 
                                          ## if not than
  data <- x$smatrix()                     ## get the matrix from the first function 
  i <- solve(data)                        ## calculate the inverse of the matrix
  x$setinversion(i)                       ## cache the inverse matrix
  i                                       ## return the inverse matrix as output
}
