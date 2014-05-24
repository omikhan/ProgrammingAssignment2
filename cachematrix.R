## Put comments here that give an overall description of what your
## functions do
## This file has two functions, vis. makeCacheMatrix() and
## cacheSolve(). makeCacheMatrix() funciton stores matrix and
## its inverse in cache and cacheSolve() calculates inverse if its
## already not calculated. If its is already present in cache, then
## it doesn't recalculate it, instead return the one from cache..



## This function is composed of the following functions
## 1) This function initiates a Null inverse,
##    initiates the matrix by 'set()' function. 
##    This function stores this matrix in cache.
## 2) Then it has a 'get()' function to return this matrix.
## 3) It has 'set.inverse()' function to set the inverse matrix 
##    It stores inverse in cache
## 4) It has 'get.inverse()' function that returns the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##initiate by setting the inverse as Null
  i <- NULL
  ##initiate the function and set original matrix and set inverse = Null 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ##returns the matrix..
  get <- function() x
  
  ##sets inverse of the matrix
  set.inverse <- function(inverse) i <<- inverse
  ##returns inverse of the matrix
  get.inverse <- function() i  
  
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get the inverse matrix from get.inverse function of matrix object
  i <- x$get.inverse()
  ## if inverse is already calculated then return the one stored in cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## else
  ## get the matrix data from matrix object using get() method,
  ## calculate the inverse here using solve()
  ## and set it in the matrix object
  ## using set.inverse() function
  
  data <- x$get()
  i <- solve(data)
  x$set.inverse(i)
  ##return inverse..
  i
}
