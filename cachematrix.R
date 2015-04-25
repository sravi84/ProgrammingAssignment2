## This R file contains 2 functions makeCacheMatrix and cacheSolve as part of Programming assignment 2
## These functions help to cache the inverse of a matrix and return whenever required

## makeCacheMatrix will cache the matrix provided as input to it. It has 4 functions which return the martix and its inverse:
## x           -- input matrix
## set         -- function to set the matrix
## get         -- function to retrieve the matrix
## setinverse  -- function to set the inverse of matrix and cache it
## get inverse -- function to retrieve the cached matrix

makeCacheMatrix <- function(x = matrix()){
  xi <- matrix()
  set <- function(y = matrix()){
    x <<- y
    xi <<- matrix()
  }
  get <- function() x
  setinverse <- function(inverse) xi <<- inverse
  getinverse <- function() xi
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve will provide the inverse of the matrix created through makeCacheMatrix.
## If the inverse is already available in cahce, it will return that inverse matrix.
## x -- input matrix
## m -- inverse of the matrix input

cacheSolve <- function(x, ...) {
 m <- x$getinverse()
 if(!is.na(m[1,1])) {
  message("getting cached data")
  return(m)
 }
 data <- x$get()
 m <- solve(data)
 x$setinverse(m)
 m
}

## Below function is a modified version of cacheSolve useful when matrix data has changed after created by makeCacheMatrix
## It takes 2 inputs, object of makeCacheMatrix and the matrix whose inverse needs to be found
## When the matrix has not changed, it performs same as cacheSolve taking only one parameter
## When the matrix has changed, it takes the changed matrix as second parameter.
## This will be useful when not sure whether the matrix has changed or not.

cacheSolveMatrix <- function(x, original = matrix()) {
 data <- x$get()
 m <- x$getinverse()
##when changed matrix is provided
 
if(!is.na(original[1,1])){
## same as the cached matrix
  if(identical(data,original) && !is.na(m[1,1])){
   message("getting cached data")
   return(m)
  }
## not same as the cached matrix
  if(!identical(data,original) && !is.na(m[1,1])){
   message("matrix changed")
   mi <- solve(original)
   x$set(original)
   x$setinverse(mi)
   return(mi)
  }
## when inverse not available
  if(is.na(m[1,1])){
   message("inverse not available")
   mi <- solve(original)
   x$set(original)
   x$setinverse(mi)
   return(mi)
  }
 }
## when second parameter is not provided, same function as cacheSolve
 if(is.na(original[1,1])){
  if(!is.na(m[1,1])){
   message("getting cached data")
   return(m)
   }
  if(is.na(m[1,1])){
   message("inverse not available")
   mi <- solve(data)
   x$setinverse(mi)
   return(mi)
  }
 }
}
