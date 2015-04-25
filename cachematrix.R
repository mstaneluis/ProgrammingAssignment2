## Coursera rprog-013 (Introduction to R Programming) Assignment 2
## Comments are assignment instructions attributed to R. Peng
## ----------------------------------------------------------------
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

## The first function, makeCacheMatrix creates a special "matrix" containing a function to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverted matrix
## 4) get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function inverts the matrix created/stored in the makeCacheMatrix function.  
## It checks to see if the inverted matrix exists first, and displays it,
## otherwise, it calculates the inversion and stores it in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data.")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

