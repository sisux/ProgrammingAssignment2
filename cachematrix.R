## rogramming Assignment 2: Lexical Scoping
## 
## 
## GOAL: To write a pair of functions that cache the inverse of a matrix.
## 
## BACKGROUND: Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly
## 
## NOTES: Computing the inverse of a square matrix can be done with the
## solve function in R. For example, if X is a square invertible matrix, 
## then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

#' \code{makeCacheMatrix} creates a special "matrix" object 
#' that can cache its inverse.
#' 
#' @param x The baseline data matrix
#' @return a matrix object that can cache its inverse
#' @examples
#' matrix01 <- rbind(c(1, -1/4), c(-1/4, 1))
#' matrixTest01 <- makeCacheMatrix(matrix01)
#' 
#' matrix02 <- matrix(
#'  data = c(3,1, 2,1),
#'  nrow = 2,
#'  ncol = 2,
#'  byrow = TRUE)
#' matrixTest02 <- makeCacheMatrix(matrix02)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' \code{cacheSolve} computes the inverse of the special 
#' "matrix" returned by makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then cacheSolve should retrieve the inverse from the cache.
#' 
#' @param x "matrix" returned by makeCacheMatrix
#' @return a matrix that is the inverse of 'x'
#' @examples
#' matrix01 <- rbind(c(1, -1/4), c(-1/4, 1))
#' matrixTest01 <- makeCacheMatrix(matrix01)
#' matrix01.inv <- cacheSolve(matrixTest01)
#' matrix01.inv <- cacheSolve(matrixTest01)
#' getting cached data
#' matrix01.inv
#' [,1]      [,2]
#' [1,] 1.0666667 0.2666667
#' [2,] 0.2666667 1.0666667
#' 
#' 
#' matrix02 <- matrix(
#'  data = c(3,1, 2,1),
#'  nrow = 2,
#'  ncol = 2,
#'  byrow = TRUE)
#' matrixTest02 <- makeCacheMatrix(matrix02)
#' matrix02.inv <- cacheSolve(matrixTest02)
#' matrix02.inv <- cacheSolve(matrixTest02)
#' getting cached data
#' matrix02.inv
#' [,1] [,2]
#' [1,]    1   -1
#' [2,]   -2    3
cacheSolve <- function(x, ...) {
  ## Return 
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
