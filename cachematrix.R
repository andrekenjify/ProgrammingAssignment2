## Put comments here that give an overall description of what your
## functions do
## The "makeCacheMatrix" function have, as described by the assignment,
##      "creates a special "matrix" object that can cache its inverse"
## The "cacheSolve" function 'computes the inverse of the special "matrix"
##      returned by makeCacheMatrix above. If the inverse has already been 
##      calculated (and the matrix has not changed), then the cachesolve should
##      retrieve the inverse from the cache', as described by the assignment.



## Write a short comment describing this function
## First of all, this function have the variables in portuguese names, due
##   to my costume of write code in my native language.
## About the function, the def function and the definversa are used to set the 
## matrix and it's inverse, respectively. The peg and peginversa have the duty 
## of define the the return matrix object and return it's inverse respectively.


makeCacheMatrix <- function(x = matrix()) {
  matrizinversa <- NULL
  def <- function(a) {
    x <<- a
    matrizinversa <<- NULL
  }
  peg <- function() x
  definversa <- function(matrizinversa){} inversa <<- matrizinversa
  peginversa <- function() inversa
  list(def = def, peg = peg,
       definversa = definversa,
       peginversa = peginversa)
}


## Write a short comment describing this function
## First of all, this function have the variables in portuguese names, due
##   to my costume of write code in my native language.
## About the function, it uses the availabe inverse of the original matrix used
## in makeCacheMatrix, and the it divid in two ways, if the inverse maintains
## the same, skip new codes and it's returned, else calculate the new inverse 
## and set it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrizinversa <- x$peginversa()
  if (!is.null(matrizinversa)) {
    message("previous cached data")
    return(matrizinversa)
  }
  base <- x$peg()
  matrizinversa <- solve(base, ...)
  x$definversa(matrizinversa)
  return(matrizinversa)
}
