## This function demonstrates the lexical scoping 
## functionality within R, the function takes a matrix, 
## computes the inverse of the matrix, and then caches 
## the inverse of the matrix. This is possible because of using the superassignment operation (<<-), we're able to 

## makeCacheMatrix is the parent environment of the 
## function, using the superassignment (<<-) as null 
## in order to allow the inverse matrix to be cached 
## as (m) when solved 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve function calculates the inverse of 
## our matrix object, based on lexical scoping, 
## m becomes null due to makeCacheMatrix, and then 
## solveCache matrix is able to calculate the inverse 
## and store the inverse as cache

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

