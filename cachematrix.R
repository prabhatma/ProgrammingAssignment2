## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse<- function() inv
list(set = set, get = get,
setInverse = setInverse,
getInverse = getInverse)
}
####---------------------------------------------------------------------------------------------------------------------------------------------####
## B. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## c. Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
####---------------------------------------------------------------------------------------------------------------------------------------------####
cacheSolve <- function(x, ...) {
inv <- x$getInverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setInverse(inv)
inv
}
