# create a special square matrix object that can cache its inverse
# assume that the matrix supplied is always invertible
makeCacheMatrix <- function(x = numeric()) {
    # set variable m null first 
    # as we need it to get the value of the inverse later 
    m <- NULL
    
    # set the value of the matrix
    setMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the value of the matrix
    getMatrix <- function() x
    
    # set the value of the inverse 
    setInverse <- function(inverse) m <<- inverse
    
    # get the value of the inverse 
    getInverse <- function() m
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}