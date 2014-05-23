# compute the inverse of the square matrix returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    # get the cached value of the inverse
    # to check if the inverse has already been calculated
    m <- x$getInverse()
    
    # if m is not null,then the inverse has already been calculated
    # so get cached inverse
    # and skip the computation
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    
    # if m is null then get the matrix to compute the inverse  
    data <- x$getMatrix()
    
    # compute the inverse
    m <- solve(data, ...)
    
    # cache the inverse 
    x$setInverse(m)
    
    #return the inverse
    m
}