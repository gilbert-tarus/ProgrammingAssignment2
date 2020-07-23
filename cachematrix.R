## This function creates a special `matrix` object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## create an empty object to cache the inverse
        invMatrix <- NULL
        ## methode to set the array
        set <- function(argy) {
                ## set value of the object
                x <<- argy
                invMatrix <<- NULL
        }
        ## function to get the new created matrix
        get <- function() x
        ## set to inverse matrix
        setMatrix <- function(inverse) invMatrix <<- inverse
        ## method to return the cached inverse m
        getMatrix <- function() invMatrix
        ## utting all methods into a list
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}
##This function computes the inverse of the special `matrix` returned by makeCacheMatrix 
##above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. For 
##example, if X is a square invertible matrix, then solve(X) returns its inverse.
cacheSolve <- function(x, ...) {
        ## return the inverse if it exists already
        invMatrix <- x$getMatrix()
        if(!is.null(invMatrix)) {
                message("getting cached data")
                ## show the existing inverse
                return(invMatrix)
        }
        ## get the matrix from the object passed
        data <- x$get()
        ##find the inverse of the matrix
        invMatrix <- solve(data,...)
        ##cache the inverse
        x$setMatrix(invMatrix)
        ## Return a matrix that is the inverse of 'x'
        invMatrix
}

