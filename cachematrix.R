## Matrix inversion is usually a costly computation 
## Following functions cache the inverse of a matrix rather
## than compute it repeatedly to save that cost.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         ## Creating a matrix and functions associated with it
         m <- NULL                      ## Defining a cache called "m"
         set <- function(y) {
                x <<- y                 ## assigning the input "y" to variable "x"
                m <<- NULL              ## reinitializing "m" to NULL
         }
         get <- function() x            ## returning the matrix "x"
         setinverse <- function(inverse) m <<- inverse
         getinverse <- function() m
         list(set = set,get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## It checks first if the inverse has already been calculated and that the matrix has not changed, 
## if so it should retrieve the inverse from the cache, otherwise cacheSolve has to calculate it.

cacheSolve <- function(x,...) {
         ## Return a matrix that is the inverse of 'x'
         m <- x$getinverse()
         if(!is.null(m)) {
                message("getting cached data")
                return(m) ## Return inverse matrix "x" from cache
         }
         data <- x$get()
         m <- solve(data,...) ## Calculates inverse matrix of "x"
         x$setinverse(m)
         m
 }

