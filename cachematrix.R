## These functions enable you to create a cached matrix and calculate its
## inverse.

## This function creates a cached matrix from a normal matrix, defining 
## functions for getting and setting values of the cached matrix and
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function calculates the inverse matrix, using cached value if it's
## available.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##Test code
# M <- c(2,2,3,2)
# dim(M) <- c(2,2)
# cM <- makeCacheMatrix(M)
# res <- cacheSolve(cM)
# res2 <- cacheSolve(cM)
# M2 <- c(2,2,3,4)
# dim(M2) <- c(2,2)
# cM$set(M2)
# res3 <- cacheSolve(cM)
