## The pair of functions below can store an invertible matrix and cache 
## its corresponding inverse for future usage

## This function outputs a list of functions and also stores the matrix of interest

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if the inverse of the matrix has been previously cached
## If not, it will compute the inverse and cache it for future usage

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        thematrix <- x$get()
        m <- solve(thematrix, ...)
        x$setinverse(m)
        m
}
