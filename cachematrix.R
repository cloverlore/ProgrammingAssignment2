# Creates a list object that can cache the inverse of a matrix  
# x is a matrix to be cached, it must be square (equal rows and columns)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inv) m <<- inv
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
## Computes the inverse of the list object returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        
        ## If the inverse has already been calculated retrieve the inverse from the cache.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## calculate the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}