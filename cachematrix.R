## Creates vector with four arguments (set$x, get$x, setinv$x, getinv$x) which is then
## used in second function to decide whether or not to cache value of inverse matrix
## set initializes m, get allows to retrieve x if already existing in cache (i.e. not NULL)
## setinv calculates inverse of matrix if not yet existing, and getinv establishes new value
## for matrix in first function for next loop run

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL     ## sets value of matrix to NULL
        }
        get <- function() x    ## 
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)   ## list containing four arguments subsequently used by cacheSolve function
}


## cacheSolve function first checks whether value for matrix pre-existing and not NULL
## if not NULL, uses pre-existing value of matrix and assigns that value to it
## if NULL, it calculates the inverse of the matrix using the solve function
## and sets this new value for subsequent reference in makeCacheMatrix argument x$setinv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}