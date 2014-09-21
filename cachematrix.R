## Creates vector with four arguments (set$x, get$x, setinv$x, getinv$x) which is then
## used in second function to decide whether or not to cache value of inverse matrix
## set initializes m, get allows to retrieve x if already existing in cache (i.e. not NULL)
## setinv calculates inverse of matrix if not yet existing, and getinv establishes new value
## for matrix in first function for next loop run

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL     ## sets value of inverse matrix to NULL
        }
        get <- function() x    ## gets value of matrix x
        setinv <- function(solve) inv_m <<- solve    ## 
        getinv <- function() inv_m
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

        inv_m <- x$getinv()             ## setting object inv_m to value stored in cache function
        if(!is.null(inv_m)) {
                message("getting cached data")  ## prints message "getting cached data"
        return(inv_m)                   ## returns value of inv_m
        }
        data <- x$get()                 ## if getinv not NULL, value of matrix x called into object "data"
        inv_m <- solve(data, ...)       ## inverse of matrix x calculated and assigned to inv_m
        x$setinv(inv_m)                 ## new value of inv_m assigned to argument "setinv" of function makeCacheMatrix
        inv_m                           ## 
}