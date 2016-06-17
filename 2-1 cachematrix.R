
## This command caches the inverse of a matrix rather than compute it repeatedly
 

## This function creates a special "matrix" object that can cache its inverse.
 
 makeCacheMatrix <- function(x = matrix()) {

## initiate the inv to NULL
        inv <- NULL

## set a new value for the matrix x and invalidate the cached inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

## return the value for underlying matrix x 
        get <- function() {
	x
	}

## set the inverse value of the matrix 
        setinv <- function(inverse) {
	inv <<- inverse
	}

## return the inverse value
        getinv <- function() {
	inv
	}

## return a list, which can be accessed with the $ operation
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
 }
 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
 
 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
## get the inverse of the matrix x
        inv <- x$getinv()

## if we already compute the inverse, this returns it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

## if not, call get() to get the underlying matrix
        data <- x$get()

##calculate the inverse value
        inv <- solve(data, ...)

## set the inverse value in inv so we can cache it and do not need to compute it later
        x$setinv(inv)

## return the caching matrix
        inv
 }
