## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a "special matrix" object 
##that can cache its inverse
## It is return a list containing the functions
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                           ## matrix is set
                x <<- y
                m <<- NULL
        }
        get <- function() x        			           ## this function gets back the matrix
        setinv <- function(solve) m <<- solve		   ## this function sets the inverse of the matrix
        getinv <- function() m				           ## this function gets back the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. 
## In the initial part it checks to see if the inverse 
## has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setinv function.

cacheSolve <- function(x, ...) {            		   ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {				               ## Checks is the matrix has already been inverted
                message("getting cached data")		   ## If so, gets back the inverse matrix from the cache
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)                          ## compute the inverse
        x$setinv(m)                                    ## cache the inverse
        m                                              ## return the inverse
}
