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
        set <- function(y) {                            
                x <<- y
                m <<- NULL
        }
        get <- function() x        			
        setinv <- function(solve) m <<- solve		
        getinv <- function() m				
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

cacheSolve <- function(x, ...) {            		
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
