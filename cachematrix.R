## The functions below compute the inverse of a matrix 
## and cache it so it can be accessed quickly the next
## time it is needed without having to recompute the
## inverse.

## Usage:
##
## source('cachematrix.R')
## M = matrix(c(1,2,3,4),2,2)
## newf <- makeCacheMatrix()
## newf$set(M)
## cacheSolve(newf)
## 

## makeCacheMatrix():
##  Provides functions for getting/setting the matrix,
##  and computing/caching the inverse.
makeCacheMatrix <- function(x = matrix()) 
{
    m   <- NULL
    
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    
    get         <- function() x
    set_inverse <- function(inverse) m <<- inverse
    get_inverse <- function() m
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve()
##  Attempts to retrieve the matrix inverse from
##  cache and return it, if the inverse is not 
##  cached, this function makes a call to 
##  compute/cache the inverse.
## 
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse()
    
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m    <- solve(data)
    x$set_inverse(m)
    m
}
