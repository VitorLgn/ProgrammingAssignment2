## Put comments here that give an overall description of what your
## functions do

#This function is TOTALLY based on the functions shown on the assignment instructions.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #the inverse
        set <- function(y) {
                x <<- y #defining the 'x' matrix
                i <<- NULL
        }
        get <- function() x #taking the 'x' matrix
        setinv <- function(inv) i <<- inv #defining the inverse of 'x'
        getinv <- function() i  #geting the inverse of 'x'
        list(set = set, get = get, setinv = setinv,
             getinv = getinv) #way to return the previous objects
}
## Write a short comment describing this function
#This function is TOTALLY based on the functions shown on the assignment instructions.
#sorry for my bad english :) 
cacheSolve <- function(x, ...) {
        i <- x$getinv() # applying on the 'x' the 'getinv' func.
        if(!is.null(i)) { #defining a condition, because if det(x) =/= 0, the inverse doesn't exist.
                message("Getting cached data") 
                return(i)
        } 
        data <- x$get() 
        i <- solve(data, ...) #solve is the function in r that really show the inverse matrix
        x$setinv(i)
        i   ## Return a matrix that is the inverse of 'x'
}
