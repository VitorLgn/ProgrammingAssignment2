
#This function is TOTALLY based on the functions shown on the assignment instructions.
#This function basically cache the matrix in another object

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL #the inverse
        set <- function(y) {
                x <<- y #defining the 'x' matrix
                i <<- NULL
        }
        get <- function() x 
        setinv <- function(inv) i <<- inv 
        getinv <- function() i  
        list(set = set, get = get, setinv = setinv,
             getinv = getinv) #way to return the previous objects
}

#This function take the previously matrix cached on that object of the first function
#and transform that matrix on the inverse with the function solve()
cacheSolve <- function(x, ...) {
        i <- x$getinv() # applying on the 'x' the 'getinv' func.
        if(!is.null(i)) { #defining a condition, because if det(x) =/= 0, the inverse doesn't exist.
                message("Getting cached data") 
                return(i)
        } 
        data <- x$get() 
        i <- solve(data, ...) #solve is the function in r that really show the inverse matrix
        x$setinv(i)
        return(i)   ## Return a matrix that is the inverse of 'x'
}
