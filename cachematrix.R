##This file contains two functions, makeCacheMatrix() and cacheSolve(). The first function takes a matrix 
##as an argument and stores its value and its inverse. The second function checks if the inverse matrix is 
##already stored from the returned data of makeCacheMatrix(), calculates it in case it doesn't exist and 
##returns it

##This function takes a matrix as an argument, defines the set(),get(),setInverse() and getInverse() functions
##and returns a list with those values, making it accessible for the next function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() function takes as an argument the result of the makeCacheMatrix() function and checks if the inverse
##of the matrix is already calculated with an if statement. If it's already available, it returns the inverse and
##prints a message. If it's not yet calculated, it uses the solve() function and stores the results. Finally, it
#returns the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
