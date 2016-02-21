## HW2
## These functions create a special matrix 'object' with
## functions to set and get the matrix data
## as well as a function to cache the inverse if it has not
## already been calculated

## makeCacheMatrix create a special matrix 'object'
## which you can use to get and set functions to get and set
## setinverse and getinverse can be used to set or get the inverse
## example of use
## a = makeMatrix()
## a$set(matrix(1:4,2,2))
## a$get()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(arg) inv <<- arg
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve is meant to be used to cache the 
## inverse of the matrix created using the function above
## or return the inverse if already cached
## example:
## cacheSolve(a)
## a$getinverse()

cacheSolve <- function(x) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
