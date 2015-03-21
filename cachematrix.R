## These two functions represent a mechanism for catching inverse of a matrix.
## If a matrix of interest already has computed inverse and stored in catche, 
## that value is simply accessed from the catche without time consumption.
## If a matrix doesn't have inverse in catche, the inverse will be computed 
## and stored for future usage.


## makeCacheMatrix is a function that creates an "object" capable for catching
## matrix inverse. It is a simple list of 4 functions and own environment in
## which the inverse will be stored in variable m, and matrix will be stored
## in variable x.
## The 4 functions are:> set->for setting the matrix(storing in the environment)
##                     > get->for accessing the matrix (from the environment)
##                     > setinverse -> for storing computed matrix inverse
##                     > getinverse -> for catching the stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse)
}


## cacheSolve is a function for solving or catching the matrix inverse.
## It functions only with an object capable of catching the inverse (see 
## makeCacheMatrix). Details are explained below.

cacheSolve <- function(x, ...) {
        
        ### Function is trying to fetch matrix inverse. If it exists it is
        ### returned imediately.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ### If the inverse is missing the solving begins.
        ### The matrix is fetched and stored in "data" variable. Based on matrix 
        ### from "data" variable the inverse is being computed (solve). Next
        ### step is storing it in catche for potential future usage. The inverse
        ### is finaly being returned.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

